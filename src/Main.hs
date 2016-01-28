{-# LANGUAGE OverloadedStrings, LambdaCase, GADTs, StandaloneDeriving, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, InstanceSigs, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables, RecordWildCards #-}

module Main where

import Debug.Trace
import Network.Wai
import Network.HTTP.Types (status200)
import Network.Wai.Handler.Warp (run)
import qualified Data.Aeson as JSON
import Data.Int (Int32)
import Data.Text.Encoding (decodeUtf8)
import Data.Attoparsec.Text (parseOnly, endOfInput)
import qualified Data.Text as Text
import Data.ByteString.Lazy (toStrict)
import qualified Data.GraphQL.AST as AST
import Data.GraphQL.Parser (document)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Haxl.Prelude
import Haxl.Core
import Data.Traversable (for)
import Control.Monad (when)

import GraphQL
import DropboxDataSource
import StarWarsModel
import StarWarsDataSource

type Name = Text

-- GraphQL schema
data SchemaType
     = TString
     | TInt
     | TFloat
     | TBoolean
     | TID
     | TObject (HashMap Name SchemaType)
     | TEnum {- ... -}
     | TInterface {- ... -}
     | TUnion {- ... -}
     | TList SchemaType
     | TNonNull SchemaType

-- Is this part of the AST or the runtime representation of a value?
data Value
     = VNull
     | VInt Int32
     | VFloat Double
     | VBoolean Bool
     | VString Text
     | VEnum Text -- unquoted on parse
     | VList [Main.Value]
     | VObject (HashMap Text Main.Value)
     | VVar Name -- looked up in environment

instance JSON.ToJSON Main.Value where
  toJSON VNull = JSON.Null
  toJSON (VInt i) = JSON.toJSON i
  toJSON (VFloat f) = JSON.toJSON f
  toJSON (VBoolean b) = JSON.toJSON b
  toJSON (VString s) = JSON.toJSON s
  toJSON (VEnum n) = JSON.toJSON n
  toJSON (VList ls) = JSON.toJSON ls
  toJSON (VObject o) = JSON.toJSON o
  toJSON (VVar _) = error "Unresolved variable in JSON encoding"

data QueryParameter = QueryParameter Name SchemaType
data Query = Query Name [QueryParameter] SchemaType
data Mutation = Mutation Name [QueryParameter] SchemaType
data ServerSchema = ServerSchema [Query] [Mutation]

{-
enum DogCommand { SIT, DOWN, HEEL }

type Dog implements Pet {
  name: String!
  nickname: String
  barkVolume: Int
  doesKnowCommand(dogCommand: DogCommand!): Boolean!
  isHousetrained(atOtherHomes: Boolean): Boolean!
  owner: Human
}

interface Sentient {
  name: String!
}

interface Pet {
  name: String!
}

type Alien implements Sentient {
  name: String!
  homePlanet: String
}

type Human implements Sentient {
  name: String!
}

enum CatCommand { JUMP }

type Cat implements Pet {
  name: String!
  nickname: String
  doesKnowCommand(catCommand: CatCommand!): Boolean!
  meowVolume: Int
}

union CatOrDog = Cat | Dog
union DogOrHuman = Dog | Human
union HumanOrAlien = Human | Alien

type QueryRoot {
  dog: Dog
}
-}

type GraphQLHandler a = GenHaxl () a

data ResolvedValue
  = RNull
  | RScalar Scalar
  | RList [ValueResolver]
  | RObject (HashMap Text ValueResolver)
type ValueResolver = ResolverArguments -> GraphQLHandler ResolvedValue

data FullyResolvedValue
  = FNull
  | FScalar Scalar
  | FList [FullyResolvedValue]
  | FObject (HashMap Text FullyResolvedValue)

instance JSON.ToJSON FullyResolvedValue where
  toJSON FNull = JSON.Null
  toJSON (FScalar s) = JSON.toJSON s
  toJSON (FList l) = JSON.toJSON l
  toJSON (FObject o) = JSON.toJSON o

-- merge with ResolvedValue / RObject?
type ObjectResolver = HashMap Text ValueResolver
data Server = Server
  { rootQuery :: ObjectResolver
  }

decodeInputValue :: AST.Value -> InputValue
decodeInputValue = \case
  AST.ValueVariable _ -> error "TODO: variable lookup in environment"
  AST.ValueInt i -> IScalar $ SInt i
  AST.ValueFloat f -> IScalar $ SFloat f
  AST.ValueBoolean f -> IScalar $ SBoolean f
  AST.ValueString (AST.StringValue s) -> IScalar $ SString s
  AST.ValueEnum s -> IScalar $ SEnum s
  AST.ValueList (AST.ListValue ls) -> IList $ fmap decodeInputValue ls
  AST.ValueObject (AST.ObjectValue fields) -> IObject $
    HashMap.fromList [(name, decodeInputValue value) | AST.ObjectField name value <- fields]

decodeArgument :: AST.Argument -> (Text, InputValue)
decodeArgument (AST.Argument name value) = (name, decodeInputValue value)

processSelectionSet :: ObjectResolver -> AST.SelectionSet -> GraphQLHandler (HashMap Text FullyResolvedValue)
processSelectionSet objectResolver selectionSet = do
  fmap HashMap.fromList $ forM selectionSet $ \case
    AST.SelectionField (AST.Field alias name arguments _directives innerSelectionSet) -> do
      traceShowM $ name
      valueResolver <- case HashMap.lookup name objectResolver of
        Just vr -> return vr
        Nothing -> fail $ "Requested unknown field: " ++ Text.unpack name

      let args = HashMap.fromList $ fmap decodeArgument arguments
      outputValue <- valueResolver args >>= \case
        RNull -> return FNull
        RScalar s -> return $ FScalar s
        RList ls -> do
          fail "TODO: lists don't work"
        RObject o -> do
          if null innerSelectionSet then do
            fail "Must select fields out of object"
          else do
            FObject <$> processSelectionSet o innerSelectionSet
      return (if Text.null alias then name else alias, outputValue)
    _ -> fail "unsupported selection"

handleRequest :: Server -> StateStore -> (Response -> IO b) -> AST.Document -> IO b
handleRequest server stateStore respond doc = do
  -- TODO: bad bad bad
  let (AST.Document defns) = doc
  let queries = [node | AST.DefinitionOperation (AST.Query node) <- defns]
  --putStrLn $ show queries

  requestEnv <- initEnv stateStore ()
  outputs <- runHaxl requestEnv $ do
    for queries $ \(AST.Node name [] [] selectionSet) -> do
      output <- processSelectionSet (rootQuery server) selectionSet
      return (name, output)

  let response = HashMap.fromList [("data" :: Text, HashMap.fromList outputs )]
  respond $ responseLBS
    status200
    [("Content-Type", "application/json")]
    (JSON.encode response)

class KnownValue v where
  encodeKnownValue :: v -> ResolvedValue

instance KnownValue Text where
  encodeKnownValue = RScalar . SString

instance KnownValue Int where
  encodeKnownValue = RScalar . SInt . fromInteger . toInteger

knownValue :: KnownValue v => v -> ValueResolver
knownValue v _args = return $ encodeKnownValue v

responseValueFromUser :: User -> ObjectResolver
responseValueFromUser (User name) = HashMap.fromList
  [ ("name", knownValue name)
  ]

meHandler :: ValueResolver
meHandler _ = do
  let myUserID = UserID "ME"
  user <- dataFetch (FetchUser myUserID)
  return $ RObject $ responseValueFromUser user

friendHandler :: ValueResolver
friendHandler args = do
  let (Just (IScalar (SString userID))) = HashMap.lookup "id" args
  user <- dataFetch (FetchUser (UserID userID))
  return $ RObject $ responseValueFromUser user

responseValueFromCharacter :: Character -> ResolvedValue
responseValueFromCharacter Character{..} = RObject $ HashMap.fromList
  [ ("name", knownValue cName)
  ]

characterHandler :: CharacterID -> ValueResolver
characterHandler characterID _args = do
  character <- dataFetch $ FetchCharacter characterID
  return $ responseValueFromCharacter character

responseValueFromEpisode :: Episode -> ResolvedValue
responseValueFromEpisode Episode{..} = RObject $ HashMap.fromList
  [ ("name", knownValue eName)
  , ("releaseYear", knownValue eReleaseYear)
  , ("hero", characterHandler eHero)
  ]

{-
type Node = HashMap Text (KeyResolver

-- TODO: constraint
type ObjectResolver a = GraphQLObjectID a -> GraphQLHandler Node

data ResolvedValue = ResolvedValue

-- TYPED
-- TODO: constraint
type KeyResolver = ResolverArguments -> GraphQLHandler Node
-}

heroHandler :: ValueResolver
heroHandler args = do
  episodeID <- lookupArgument args "episode" >>= \case
    Just x -> return x
    Nothing -> return NewHope
  episode <- dataFetch $ FetchEpisode episodeID
  character <- dataFetch $ FetchCharacter $ eHero episode
  return $ responseValueFromCharacter character

episodeHandler :: ValueResolver
episodeHandler args = do
  episodeID <- requireArgument args "id"
  episode <- dataFetch $ FetchEpisode episodeID
  return $ responseValueFromEpisode episode

app :: StateStore -> Application
app stateStore request respond = do
  -- TODO: check the request URL
  -- TODO: check the request method (require POST)

  _body <- fmap (decodeUtf8 . toStrict) $ strictRequestBody request
  -- let body' = "query our_names { me { name }, friend(id: \"10\") { name } }"
  -- let body' = "query HeroNameQuery { newhope_hero: hero(episode: NEWHOPE) { name } empire_hero: hero(episode: EMPIRE) { name } jedi_hero: hero(episode: JEDI) { name } } query EpisodeQuery { episode(id: NEWHOPE) { name releaseYear } }"
  let body' = "query newhope_hero_friends { episode(id: NEWHOPE) { hero { name } } }"

  queryDoc <- case parseOnly (document <* endOfInput) body' of
    Left err -> do
      fail $ "Error parsing query: " ++ err
    Right d -> do
      return d

  let rootQuery = HashMap.fromList
                  [ ("me", meHandler)
                  , ("friend", friendHandler)
                  , ("hero", heroHandler)
                  , ("episode", episodeHandler)
                  ]
  let server = Server rootQuery
  handleRequest server stateStore respond queryDoc

main :: IO ()
main = do
  putStrLn $ "http://localhost:8080/"

  conn <- openConnection
  let stateStore = stateSet conn {-$ stateSet UserRequestState-} stateEmpty
  run 8080 $ app stateStore
