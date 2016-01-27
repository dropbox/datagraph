{-# LANGUAGE OverloadedStrings, LambdaCase, GADTs, StandaloneDeriving, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, InstanceSigs, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables, RecordWildCards #-}

module Main where

import Data.Proxy
--import Debug.Trace
import Network.Wai
import Network.HTTP.Types (status200)
import Network.Wai.Handler.Warp (run)
import qualified Data.Aeson as JSON
import Data.Int (Int32)
import Data.Text.Encoding (decodeUtf8)
import Data.Attoparsec.Text (parseOnly, endOfInput)
import Data.Hashable
import qualified Data.Text as Text
import Data.ByteString.Lazy (toStrict)
import qualified Data.GraphQL.AST as AST
import Data.GraphQL.Parser (document)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Haxl.Prelude
import Haxl.Core
import Text.Printf
import Data.Typeable
import Data.Traversable (for)

import GraphQL
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

{-
data ResponseValue
     = RVNull
     | VList [Main.Value]
     | VObject (HashMap Text Main.Value)
     | VVar Name -- looked up in environment
-}


data ResponseValue
     = RNull
     | RScalar Scalar
     | RList [ResponseValue]
     | RObject (HashMap Text ResponseValue)

instance JSON.ToJSON ResponseValue where
  toJSON RNull = JSON.Null
  toJSON (RScalar s) = JSON.toJSON s
  toJSON (RList l) = JSON.toJSON l
  toJSON (RObject o) = JSON.toJSON o

-- introduce a sum type: is this a record containing deeper info or a value

type TheMonad a = GenHaxl () a

data KeyResponse = NodeResponse NodeHandler | ValueResponse ResponseValue
data KeyHandler = KeyHandler (HashMap Text InputValue -> TheMonad KeyResponse)
data NodeHandler = NodeHandler (HashMap Text KeyHandler)

data Server = Server
              { rootQuery :: NodeHandler
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

processSelectionSet :: NodeHandler -> AST.SelectionSet -> TheMonad (HashMap Text ResponseValue)
processSelectionSet (NodeHandler keyHandlers) selectionSet = do
  fmap HashMap.fromList $ forM selectionSet $ \case
    AST.SelectionField (AST.Field alias name arguments _directives innerSelectionSet) -> do
      let (Just (KeyHandler keyHandler)) = HashMap.lookup name keyHandlers
      let args = HashMap.fromList $ fmap decodeArgument arguments
      outputValue <- keyHandler args >>= \case
        NodeResponse handler -> do
          if null innerSelectionSet then do
            fail "Must select fields of node"
          else do
            RObject <$> processSelectionSet handler innerSelectionSet
        ValueResponse value -> do
          if null innerSelectionSet then do
            -- TODO: assert not RObject
            return value
          else do
            -- TODO: assert RObject
            let (RObject m) = value
            RObject <$> processSelectionSet (NodeHandler $ fmap (KeyHandler . const . return . ValueResponse) m) innerSelectionSet
      return (if Text.null alias then name else alias, outputValue)
    _ -> fail "unsupported selection"

handleRequest :: Server -> (Response -> IO b) -> AST.Document -> IO b
handleRequest server respond doc = do
  -- TODO: bad bad bad
  let (AST.Document defns) = doc
  let queries = [node | AST.DefinitionOperation (AST.Query node) <- defns]
  putStrLn $ show queries

  requestEnv <- initEnv (stateSet StarWarsState {-$ stateSet UserRequestState-} stateEmpty) ()
  outputs <- runHaxl requestEnv $ do
    for queries $ \(AST.Node name [] [] selectionSet) -> do
      output <- processSelectionSet (rootQuery server) selectionSet
      return (name, output)

  let response = HashMap.fromList [("data" :: Text, HashMap.fromList outputs )]
  respond $ responseLBS
    status200
    [("Content-Type", "application/json")]
    (JSON.encode response)


-- GraphQL classes

class GraphQLEnum a where
  enumName :: Proxy a -> Text
  enumDescription :: Proxy a -> Maybe Text
  enumValues :: Proxy a -> [a]
  renderValue :: a -> Text
  renderDescription :: a -> Text
  -- TODO: deprecation


-- Star Wars Data Source

newtype UserID = UserID Text
        deriving (Show, Eq, Hashable, IsString)

data User = User { userName :: Text }
     deriving (Show)


data UserRequest a where
    FetchUser :: UserID -> UserRequest User
  deriving Typeable

-- This function is necessary to resolve the GADT properly.  Otherwise you get insane errors like
-- 'b0' is untouchable: https://ghc.haskell.org/trac/ghc/ticket/9223
runUserRequest :: UserRequest a -> ResultVar a -> IO ()
runUserRequest (FetchUser userId) var = do
  if userId == "10" then
    putSuccess var $ User "FRIEND!!"
  else
    putSuccess var $ User "ME!!"

deriving instance Show (UserRequest a)
deriving instance Eq (UserRequest a)

instance DataSourceName UserRequest where
    dataSourceName _ = "UserRequestDataSource"

instance Show1 UserRequest where
    show1 (FetchUser (UserID userID)) = printf "FetchUser(%s)" (Text.unpack userID)

instance Hashable (UserRequest a) where
    hashWithSalt salt (FetchUser userId) = hashWithSalt salt (0 :: Int, userId)

instance StateKey UserRequest where
    data State UserRequest = NoStateE

instance DataSource () UserRequest where
    fetch _ _ _ reqs = SyncFetch $ do
        putStrLn $ "do some requests: " ++ show (length reqs)
        forM_ reqs $ \(BlockedFetch req var) -> do
            runUserRequest req var






responseValueFromUser :: User -> ResponseValue
responseValueFromUser (User name) = RObject $ HashMap.fromList
  [ ("name", RScalar $ SString $ name)
  ]

meHandler :: HashMap Text InputValue -> TheMonad KeyResponse
meHandler _ = do
  let myUserID = UserID "ME"
  user <- dataFetch (FetchUser myUserID)
  return $ ValueResponse $ responseValueFromUser user

friendHandler :: HashMap Text InputValue -> TheMonad KeyResponse
friendHandler args = do
  let (Just (IScalar (SString userID))) = HashMap.lookup "id" args
  user <- dataFetch (FetchUser (UserID userID))
  return $ ValueResponse $ responseValueFromUser user

responseValueFromCharacter :: Character -> ResponseValue
responseValueFromCharacter Character{..} = RObject $ HashMap.fromList
  [ ("name", RScalar $ SString $ cName)
  ]

responseValueFromEpisode :: Episode -> ResponseValue
responseValueFromEpisode Episode{..} = RObject $ HashMap.fromList
  [ ("name", RScalar $ SString $ eName)
  , ("releaseYear", RScalar $ SInt $ fromInteger $ toInteger $ eReleaseYear)
  ]

heroHandler :: ResolverArguments -> TheMonad KeyResponse
heroHandler args = do
  episodeID <- lookupArgument args "episode" >>= \case
    Just x -> return x
    Nothing -> return NewHope
  episode <- dataFetch $ FetchEpisode episodeID
  character <- dataFetch $ FetchCharacter $ eHero episode
  return $ ValueResponse $ responseValueFromCharacter character

episodeHandler :: ResolverArguments -> TheMonad KeyResponse
episodeHandler args = do
  episodeID <- requireArgument args "id"
  episode <- dataFetch $ FetchEpisode episodeID
  return $ ValueResponse $ responseValueFromEpisode episode

app :: Application
app request respond = do
  -- TODO: check the request URL
  -- TODO: check the request method (require POST)

  _body <- fmap (decodeUtf8 . toStrict) $ strictRequestBody request
  --let body' = "query our_names { me { name }, friend(id: \"10\") { name } }"
  let body' = "query HeroNameQuery { newhope_hero: hero(episode: NEWHOPE) { name } empire_hero: hero(episode: EMPIRE) { name } jedi_hero: hero(episode: JEDI) { name } } query EpisodeQuery { episode(id: NEWHOPE) { name releaseYear } }"

  queryDoc <- case parseOnly (document <* endOfInput) body' of
    Left err -> do
      fail $ "Error parsing query: " ++ err
    Right d -> do
      return d

  let rootQuery = NodeHandler $ HashMap.fromList
                  [ ("me", KeyHandler meHandler)
                  , ("friend", KeyHandler friendHandler)
                  , ("hero", KeyHandler heroHandler)
                  , ("episode", KeyHandler episodeHandler)
                  ]
  let server = Server rootQuery
  handleRequest server respond queryDoc

main :: IO ()
main = do
  putStrLn $ "http://localhost:8080/"
  run 8080 app
