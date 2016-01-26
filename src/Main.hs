{-# LANGUAGE OverloadedStrings, LambdaCase, GADTs, StandaloneDeriving, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, InstanceSigs, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables, RecordWildCards #-}

module Main where

import Data.Proxy
import Debug.Trace
import Network.Wai
import Network.HTTP.Types (status200)
import Network.Wai.Handler.Warp (run)
import qualified Data.Aeson as JSON
import Data.Int (Int32)
import Data.Text.Encoding (decodeUtf8)
import Data.Attoparsec.Text (parseOnly, endOfInput)
import Data.Hashable
import Data.Text (Text)
import qualified Data.Text as Text
import Data.ByteString.Lazy (toStrict)
import qualified Data.GraphQL.AST as AST
import Data.GraphQL.Parser (document)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.IORef
import Haxl.Prelude
import Haxl.Core
import Text.Printf
import Data.Typeable

import StarWarsModel
import StarWarsData

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

data Scalar
     = SInt Int32
     | SFloat Double
     | SBoolean Bool
     | SString Text
     | SEnum Text -- unquoted on parse
     deriving (Show)

instance JSON.ToJSON Scalar where
  toJSON (SInt i) = JSON.toJSON i
  toJSON (SFloat f) = JSON.toJSON f
  toJSON (SBoolean b) = JSON.toJSON b
  toJSON (SString s) = JSON.toJSON s
  toJSON (SEnum t) = JSON.toJSON t

data InputValue
     = IVar Text
     | IScalar Scalar
     | IList [InputValue]
     | IObject (HashMap Text InputValue)
     deriving (Show)

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
    AST.SelectionField (AST.Field alias name arguments directives innerSelectionSet) -> do
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

handleRequest server respond doc = do
  -- TODO: bad bad bad
  let (AST.Document defns) = doc
  let (query:_) = [node | AST.DefinitionOperation (AST.Query node) <- defns]
  putStrLn $ show query

  let AST.Node name [] [] selectionSet = query

  env <- initEnv (stateSet StarWarsState {-$ stateSet UserRequestState-} stateEmpty) ()
  output <- runHaxl env $ processSelectionSet (rootQuery server) selectionSet

  let response = HashMap.fromList [("data" :: Text, HashMap.fromList [(name, output)] )]
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

data StarWarsRequest a where
  FetchCharacter :: CharacterID -> StarWarsRequest Character
deriving instance Eq (StarWarsRequest a)
deriving instance Show (StarWarsRequest a)

instance Hashable (StarWarsRequest a) where
    hashWithSalt salt (FetchCharacter userId) = hashWithSalt salt (0 :: Int, userId)

runStarWarsRequest :: StarWarsRequest a -> ResultVar a -> IO ()
runStarWarsRequest (FetchCharacter characterID) var = do
  case HashMap.lookup characterID starWarsCharacters of
    Just c -> do
      putSuccess var c
    Nothing -> do
      putFailure var $ userError "No such character"

instance DataSourceName StarWarsRequest where
  dataSourceName _ = "StarWarsRequest"

instance StateKey StarWarsRequest where
    data State StarWarsRequest = StarWarsState

instance Show1 StarWarsRequest where
    show1 (FetchCharacter (CharacterID characterID)) = printf "FetchCharacter(%s)" (Text.unpack characterID)

instance DataSource () StarWarsRequest where
    fetch _ _ _ reqs = SyncFetch $ do
        putStrLn $ "do some star wars requests: " ++ show (length reqs)
        forM_ reqs $ \(BlockedFetch req var) -> do
            runStarWarsRequest req var

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
  traceShowM $ show args
  let (Just (IScalar (SString userID))) = HashMap.lookup "id" args
  user <- dataFetch (FetchUser (UserID userID))
  return $ ValueResponse $ responseValueFromUser user

responseValueFromCharacter :: Character -> ResponseValue
responseValueFromCharacter Character{..} = RObject $ HashMap.fromList
  [ ("name", RScalar $ SString $ cName)
  ]

heroHandler :: HashMap Text InputValue -> TheMonad KeyResponse
heroHandler args = do
  traceShowM $ show args
  episode <- case HashMap.lookup "episode" args of
    Just (IScalar (SEnum episode)) -> do
      case episode of
        "NEWHOPE" -> return NewHope
        "EMPIRE" -> return Empire
        "JEDI" -> return Jedi
        _ -> fail "Unknown episode enum"
    Nothing -> return NewHope

  character <- if episode == Empire then
    -- Luke is the hero of Episode V.
    dataFetch $ FetchCharacter "1000"
  else do
    -- Artoo is the hero otherwise.
    dataFetch $ FetchCharacter "2001"
  return $ ValueResponse $ responseValueFromCharacter character

app :: Application
app request respond = do
  -- TODO: check the request URL
  -- TODO: check the request method (require POST)

  body <- fmap (decodeUtf8 . toStrict) $ strictRequestBody request
  --let body' = "query our_names { me { name }, friend(id: \"10\") { name } }"
  let body' = "query HeroNameQuery { newhope_hero: hero(episode: NEWHOPE) { name } empire_hero: hero(episode: EMPIRE) { name } jedi_hero: hero(episode: JEDI) { name } }"

  queryDoc <- case parseOnly (document <* endOfInput) body' of
    Left err -> do
      fail $ "Error parsing query: " ++ err
    Right d -> do
      return d

  let rootQuery = NodeHandler $ HashMap.fromList
                  [ ("me", KeyHandler meHandler)
                  , ("friend", KeyHandler friendHandler)
                  , ("hero", KeyHandler heroHandler)
                  ]
  let server = Server rootQuery
  handleRequest server respond queryDoc

main :: IO ()
main = do
  putStrLn $ "http://localhost:8080/"
  run 8080 app
