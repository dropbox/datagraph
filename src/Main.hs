{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Main where

import Network.Wai
import Network.HTTP.Types (status200)
import Network.Wai.Handler.Warp (run)
import Data.Aeson
import Data.Int (Int32)
import Data.Text.Encoding (decodeUtf8)
import Data.Attoparsec.Text (parseOnly, endOfInput)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.ByteString.Lazy (toStrict)
import qualified Data.GraphQL.AST as AST
import Data.GraphQL.Parser (document)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.IORef
import Control.Monad (forM_)

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

instance ToJSON Main.Value where
  toJSON VNull = Null
  toJSON (VInt i) = toJSON i
  toJSON (VFloat f) = toJSON f
  toJSON (VBoolean b) = toJSON b
  toJSON (VString s) = toJSON s
  toJSON (VEnum n) = toJSON n
  toJSON (VList ls) = toJSON ls
  toJSON (VObject o) = toJSON o
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

instance ToJSON Scalar where
  toJSON (SInt i) = toJSON i
  toJSON (SFloat f) = toJSON f
  toJSON (SBoolean b) = toJSON b
  toJSON (SString s) = toJSON s
  toJSON (SEnum t) = toJSON t

data InputValue
     = IVar Text
     | IScalar Scalar
     | IList [InputValue]
     | IObject (HashMap Text InputValue)

data ResponseValue
     = RNull
     | RScalar Scalar
     | RList [ResponseValue]
     | RObject (HashMap Text ResponseValue)
     | RNode NodeHandler

instance ToJSON ResponseValue where
  toJSON RNull = Null
  toJSON (RScalar s) = toJSON s
  toJSON (RList l) = toJSON l
  toJSON (RObject o) = toJSON o
  -- TODO: a wart
  toJSON (RNode _) = error "Cannot encode NodeHandler into JSON -- unresolved data"

-- introduce a sum type: is this a record containing deeper info or a value

data KeyHandler = KeyHandler (HashMap Text InputValue -> IO ResponseValue)
data NodeHandler = NodeHandler (HashMap Text KeyHandler)

data Server = Server
              { rootQuery :: NodeHandler
              }
              
processSelectionSet :: NodeHandler -> AST.SelectionSet -> IO (HashMap Text ResponseValue)
processSelectionSet (NodeHandler keyHandlers) selectionSet = do
  rv <- newIORef $ HashMap.empty
  forM_ selectionSet $ \case
    AST.SelectionField (AST.Field alias name arguments directives innerSelectionSet) -> do
      let (Just (KeyHandler keyHandler)) = HashMap.lookup name keyHandlers
      let arguments = HashMap.empty -- TODO
      outputValue <- keyHandler arguments
      modifyIORef rv $ HashMap.insert (if Text.null alias then name else alias) outputValue
    _ -> error "unsupported selection"
  readIORef rv

handleRequest server respond doc = do
  -- TODO: bad bad bad
  let (AST.Document defns) = doc
  let (query:_) = [node | AST.DefinitionOperation (AST.Query node) <- defns]
  putStrLn $ show query
        
  let AST.Node name [] [] selectionSet = query
  
  output <- processSelectionSet (rootQuery server) selectionSet

  let response = HashMap.fromList [(name, output)]
  respond $ responseLBS
    status200
    [("Content-Type", "application/json")]
    (encode response)
    
meHandler :: HashMap Text InputValue -> IO ResponseValue
meHandler _ = do
  return $ RScalar $ SString "me!"

app :: Application
app request respond = do
  -- TODO: check the request URL
  -- TODO: check the request method (require POST)

  body <- fmap (decodeUtf8 . toStrict) $ strictRequestBody request
  let body' = "query is_this_needed { me }"
  
  queryDoc <- case parseOnly (document <* endOfInput) body' of
    Left err -> do
      fail $ "Error parsing query: " ++ err
    Right d -> do
      return d

  let rootQuery = NodeHandler $ HashMap.fromList
                  [ ("me", KeyHandler meHandler)
                  ]
  let server = Server rootQuery
  handleRequest server respond queryDoc

main :: IO ()
main = do
  putStrLn $ "http://localhost:8080/"
  run 8080 app
