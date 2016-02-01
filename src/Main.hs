{-# LANGUAGE OverloadedStrings, LambdaCase, GADTs, StandaloneDeriving, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, InstanceSigs, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables, RecordWildCards, PartialTypeSignatures #-}

module Main (main) where

--import Debug.Trace
import Network.Wai
import Network.HTTP.Types (status200)
import Network.Wai.Handler.Warp (run)
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
import qualified Data.Aeson.Encode.Pretty as JSON

import GraphQL
import GraphQLHelpers
import DropboxDataSource
import StarWarsModel
import StarWarsDataSource
import NumberDataSource

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
  fmap HashMap.fromList $ for selectionSet $ \case
    AST.SelectionField (AST.Field alias name arguments _directives innerSelectionSet) -> do
      -- traceShowM $ name
      valueResolver <- case HashMap.lookup name objectResolver of
        Just vr -> return vr
        Nothing -> fail $ "Requested unknown field: " ++ Text.unpack name

      let args = HashMap.fromList $ fmap decodeArgument arguments
      outputValue <- valueResolver args >>= \case
        RNull -> return FNull
        RScalar s -> return $ FScalar s
        RList ls -> do
          if null innerSelectionSet then do
            fail "TODO: lists without selection sets are unsupported"
          else do
            elements <- for ls $ \elementResolver -> do
              element <- elementResolver HashMap.empty >>= \case
                RObject elementObjectResolver -> do
                  processSelectionSet elementObjectResolver innerSelectionSet
                _ -> do
                  fail "Selecting fields from lists requires that all element values be lists"
              return (FObject element :: FullyResolvedValue)
            return (FList elements :: FullyResolvedValue)
        RObject o -> do
          if null innerSelectionSet then do
            fail "Must select fields out of object"
          else do
            FObject <$> processSelectionSet o innerSelectionSet
      return (if Text.null alias then name else alias, outputValue)
    _ -> fail "unsupported selection"

meResolver :: ValueResolver
meResolver = idResolver $ UserID "ME"

friendResolver :: ValueResolver
friendResolver args = do
  userID <- requireArgument args "id"
  fetchByID (userID :: UserID)

heroResolver :: ValueResolver
heroResolver args = do
  episodeID <- lookupArgument args "episode" >>= \case
    Just x -> return x
    Nothing -> return NewHope
  episode <- dataFetch $ FetchEpisode episodeID
  character <- dataFetch $ FetchCharacter $ eHero episode
  return $ RObject $ resolveObject character

episodeResolver :: ValueResolver
episodeResolver args = do
  episodeID <- requireArgument args "id"
  episode <- dataFetch $ FetchEpisode episodeID
  return $ RObject $ resolveObject episode

addToNumberResolver :: ValueResolver
addToNumberResolver args = do
  newNumber <- requireArgument args "newNumber"
  () <- uncachedRequest $ AddToNumber newNumber
  -- CAREFUL - the () <- above causes ghc to emit a >>= rather than >> which
  -- is important because >>= guarantees sequencing in haxl but >> runs
  -- both sides in parallel.  Running in parallel here is a bad deal because
  -- the fetch needs to happen after the write.
  newNumberObject <- dataFetch FetchCurrentNumber
  return $ RObject $ resolveObject newNumberObject

data Server = Server
  { rootQuery :: ObjectResolver
  , rootMutation :: ObjectResolver
  }

data QueryBatch
  = QueryBatch [AST.Node]
  | SingleMutation AST.Node

data AccumulationState = AccumulationState [AST.Node] [QueryBatch]

flushQueries :: AccumulationState -> [QueryBatch]
flushQueries (AccumulationState [] batches) = batches
flushQueries (AccumulationState queries batches) = QueryBatch (reverse queries) : batches

addDefinition :: AccumulationState -> AST.OperationDefinition -> AccumulationState
addDefinition (AccumulationState queries batches) (AST.Query node) =
  AccumulationState (node : queries) batches
addDefinition acc (AST.Mutation node) =
  AccumulationState [] (SingleMutation node : flushQueries acc)

groupQueries :: [AST.OperationDefinition] -> [QueryBatch]
groupQueries = reverse . flushQueries . foldl' addDefinition (AccumulationState [] [])

handleRequest :: Server -> StateStore -> (Response -> IO b) -> AST.Document -> IO b
handleRequest server stateStore respond doc = do
  let (AST.Document defns) = doc
  let operations = [op | AST.DefinitionOperation op <- defns]
  let groups = groupQueries operations

  outputs <- for groups $ \case
    QueryBatch queries -> do
      queryEnv <- initEnv stateStore ()
      runHaxl queryEnv $ do
        for queries $ \(AST.Node name [] [] selectionSet) -> do
          output <- processSelectionSet (rootQuery server) selectionSet
          return (name, output)
    SingleMutation mutation -> do
      let (AST.Node name [] [] selectionSet) = mutation
      -- top-level mutations must be executed in order, and clear the cache
      -- in between
      maps <- for selectionSet $ \selection -> do
        mutationEnv <- initEnv stateStore ()
        runHaxl mutationEnv $ do
          processSelectionSet (rootMutation server) [selection]
      return [(name, mconcat $ maps)]

  let response = HashMap.fromList [("data" :: Text, HashMap.fromList $ mconcat outputs )]
  respond $ responseLBS
    status200
    [("Content-Type", "application/json")]
    (JSON.encodePretty response)

app :: StateStore -> Application
app stateStore request respond = do
  -- TODO: check the request URL
  -- TODO: check the request method (require POST)

  _body <- fmap (decodeUtf8 . toStrict) $ strictRequestBody request

  let body' = Text.unlines
        [ ""
        , "query our_names { me { name }, friend(id: \"10\") { name } }"
        , "query HeroNameQuery { newhope_hero: hero(episode: NEWHOPE) { name } empire_hero: hero(episode: EMPIRE) { name } jedi_hero: hero(episode: JEDI) { name } }"
        , "query EpisodeQuery { episode(id: NEWHOPE) { name releaseYear } }"
        , "query newhope_hero_friends { episode(id: NEWHOPE) { hero { name, friends { name }, appearsIn { releaseYear } } } }"
        , "mutation numbers { first: addToNumber(newNumber: 1) { theNumber } second: addToNumber(newNumber: 2) { theNumber } third: addToNumber(newNumber: 3) { theNumber } }"
        ]

  queryDoc <- case parseOnly (document <* endOfInput) body' of
    Left err -> do
      fail $ "Error parsing query: " ++ err
    Right d -> do
      return d

  let rootQuery = HashMap.fromList
        [ ("me", meResolver)
        , ("friend", friendResolver)
        , ("hero", heroResolver)
        , ("episode", episodeResolver)
        ]
  let rootMutation = HashMap.fromList
        [ ("addToNumber", addToNumberResolver)
        ]

  let server = Server rootQuery rootMutation
  handleRequest server stateStore respond queryDoc

main :: IO ()
main = do
  putStrLn $ "http://localhost:8080/"

  conn <- openStarWarsRedisConnection
  nds <- initializeNumberDataSource 0
  let stateStore = stateSet nds $ stateSet conn $ stateSet UserRequestState $ stateEmpty
  run 8080 $ app stateStore
