{-# LANGUAGE StandaloneDeriving, GADTs, TypeFamilies, MultiParamTypeClasses, OverloadedStrings, LambdaCase, RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module StarWarsDataSource where

import Data.Hashable (Hashable(..))
import Text.Printf
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.HashMap.Strict as HashMap
import Haxl.Core
import Control.Monad (void, forM)
import Database.Redis
import Data.ByteString (ByteString)
import Data.Aeson as JSON
import Data.ByteString.Lazy (fromStrict)
import Data.Monoid
import GraphQL
import GraphQLHelpers

import StarWarsModel
--import StarWarsData

data StarWarsRequest a where
  FetchCharacter :: CharacterID -> StarWarsRequest Character
  FetchEpisode :: EpisodeID -> StarWarsRequest Episode

deriving instance Eq (StarWarsRequest a)
deriving instance Show (StarWarsRequest a)

-- Is there a way to make GHC automatically derive this?
instance Hashable (StarWarsRequest a) where
  hashWithSalt salt (FetchCharacter userId) = hashWithSalt salt (0 :: Int, userId)
  hashWithSalt salt (FetchEpisode episodeId) = hashWithSalt salt (1 :: Int, episodeId)

class RedisKey a where
  encodeRedisKey :: a -> ByteString

instance RedisKey CharacterID where
  encodeRedisKey (CharacterID c) = "CHARACTER:" <> Text.encodeUtf8 c

instance RedisKey EpisodeID where
  encodeRedisKey NewHope = "EPISODE:4"
  encodeRedisKey Empire = "EPISODE:5"
  encodeRedisKey Jedi= "EPISODE:6"

readRedisValue :: (RedisKey k, FromJSON v) => k -> Redis (Maybe v)
readRedisValue k = do
  get (encodeRedisKey k) >>= \case
    Right (Just bs) -> return $ JSON.decode $ fromStrict bs
    _ -> return Nothing

runStarWarsRequest :: StarWarsRequest a -> ResultVar a -> Redis (IO ())
runStarWarsRequest (FetchCharacter characterID) var = do
  readRedisValue characterID >>= \case
    Just c -> do
      return $ putSuccess var c
    Nothing -> do
      return $ putFailure var $ userError "No such character or failed to decode"
runStarWarsRequest (FetchEpisode episodeID) var = do
  readRedisValue episodeID >>= \case
    Just e -> do
      return $ putSuccess var e
    Nothing -> do
      return $ putFailure var $ userError "No such episode or failed to decode"

{-
  case HashMap.lookup characterID starWarsCharacters of
    Just c -> do
      putSuccess var c
    Nothing -> do
      putFailure var $ userError "No such character"

  case HashMap.lookup episodeID starWarsEpisodes of
    Just e -> do
      putSuccess var e
    Nothing -> do
      putFailure var $ userError $ "No such episode: " ++ show episodeID
-}

instance DataSourceName StarWarsRequest where
  dataSourceName _ = "StarWarsRequest"

instance StateKey StarWarsRequest where
  data State StarWarsRequest = StarWarsState Connection

instance Show1 StarWarsRequest where
  show1 (FetchCharacter (CharacterID characterID)) = printf "FetchCharacter(%s)" (Text.unpack characterID)
  show1 (FetchEpisode episodeID) = printf "FetchEpisode(%s)" (show episodeID)

instance DataSource () StarWarsRequest where
  fetch (StarWarsState conn) _ _ reqs = SyncFetch $ do
    putStrLn $ "fetch star wars batch of size " ++ show (length reqs) ++ ": " ++ show [show1 req | BlockedFetch req _var <- reqs]
    actions <- runRedis conn $ do
      forM reqs $ \(BlockedFetch req var) -> do
        runStarWarsRequest req var
    void $ sequence actions

openConnection :: IO (State StarWarsRequest)
openConnection = do
  putStrLn "Connecting to Redis"
  conn <- connect defaultConnectInfo
  return $ StarWarsState conn

instance GraphQLID CharacterID where
  fetchByID characterID = do
    character <- dataFetch (FetchCharacter characterID)
    return $ RObject $ resolveObject character

instance GraphQLID EpisodeID where
  fetchByID episodeID = do
    episode <- dataFetch (FetchEpisode episodeID)
    return $ RObject $ resolveObject episode

instance GraphQLObject Character where
  resolveObject Character{..} = HashMap.fromList
    [ ("name", knownValue cName)
    , ("friends", listResolver cFriends)
    , ("appearsIn", listResolver cAppearsIn)
    ]

instance GraphQLObject Episode where
  resolveObject Episode{..} = HashMap.fromList
    [ ("name", knownValue eName)
    , ("releaseYear", knownValue eReleaseYear)
    , ("hero", idResolver eHero)
    ]
