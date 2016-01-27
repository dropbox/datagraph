{-# LANGUAGE StandaloneDeriving, GADTs, TypeFamilies, MultiParamTypeClasses, OverloadedStrings #-}

module StarWarsDataSource where

import Data.Hashable (Hashable(..))
import Text.Printf
import qualified Data.Text as Text
import qualified Data.HashMap.Strict as HashMap
import Haxl.Core
import Control.Monad (forM_)

import StarWarsModel
import StarWarsData

data StarWarsRequest a where
  FetchCharacter :: CharacterID -> StarWarsRequest Character
  FetchEpisode :: EpisodeID -> StarWarsRequest Episode

deriving instance Eq (StarWarsRequest a)
deriving instance Show (StarWarsRequest a)

-- Is there a way to make GHC automatically derive this?
instance Hashable (StarWarsRequest a) where
    hashWithSalt salt (FetchCharacter userId) = hashWithSalt salt (0 :: Int, userId)
    hashWithSalt salt (FetchEpisode episodeId) = hashWithSalt salt (1 :: Int, episodeId)

runStarWarsRequest :: StarWarsRequest a -> ResultVar a -> IO ()
runStarWarsRequest (FetchCharacter characterID) var = do
  case HashMap.lookup characterID starWarsCharacters of
    Just c -> do
      putSuccess var c
    Nothing -> do
      putFailure var $ userError "No such character"
runStarWarsRequest (FetchEpisode episodeID) var = do
  case HashMap.lookup episodeID starWarsEpisodes of
    Just e -> do
      putSuccess var e
    Nothing -> do
      putFailure var $ userError "No such episode"

instance DataSourceName StarWarsRequest where
  dataSourceName _ = "StarWarsRequest"

instance StateKey StarWarsRequest where
    data State StarWarsRequest = StarWarsState

instance Show1 StarWarsRequest where
  show1 (FetchCharacter (CharacterID characterID)) = printf "FetchCharacter(%s)" (Text.unpack characterID)
  show1 (FetchEpisode episodeID) = printf "FetchEpisode(%s)" (show episodeID)

instance DataSource () StarWarsRequest where
  fetch _ _ _ reqs = SyncFetch $ do
    putStrLn $ "do some star wars requests: " ++ show (length reqs)
    forM_ reqs $ \(BlockedFetch req var) -> do
      runStarWarsRequest req var
