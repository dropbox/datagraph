{-# LANGUAGE StandaloneDeriving, GADTs, TypeFamilies, MultiParamTypeClasses, GeneralizedNewtypeDeriving, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module NumberDataSource where

import Data.Hashable
import Haxl.Core
import Data.IORef
import Control.Monad (forM_)
import Text.Printf
import GraphQLHelpers
import qualified Data.HashMap.Strict as HashMap

data NumberRequest a where
  AddToNumber :: Int -> NumberRequest ()
  FetchCurrentNumber :: NumberRequest NumberObject

deriving instance Eq (NumberRequest a)
deriving instance Show (NumberRequest a)

instance Hashable (NumberRequest a) where
  hashWithSalt salt (AddToNumber i) = hashWithSalt salt (0 :: Int, i)
  hashWithSalt salt (FetchCurrentNumber) = hashWithSalt salt (1 :: Int)

data NumberObject = NumberObject
  { theNumber :: Int
  }
  deriving (Show)

instance GraphQLObject NumberObject where
  resolveObject (NumberObject v) = HashMap.fromList
    [ ("theNumber", knownValue v)
    ]

instance StateKey NumberRequest where
  data State NumberRequest = NumberRequestState (IORef Int)

runNumberRequest :: IORef Int -> NumberRequest a -> ResultVar a -> IO ()
runNumberRequest numberRef (AddToNumber i) var = do
  modifyIORef numberRef (+ i)
  putSuccess var ()
runNumberRequest numberRef FetchCurrentNumber var = do
  NumberObject <$> readIORef numberRef >>= putSuccess var

instance DataSourceName NumberRequest where
  dataSourceName _ = "NumberRequestDataSource"

instance Show1 NumberRequest where
  show1 (AddToNumber i) = printf "AddToNumber(%i)" i
  show1 (FetchCurrentNumber) = "FetchCurrentNumber"

instance DataSource () NumberRequest where
  fetch (NumberRequestState numberRef) _ _ reqs = SyncFetch $ do
    putStrLn $ "do some number requests: " ++ show [show1 req | BlockedFetch req _ <- reqs]
    forM_ reqs $ \(BlockedFetch req var) -> do
      runNumberRequest numberRef req var

initializeNumberDataSource :: Int -> IO (State NumberRequest)
initializeNumberDataSource i = NumberRequestState <$> newIORef i
