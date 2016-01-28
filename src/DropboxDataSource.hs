{-# LANGUAGE StandaloneDeriving, GADTs, TypeFamilies, MultiParamTypeClasses, GeneralizedNewtypeDeriving, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module DropboxDataSource where

import Data.Text (Text)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Data.Hashable
import qualified Data.HashMap.Strict as HashMap
import Haxl.Core
import Text.Printf
import qualified Data.Text as Text
import Control.Monad (forM_)
import GraphQL
import GraphQLHelpers

newtype UserID = UserID Text
  deriving (Show, Eq, Hashable, IsString)

instance GraphQLArgument UserID where
  decodeInputArgument (IScalar (SString cid)) = Right $ UserID cid
  decodeInputArgument _ = Left $ "invalid Character ID"


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
  data State UserRequest = UserRequestState

instance DataSource () UserRequest where
  fetch _ _ _ reqs = SyncFetch $ do
    putStrLn $ "do some requests: " ++ show (length reqs)
    forM_ reqs $ \(BlockedFetch req var) -> do
      runUserRequest req var

instance GraphQLObject User where
  resolveObject (User name) = HashMap.fromList
    [ ("name", knownValue name)
    ]

instance GraphQLID UserID where
  fetchByID userID = do
    user <- dataFetch (FetchUser userID)
    return $ RObject $ resolveObject user
