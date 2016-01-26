{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module StarWarsModel where

import Data.Text (Text)
import Data.Hashable (Hashable(..))
import Data.String (IsString)

data EpisodeID = NewHope | Empire | Jedi
  deriving (Eq, Show)
instance Hashable EpisodeID where
  hashWithSalt salt NewHope = hashWithSalt salt (0 :: Int)
  hashWithSalt salt Empire = hashWithSalt salt (1 :: Int)
  hashWithSalt salt Jedi = hashWithSalt salt (2 :: Int)

newtype CharacterID = CharacterID Text
  deriving (Eq, Show, Hashable, IsString)

data CharacterType
  = Human {- home planet -} (Maybe Text)
  | Droid {- primary function -} Text
  deriving (Eq, Show)

data Character = Character
  { cName :: Text
  , cFriends :: [CharacterID]
  , cAppearsIn :: [EpisodeID]
  , cType :: CharacterType
  }
  deriving (Eq, Show)
