{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}

module StarWarsModel where

import qualified Data.Text as Text
import Data.Text (Text)
import Data.Hashable (Hashable(..))
import Data.String (IsString)
import GraphQL

-- Episode ID

data EpisodeID = NewHope | Empire | Jedi
  deriving (Eq, Show)

instance GraphQLArgument EpisodeID where
  decodeInputArgument (IScalar (SEnum episode)) = case episode of
    "NEWHOPE" -> return NewHope
    "EMPIRE" -> return Empire
    "JEDI" -> return Jedi
    _ -> Left $ "Unknown episode enum: " ++ Text.unpack episode
  decodeInputArgument _ = Left $ "invalid episode ID"

instance Hashable EpisodeID where
  hashWithSalt salt NewHope = hashWithSalt salt (0 :: Int)
  hashWithSalt salt Empire = hashWithSalt salt (1 :: Int)
  hashWithSalt salt Jedi = hashWithSalt salt (2 :: Int)

-- Episode

data Episode = Episode
  { eName :: Text
  , eReleaseYear :: Int
  , eHero :: CharacterID
  }
  deriving (Eq, Show)

-- CharacterID

newtype CharacterID = CharacterID Text
  deriving (Eq, Show, Hashable, IsString)

-- Character

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
