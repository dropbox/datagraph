{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, LambdaCase, RecordWildCards #-}

module StarWarsModel where

import qualified Data.Text as Text
import Data.Text (Text)
import Data.Hashable (Hashable(..))
import Data.String (IsString)
import GraphQL
import Data.Aeson (FromJSON(..), Value(..), (.:))
import Control.Applicative ((<|>))

-- Episode ID

data EpisodeID = NewHope | Empire | Jedi
  deriving (Eq, Show)

instance FromJSON EpisodeID where
  parseJSON (Number 4) = return NewHope
  parseJSON (Number 5) = return Empire
  parseJSON (Number 6) = return Jedi
  parseJSON _ = fail "Unknown EpisodeID"

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

instance FromJSON Episode where
  parseJSON (Object o) = do
    eName <- o .: "name"
    eReleaseYear <- o .: "releaseYear"
    eHero <- o .: "hero"
    return Episode{..}
  parseJSON _ = fail "Episode must be an Object"

-- CharacterID

newtype CharacterID = CharacterID Text
  deriving (Eq, Show, Hashable, IsString, FromJSON)

instance GraphQLArgument CharacterID where
  decodeInputArgument (IScalar (SString cid)) = Right $ CharacterID cid
  decodeInputArgument _ = Left $ "invalid Character ID"

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

instance FromJSON Character where
  parseJSON (Object o) = do
    cName <- o .: "name"
    cFriends <- o .: "friends"
    cAppearsIn <- o .: "appearsIn"
    cType <- (Human <$> o .: "homePlanet") <|> (Droid <$> o .: "primaryFunction")
    return Character{..}
  parseJSON _ = fail "Character must be an Object"
