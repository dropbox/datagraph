{-# LANGUAGE LambdaCase #-}

module GraphQL where

import Data.Int (Int32)
import Data.Text (Text)
import qualified Data.Aeson as JSON
import Data.HashMap.Strict (HashMap)
import qualified Data.Text as Text
import qualified Data.HashMap.Strict as HashMap
import Data.Proxy

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

class GraphQLArgument a where
  decodeInputArgument :: InputValue -> Either String a

type ResolverArguments = HashMap Text InputValue

requireArgument :: (Monad m, GraphQLArgument a) => ResolverArguments -> Text -> m a
requireArgument args argName = do
  lookupArgument args argName >>= \case
    Just x -> return x
    Nothing -> fail $ "Required argument missing: " ++ Text.unpack argName

lookupArgument :: (Monad m, GraphQLArgument a) => ResolverArguments -> Text -> m (Maybe a)
lookupArgument args argName = do
  case HashMap.lookup argName args of
    Just x -> case decodeInputArgument x of
      Right y -> return $ Just y
      Left err -> fail $ "Error decoding argument " ++ Text.unpack argName ++ ": " ++ err
    Nothing -> return Nothing

-- TODO: actually use this
class GraphQLEnum a where
  enumName :: Proxy a -> Text
  enumDescription :: Proxy a -> Maybe Text
  enumValues :: Proxy a -> [a]
  renderValue :: a -> Text
  renderDescription :: a -> Text
  -- TODO: deprecation
