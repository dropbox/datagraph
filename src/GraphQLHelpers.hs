module GraphQLHelpers where

import Data.Text (Text)
import GraphQL

class KnownValue v where
  encodeKnownValue :: v -> ResolvedValue

instance KnownValue Text where
  encodeKnownValue = RScalar . SString

instance KnownValue Int where
  encodeKnownValue = RScalar . SInt . fromInteger . toInteger

knownValue :: KnownValue v => v -> ValueResolver
-- TODO: assert _args is empty
knownValue v _args = return $ encodeKnownValue v

class GraphQLID id where
  fetchByID :: id -> GraphQLHandler ResolvedValue

idResolver :: GraphQLID id => id -> ValueResolver
-- TODO: assert _args is empty
idResolver i _args = fetchByID i

listResolver :: GraphQLID id => [id] -> ValueResolver
listResolver elementIDs _args = do
  -- TODO: assert _args is empty
  return $ RList $ fmap idResolver elementIDs

class GraphQLObject a where
  resolveObject :: a -> ObjectResolver
