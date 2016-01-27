module GraphQL where

import Data.Int (Int32)
import Data.Text (Text)
import qualified Data.Aeson as JSON
import Data.HashMap.Strict (HashMap)

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
