module Printer
  ( colorizeError
  , prettyPrintCollections
  , prettyPrintCollection
  , prettyPrintConformance
  ) where

import Ansi.Codes (Color(..))
import Ansi.Output (foreground, withGraphics)
import Data.Functor (void)
import Data.Stac (Collection(..), CollectionsResponse(..), ConformanceClasses)
import Data.Traversable (traverse)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Prelude (class Show, Unit, discard, show, ($), (<<<), (<>))

prettyPrintKVPair :: forall a. Show a => String -> a -> String
prettyPrintKVPair key value = (withGraphics (foreground Blue) key) <> ": " <> show value

prettyPrintCollections :: forall m. MonadEffect m => CollectionsResponse -> m Unit
prettyPrintCollections (CollectionsResponse { collections }) =
  let
    collectionLine (Collection collection) = withGraphics (foreground Blue) (collection.id <> ": ") <> collection.description <> "\n"
  in
    void $ traverse (log <<< collectionLine) collections

prettyPrintConformance :: forall m. MonadEffect m => ConformanceClasses -> m Unit
prettyPrintConformance { conformsTo } = do
  log $ withGraphics (foreground Blue) "Conforms to:\n"
  void $ traverse log conformsTo

prettyPrintCollection :: forall m. MonadEffect m => Collection -> m Unit
prettyPrintCollection (Collection record) = do
  log $ (withGraphics (foreground Blue) "Got collection ") <> record.id
  log $ "Here's some information about it."
  log $ prettyPrintKVPair "License" record.license
  log $ prettyPrintKVPair "Temporal extent" record.extent.temporal
  log $ prettyPrintKVPair "Spatial extent" record.extent.spatial

colorizeError :: String -> String
colorizeError = withGraphics (foreground Red)
