module Printer
  ( colorizeError
  , prettyPrintCollections
  , prettyPrintCollection
  , prettyPrintConformance
  , prettyPrintItems
  ) where

import Ansi.Codes (Color(..))
import Ansi.Output (foreground, withGraphics)
import Data.Array.NonEmpty (fromArray)
import Data.Functor (void)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Stac (Collection(..), CollectionsResponse(..), ConformanceClasses, Item(..), Asset(..))
import Data.Traversable (fold, traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Foreign.Object (Object)
import Prelude (class Show, Unit, discard, show, ($), (*>), (<$>), (<<<), (<>))

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

prettyPrintAsset :: forall m. MonadEffect m => String -> Asset -> m Unit
prettyPrintAsset assetKey (Asset { _type, title, href }) =
  let
    titlePrefix = withGraphics (foreground Blue) (fromMaybe assetKey title) <> ": "

    mediaTypeSuffix = fold $ (" (" <> _) <<< (_ <> ")") <<< show <$> _type
  in
    log $ titlePrefix <> href <> mediaTypeSuffix

prettyPrintAssets :: forall m. MonadEffect m => Object Asset -> m Unit
prettyPrintAssets assets = void $ traverseWithIndex prettyPrintAsset assets

prettyPrintItem :: forall m. MonadEffect m => Item -> m Unit
prettyPrintItem (Item { id, stacExtensions, bbox, assets }) = do
  log $ prettyPrintKVPair "Id" id
  log $ prettyPrintKVPair "Extensions" stacExtensions
  log $ prettyPrintKVPair "Bbox" bbox
  log $ withGraphics (foreground Blue) "Assets:"
  prettyPrintAssets assets *> log "\n"

prettyPrintItems :: forall m. MonadEffect m => Array Item -> m Unit
prettyPrintItems features = case fromArray features of
  Just arr -> void $ traverse prettyPrintItem arr
  Nothing -> log "No items found"
