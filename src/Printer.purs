module Printer where

import Ansi.Codes (Color(..))
import Ansi.Output (foreground, withGraphics)
import Data.Functor (void)
import Data.Stac (Collection(..), CollectionsResponse(..))
import Data.Traversable (traverse)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Prelude (Unit, ($), (<<<), (<>))

prettyPrintCollections :: forall m. MonadEffect m => CollectionsResponse -> m Unit
prettyPrintCollections (CollectionsResponse { collections }) =
  let
    collectionLine (Collection collection) = withGraphics (foreground Blue) (collection.id <> ": ") <> collection.description <> "\n"
  in
    void $ traverse (log <<< collectionLine) collections
