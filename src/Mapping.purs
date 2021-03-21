module Mapping (drawMap, Latitude(..), Longitude(..)) where

import Control.Promise (Promise)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Prelude (Unit, ($), (<<<))

type MapsciiConfig
  = { initialLat :: Number
    , initialLon :: Number
    , initialZoom :: Number
    , size ::
        { width :: Int
        , height :: Int
        }
    , source :: String
    }

foreign import drawMapImpl :: MapsciiConfig -> Effect (Promise Unit)

newtype Longitude
  = Longitude Number

newtype Latitude
  = Latitude Number

options :: Latitude -> Longitude -> MapsciiConfig
options (Latitude y) (Longitude x) =
  { initialLat: y
  , initialLon: x
  , initialZoom: 9.0
  , size: { width: 120, height: 120 }
  , source: "http://mapscii.me/"
  }

drawMap :: forall m. MonadEffect m => Latitude -> Longitude -> m (Promise Unit)
drawMap lat lon = liftEffect <<< drawMapImpl $ options lat lon
