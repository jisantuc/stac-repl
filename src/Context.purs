module Context where

import Data.Maybe (Maybe(..))
import Types (CollectionContextRecord, Context(..), RootUrl, RootContextRecord)

toCollectionContext :: Context -> Maybe CollectionContextRecord
toCollectionContext (CollectionContext rec) = Just rec

toCollectionContext _ = Nothing

toRootUrl :: Context -> Maybe RootUrl
toRootUrl (RootContext { rootUrl: Just u }) = Just u

toRootUrl (CollectionContext { rootUrl }) = Just rootUrl

toRootUrl _ = Nothing

toRootContext :: Context -> Maybe RootContextRecord
toRootContext (RootContext rec) = Just rec

toRootContext _ = Nothing
