module Completions (getCompletions) where

import Command (collectionIdParser, locateItemIdParser)
import Data.Array (filter)
import Data.Either (fromRight)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe)
import Data.Set (Set, toUnfoldable)
import Data.String (Pattern(..), contains)
import Effect (Effect)
import Effect.Ref (Ref, read)
import Prelude (bind, const, pure, ($), (<$>), (<>))
import Text.Parsing.Parser (runParser)
import Types (Context(..), RootUrl)

getCompletions :: Ref Context -> String -> Effect ({ matched :: String, completions :: Array String })
getCompletions ctxRef s = do
  ctx <- read ctxRef
  contextCompleter ctx $ s

collectionCommands :: Array String
collectionCommands =
  [ "view"
  , "unset collection"
  , "locate collection"
  , "get conformance"
  , "list items"
  , "next page"
  , "locate item"
  ]

rootCommands :: Maybe RootUrl -> Array String
rootCommands rootUrlM =
  [ "set root url" ]
    <> foldMap
        ( const [ "get conformance", "set collection", "list collections" ]
        )
        rootUrlM

collectionIdMatches :: Set String -> String -> Array String
collectionIdMatches knownCollections s =
  let
    collectionIdParseResult = runParser s collectionIdParser
  in
    ("set collection " <> _)
      <$> ( fromRight []
            $ ( \collectionId ->
                  filter
                    ( \known -> contains (Pattern collectionId) known
                    )
                    (toUnfoldable knownCollections)
              )
            <$> collectionIdParseResult
        )

itemIdMatches :: Set String -> String -> Array String
itemIdMatches knownItems s =
  let
    itemIdParseResult = runParser s locateItemIdParser
  in
    ("locate item " <> _)
      <$> ( fromRight []
            $ ( \itemId ->
                  filter
                    ( \known -> contains (Pattern itemId) known
                    )
                    (toUnfoldable knownItems)
              )
            <$> itemIdParseResult
        )

contextCompleter :: Context -> (String -> Effect { matched :: String, completions :: Array String })
contextCompleter (RootContext { rootUrl, knownCollections }) = \s ->
  let
    strInCommand = filter (\cmd -> contains (Pattern s) cmd) (rootCommands rootUrl)

    collectionIds = collectionIdMatches knownCollections s
  in
    pure $ { matched: s, completions: strInCommand <> collectionIds }

contextCompleter (CollectionContext { knownCollections, knownItems }) = \s ->
  let
    strInCommand = filter (\cmd -> contains (Pattern s) cmd) collectionCommands

    collectionIds = collectionIdMatches knownCollections s

    itemIds = itemIdMatches knownItems s
  in
    pure $ { matched: s, completions: strInCommand <> collectionIds <> itemIds }
