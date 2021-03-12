module Completions where

import Command (collectionIdParser)
import Data.Array (filter)
import Data.Either (fromRight)
import Data.Set (toUnfoldable)
import Data.String (Pattern(..), contains)
import Effect (Effect)
import Effect.Ref (Ref, read)
import Prelude (bind, pure, ($), (<$>), (<>))
import Text.Parsing.Parser (runParser)
import Types (Context(..))

getCompletions :: Ref Context -> String -> Effect ({ matched :: String, completions :: Array String })
getCompletions ctxRef s = do
  ctx <- read ctxRef
  contextCompleter ctx $ s

collectionCommands :: Array String
collectionCommands = [ "view", "unset collection", "locate" ]

rootCommands :: Array String
rootCommands = [ "set root url", "set collection", "list collections" ]

contextCompleter :: Context -> (String -> Effect { matched :: String, completions :: Array String })
contextCompleter (RootContext { knownCollections }) = \s ->
  let
    strInCommand = filter (\cmd -> contains (Pattern s) cmd) rootCommands

    collectionIdParseResult = runParser s collectionIdParser

    collectionIdMatches =
      fromRight []
        $ (\collectionId -> filter (\known -> contains (Pattern collectionId) known) (toUnfoldable knownCollections))
        <$> collectionIdParseResult
  in
    pure $ { matched: s, completions: strInCommand <> collectionIdMatches }

contextCompleter (CollectionContext _) = \s ->
  let
    strInCommand = filter (\cmd -> contains (Pattern s) cmd) collectionCommands
  in
    pure $ { matched: s, completions: strInCommand }
