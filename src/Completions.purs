module Completions (getCompletions) where

import Command (collectionIdParser)
import Data.Array (filter)
import Data.Either (fromRight)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe)
import Data.Set (toUnfoldable)
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
collectionCommands = [ "view", "unset collection", "locate", "get conformance" ]

rootCommands :: Maybe RootUrl -> Array String
rootCommands rootUrlM =
  [ "set root url" ]
    <> foldMap
        ( const [ "get conformance", "set collection", "list collections" ]
        )
        rootUrlM

contextCompleter :: Context -> (String -> Effect { matched :: String, completions :: Array String })
contextCompleter (RootContext { rootUrl, knownCollections }) = \s ->
  let
    strInCommand = filter (\cmd -> contains (Pattern s) cmd) (rootCommands rootUrl)

    collectionIdParseResult = runParser s collectionIdParser

    -- the _match_ needs to be a prefix of the completion to make the magic tab
    -- complete "advance to longest matched point" behavior work.
    -- mapping "set collection " onto the beginning of the collection IDs that
    -- we know about ensures the match.
    -- this is brittle -- it only works because we _know_ in this case that
    -- collectionIdParser relies on that string as a leader.
    -- best case would be wrap the parser in some kind of data type like
    -- data CmdParser = { leaderString :: String, parser :: Parser Cmd }
    -- that would tie this information to the parser, but I haven't thought about
    -- this much yet. For now, I'm acknowledging that it's brittle and moving on.
    collectionIdMatches =
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
  in
    pure $ { matched: s, completions: strInCommand <> collectionIdMatches }

contextCompleter (CollectionContext _) = \s ->
  let
    strInCommand = filter (\cmd -> contains (Pattern s) cmd) collectionCommands
  in
    pure $ { matched: s, completions: strInCommand }
