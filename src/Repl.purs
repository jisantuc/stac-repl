module Repl (replProgram) where

import Node.ReadLine
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Prelude (Unit, bind, discard, pure, ($))
import Text.Parsing.Parser (Parser, parseErrorMessage, runParser)

data Context
  = RootContext
  | CollectionContext String

data Cmd
  = GetCollection String

type StringParser
  = Parser String

getContext :: Effect Context
getContext = pure RootContext

getParser :: Context -> StringParser Cmd
getParser _ = pure $ GetCollection "abcde"

execute :: Cmd -> Effect Unit
execute (GetCollection s) = log s

-- similar context handling to lineHandler happens here
getCompletions :: String -> Effect ({ matched :: String, completions :: Array String })
getCompletions s = do
  ctx <- getContext
  let
    completer = contextCompleter ctx
  pure $ completer s

contextCompleter :: Context -> (String -> { matched :: String, completions :: Array String })
contextCompleter _ = \s -> { matched: "", completions: [ "hi", "bye" ] }

-- what needs to happen in lineHandler?
-- need to:
-- * get current context (e.g. root, collection, whatever)
-- * obtain the parser for that context
-- * parse the string to the command for that context
-- * execute command (which might set a new context)
lineHandler :: Interface -> String -> Effect Unit
lineHandler interface s = do
  ctx <- getContext
  let
    parser = getParser ctx
  let
    cmdParseResult = runParser s parser
  case cmdParseResult of
    Left parseError -> log $ parseErrorMessage parseError
    Right cmd -> execute cmd
  prompt interface

replProgram :: Effect Interface
replProgram = do
  interface <- createConsoleInterface getCompletions
  setPrompt "stac > " interface
  setLineHandler (lineHandler interface) interface
  pure $ interface
