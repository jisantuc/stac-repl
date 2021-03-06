module Repl (replProgram) where

import Node.ReadLine
import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.List (List, toUnfoldable)
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits (fromCharArray)
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Ref (Ref, modify_, new, read)
import Prelude (class Show, Unit, bind, discard, pure, show, ($), (*>), (<$), (<$>), (<<<), (<>), (>>=))
import Text.Parsing.Parser (Parser, parseErrorMessage, runParser)
import Text.Parsing.Parser.Combinators (manyTill)
import Text.Parsing.Parser.String (anyChar, eof, skipSpaces, string)

data Context
  = RootContext
  | CollectionContext String

derive instance genericContext :: Generic Context _

instance showContext :: Show Context where
  show = genericShow

data Cmd
  = GetCollection String
  | SetCollection String
  | ViewCollection String
  | UnsetCollection

type StringParser
  = Parser String

getContext :: Effect (Ref Context)
getContext = new RootContext

fromList :: forall a. List a -> Array a
fromList = toUnfoldable

getParser :: Context -> StringParser Cmd
getParser ctx =
  let
    setCollectionParser = SetCollection <<< fromCharArray <<< fromList <$> (string "set collection" *> skipSpaces *> manyTill anyChar eof)
  in
    case ctx of
      RootContext -> setCollectionParser
      CollectionContext s ->
        ViewCollection s <$ (string "view" *> skipSpaces *> eof)
          <|> UnsetCollection
          <$ (string "unset collection" *> skipSpaces *> eof)
          <|> setCollectionParser

execute :: Ref Context -> Cmd -> Effect Unit
execute ctxRef cmd = case cmd of
  GetCollection s -> do
    read ctxRef >>= log <<< show
    log s
  SetCollection s -> do
    read ctxRef >>= log <<< show
    modify_ (\_ -> CollectionContext s) ctxRef
    log $ "Set context to collection " <> s
  ViewCollection s -> do
    read ctxRef >>= log <<< show
    log s
  UnsetCollection -> do
    modify_ (\_ -> RootContext) ctxRef
    log "Returning to root context"

-- similar context handling to lineHandler happens here
getCompletions :: Context -> String -> Effect ({ matched :: String, completions :: Array String })
getCompletions ctx s = do
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
lineHandler :: Ref Context -> Interface -> String -> Effect Unit
lineHandler ctxRef interface s = do
  parser <- case s of
    "set collection" -> pure <<< pure $ SetCollection s
    _ -> getParser <$> read ctxRef
  let
    cmdParseResult = runParser s parser
  case cmdParseResult of
    Left parseError -> log $ parseErrorMessage parseError
    Right cmd -> execute ctxRef cmd
  prompt interface

replProgram :: Effect Interface
replProgram = do
  ctxRef <- getContext
  currentContext <- read ctxRef
  interface <- createConsoleInterface $ getCompletions currentContext
  setPrompt "stac > " interface
  setLineHandler (lineHandler ctxRef interface) interface
  pure $ interface
