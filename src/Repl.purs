module Repl (replProgram) where

import Client.Stac (getCollection)
import Control.Alt ((<|>))
import Data.Array (filter)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.List (List, toUnfoldable)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Stac (Collection(..))
import Data.String (Pattern(..))
import Data.String.CodeUnits (contains, fromCharArray)
import Data.String.NonEmpty (NonEmptyString, fromString, toString)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (error, log)
import Effect.Ref (Ref, modify_, new, read)
import Node.ReadLine (Interface, createConsoleInterface, prompt, setLineHandler, setPrompt)
import Prelude (class Show, Unit, bind, discard, pure, show, ($), (*>), (<$), (<$>), (<<<), (<>), (>>=))
import Text.Parsing.Parser (Parser, fail, parseErrorMessage, runParser)
import Text.Parsing.Parser.Combinators (manyTill)
import Text.Parsing.Parser.String (anyChar, eof, skipSpaces, string)

type RootUrl
  = String

data Context
  = RootContext (Maybe RootUrl)
  | CollectionContext RootUrl NonEmptyString

derive instance genericContext :: Generic Context _

instance showContext :: Show Context where
  show = genericShow

data Cmd
  = GetCollection NonEmptyString
  | SetCollection NonEmptyString
  | ViewCollection NonEmptyString
  | LocateCollection
  | SetRootUrl RootUrl
  | UnsetCollection

type StringParser
  = Parser String

fromList :: forall a. List a -> Array a
fromList = toUnfoldable

getParser :: Context -> StringParser Cmd
getParser ctx =
  let
    setCollectionParser =
      SetCollection
        <$> ( fromCharArray <<< fromList <$> (string "set collection" *> skipSpaces *> manyTill anyChar eof)
              >>= \s -> case fromString s of
                  Just ne -> pure ne
                  Nothing -> fail "Cannot set an empty string as collection"
          )

    setRootUrlParser = SetRootUrl <<< fromCharArray <<< fromList <$> (string "set root url" *> skipSpaces *> manyTill anyChar eof)

    locateCollectionParser = \collectionId -> LocateCollection <$ (string "locate" *> skipSpaces *> eof)
  in
    case ctx of
      RootContext _ -> setCollectionParser <|> setRootUrlParser
      CollectionContext _ s ->
        ViewCollection s <$ (string "view" *> skipSpaces *> eof)
          <|> UnsetCollection
          <$ (string "unset collection" *> skipSpaces *> eof)
          <|> setCollectionParser
          <|> setRootUrlParser
          <|> locateCollectionParser s

execute :: Interface -> Ref Context -> Cmd -> Effect Unit
execute interface ctxRef cmd = do
  case cmd of
    GetCollection s -> do
      read ctxRef >>= log <<< show
      log $ toString s
    SetCollection s -> do
      ctx <- read ctxRef
      case ctx of
        RootContext (Just rootUrl) -> do
          log $ "Set context to collection " <> toString s
          modify_ (\_ -> CollectionContext rootUrl s) ctxRef
        RootContext Nothing -> log $ "Can't set collection context without a root url"
        CollectionContext rootUrl _ -> modify_ (\_ -> CollectionContext rootUrl s) ctxRef
    ViewCollection s -> do
      read ctxRef >>= log <<< show
      log $ toString s
    UnsetCollection -> do
      modify_
        ( case _ of
            RootContext url -> RootContext url
            CollectionContext url _ -> RootContext (Just url)
        )
        ctxRef
      log "Returning to root context"
    SetRootUrl s -> do
      modify_
        ( case _ of
            CollectionContext _ coll -> CollectionContext s coll
            RootContext _ -> RootContext (Just s)
        )
        ctxRef
      let
        newPrompt = "stac " <> s <> " > "
      setPrompt newPrompt interface
    LocateCollection -> do
      ctx <- read ctxRef
      case ctx of
        CollectionContext url collectionId ->
          launchAff_
            $ do
                response <- getCollection url collectionId
                case response of
                  Left err -> error "lol"
                  Right (Collection { id }) -> log <<< show $ "Got collection " <> id
        _ -> error $ "Cannot locate a collection outside of a collection context: " <> show ctx

-- similar context handling to lineHandler happens here
getCompletions :: Ref Context -> String -> Effect ({ matched :: String, completions :: Array String })
getCompletions ctxRef s = do
  ctx <- read ctxRef
  contextCompleter ctx $ s

collectionCommands :: Array String
collectionCommands = [ "view", "unset collection", "locate" ]

contextCompleter :: Context -> (String -> Effect { matched :: String, completions :: Array String })
contextCompleter (RootContext _) = \s -> pure { matched: "", completions: [ "hi", "bye" ] }

contextCompleter (CollectionContext url collectionId) = \s ->
  let
    strInCommand = filter (\cmd -> contains (Pattern s) cmd) collectionCommands
  in
    pure $ { matched: s, completions: strInCommand }

-- what needs to happen in lineHandler?
-- need to:
-- * get current context (e.g. root, collection, whatever)
-- * obtain the parser for that context
-- * parse the string to the command for that context
-- * execute command (which might set a new context)
lineHandler :: Ref Context -> Interface -> String -> Effect Unit
lineHandler ctxRef interface s = do
  parser <- getParser <$> read ctxRef
  let
    cmdParseResult = runParser s parser
  case cmdParseResult of
    Left parseError -> log $ parseErrorMessage parseError
    Right cmd -> execute interface ctxRef cmd
  prompt interface

replProgram :: Effect Interface
replProgram = do
  ctxRef <- new $ RootContext Nothing
  interface <- createConsoleInterface $ getCompletions ctxRef
  setPrompt "stac > " interface
  setLineHandler (lineHandler ctxRef interface) interface
  pure $ interface
