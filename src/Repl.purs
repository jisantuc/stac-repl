module Repl (replProgram) where

import Affjax (printError)
import Client.Stac (getCollection, getCollections, getConformance)
import Command (getParser)
import Completions (getCompletions)
import Context (toCollectionContext, toRootContext, toRootUrl)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (mempty)
import Data.Set (fromFoldable)
import Data.Stac (Collection(..), CollectionsResponse(..))
import Data.String.NonEmpty (toString)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (error, log)
import Effect.Ref (Ref, modify_, new, read)
import Node.ReadLine (Interface, createConsoleInterface, prompt, setLineHandler, setPrompt)
import Prelude (Unit, bind, discard, pure, show, ($), (*>), (<$>), (<<<), (<>), (>>=))
import Printer (prettyPrintCollections, prettyPrintConformance)
import Text.Parsing.Parser (runParser)
import Types (Cmd(..), Context(..))

updateKnownCollections :: forall m. MonadEffect m => Ref Context -> Array Collection -> m Unit
updateKnownCollections ctxRef collections =
  let
    collectionIds = (\(Collection { id }) -> id) <$> collections
  in
    liftEffect
      $ do
          ctx <- read ctxRef
          validFor toRootContext ctx \{ rootUrl, knownCollections } ->
            modify_
              ( \_ ->
                  RootContext { rootUrl, knownCollections: knownCollections <> fromFoldable collectionIds }
              )
              ctxRef

validFor :: forall a m. MonadEffect m => (Context -> Maybe a) -> Context -> (a -> m Unit) -> m Unit
validFor predicate ctx continuation =
  let
    effect = continuation <$> predicate ctx

    fallback = log $ "Cannot do this action from " <> show ctx
  in
    fromMaybe fallback effect

execute :: Interface -> Ref Context -> Cmd -> Effect Unit
execute interface ctxRef cmd = do
  ( case cmd of
      GetCollection s -> do
        read ctxRef >>= log <<< show
        log $ toString s
      SetCollection s -> do
        ctx <- read ctxRef
        case ctx of
          RootContext { rootUrl: Just url } -> do
            log $ "Set context to collection " <> toString s
            modify_ (\_ -> CollectionContext { rootUrl: url, collectionId: s }) ctxRef
          RootContext { rootUrl: Nothing } -> log $ "Can't set collection context without a root url"
          CollectionContext { rootUrl } -> modify_ (\_ -> CollectionContext { rootUrl, collectionId: s }) ctxRef
      ViewCollection s -> do
        ctx <- read ctxRef
        validFor toCollectionContext ctx \{ rootUrl, collectionId } ->
          launchAff_ do
            collectionResp <- getCollection rootUrl collectionId
            case collectionResp of
              Right (Collection { id }) -> (log $ "Got collection id " <> id)
              Left err -> log $ "Could not fetch collection " <> toString collectionId <> ": " <> printError err
      UnsetCollection -> do
        modify_
          ( case _ of
              RootContext rec -> RootContext rec
              CollectionContext { rootUrl } -> RootContext { rootUrl: Just rootUrl, knownCollections: mempty }
          )
          ctxRef
        log "Returning to root context"
      SetRootUrl s -> do
        modify_
          ( case _ of
              CollectionContext { collectionId } -> CollectionContext { rootUrl: s, collectionId }
              RootContext _ -> RootContext { rootUrl: Just s, knownCollections: mempty }
          )
          ctxRef
        let
          newPrompt = "stac " <> s <> " > "
        setPrompt newPrompt interface
      LocateCollection -> do
        ctx <- read ctxRef
        case ctx of
          CollectionContext { rootUrl, collectionId } ->
            launchAff_
              $ do
                  response <- getCollection rootUrl collectionId
                  case response of
                    Left err -> error "lol"
                    Right (Collection { id }) -> log <<< show $ "\nGot collection " <> id
          _ -> error $ "Cannot locate a collection outside of a collection context: " <> show ctx
      ListCollections -> do
        ctx <- read ctxRef
        validFor toRootUrl ctx \rootUrl ->
          launchAff_
            $ do
                response <- getCollections rootUrl
                case response of
                  Left err -> log $ "Could not list collections for " <> rootUrl <> ": " <> printError err
                  Right resp@(CollectionsResponse { collections }) -> do
                    updateKnownCollections ctxRef collections
                    prettyPrintCollections resp
      GetConformance root -> do
        ctx <- read ctxRef
        validFor toRootUrl ctx \rootUrl ->
          launchAff_
            $ do
                response <- getConformance rootUrl
                case response of
                  Left err -> log $ "Could not get conformance for " <> rootUrl <> ": " <> printError err
                  Right conformance -> prettyPrintConformance conformance
  )
    *> prompt interface

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
  case Tuple s cmdParseResult of
    Tuple "" _ -> prompt interface
    Tuple _ (Left _) -> do
      log
        $ "I didn't recognize the command "
        <> s
        <> ". Try <TAB><TAB> to see available commands."
      prompt interface
    Tuple _ (Right cmd) -> execute interface ctxRef cmd

replProgram :: Effect Interface
replProgram = do
  ctxRef <- new $ RootContext { rootUrl: Nothing, knownCollections: mempty }
  interface <- createConsoleInterface $ getCompletions ctxRef
  setPrompt "stac > " interface
  setLineHandler (lineHandler ctxRef interface) interface
  pure $ interface
