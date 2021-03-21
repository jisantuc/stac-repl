module Repl (replProgram) where

import Affjax (printError)
import Ansi.Codes (Color(..))
import Ansi.Output (foreground, withGraphics)
import Client.Stac (getCollection, getCollectionItems, getCollections, getConformance, nextCollectionItemsPage)
import Command (getParser)
import Completions (getCompletions)
import Context (toCollectionContext, toRootContext, toRootUrl)
import Control.Promise (toAffE)
import Data.Array (uncons)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (mempty)
import Data.Set (fromFoldable)
import Data.Stac (Collection(..), CollectionItemsResponse, CollectionsResponse(..), Item(..), SpatialExtent, TwoDimBbox(..))
import Data.String.NonEmpty (toString)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (error, log)
import Effect.Ref (Ref, modify_, new, read)
import Mapping (Latitude(..), Longitude(..), drawMap)
import Node.ReadLine (Interface, createConsoleInterface, prompt, setLineHandler, setPrompt)
import Prelude (Unit, bind, discard, pure, show, ($), (-), (/), (<$>), (<>))
import Printer (prettyPrintCollection, prettyPrintCollections, prettyPrintConformance, prettyPrintItems)
import Text.Parsing.Parser (runParser)
import Types (Cmd(..), Context(..))

greenPrompt :: String -> String
greenPrompt = withGraphics (foreground Green)

updateKnownCollections ::
  forall m.
  MonadEffect m =>
  Ref Context ->
  Array Collection -> m Unit
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
                  RootContext
                    { rootUrl, knownCollections: knownCollections <> fromFoldable collectionIds
                    }
              )
              ctxRef

updateItemsResponse :: forall m. MonadEffect m => Ref Context -> CollectionItemsResponse -> m Unit
updateItemsResponse ctxRef itemsResponse =
  let
    itemIds = fromFoldable $ (\(Item { id }) -> id) <$> itemsResponse.features
  in
    liftEffect
      $ modify_
          ( case _ of
              CollectionContext rec ->
                CollectionContext
                  $ rec
                      { itemsResponse = itemsResponse
                      , knownItems = rec.knownItems <> mempty
                      }
              ctx -> ctx
          )
          ctxRef

collectionCentroid :: SpatialExtent -> Maybe (Tuple Latitude Longitude)
collectionCentroid { bbox } =
  ( \{ head: (TwoDimBbox { llx, lly, urx, ury }) } ->
      let
        latitude = Latitude $ (ury - lly) / 2.0

        longitude = Longitude $ (urx - llx) / 2.0
      in
        Tuple latitude longitude
  )
    <$> uncons bbox

validFor ::
  forall a m.
  MonadEffect m =>
  (Context -> Maybe a) -> Context -> (a -> m Unit) -> m Unit
validFor predicate ctx continuation =
  let
    effect = continuation <$> predicate ctx

    fallback = log $ "Cannot do this action from " <> show ctx
  in
    fromMaybe fallback effect

execute :: Interface -> Ref Context -> Cmd -> Effect Unit
execute interface ctxRef cmd = case cmd of
  SetCollection s -> do
    ctx <- read ctxRef
    case ctx of
      RootContext { rootUrl: Just url, knownCollections } -> do
        log $ "Set context to collection " <> toString s
        modify_
          ( \_ ->
              CollectionContext
                { rootUrl: url
                , collectionId: s
                , knownCollections
                , itemsResponse: { features: [], links: [] }
                , knownItems: mempty
                }
          )
          ctxRef
      RootContext { rootUrl: Nothing } -> log $ "Can't choose a collection without a root url"
      CollectionContext { rootUrl, knownCollections } -> do
        log $ "Set context to collection " <> toString s
        modify_
          ( \_ ->
              CollectionContext
                { rootUrl
                , collectionId: s
                , knownCollections
                , itemsResponse: { features: [], links: [] }
                , knownItems: mempty
                }
          )
          ctxRef
    prompt interface
  ViewCollection s -> do
    ctx <- read ctxRef
    validFor toCollectionContext ctx \{ rootUrl, collectionId } ->
      launchAff_ do
        collectionResp <- getCollection rootUrl collectionId
        case collectionResp of
          Right collection -> prettyPrintCollection collection
          Left err ->
            error
              $ "Could not fetch collection "
              <> toString collectionId
              <> ": "
              <> printError err
        liftEffect $ prompt interface
  UnsetCollection -> do
    modify_
      ( case _ of
          RootContext rec -> RootContext rec
          CollectionContext { rootUrl, knownCollections } ->
            RootContext
              { rootUrl: Just rootUrl, knownCollections
              }
      )
      ctxRef
    log "Returning to root context"
    prompt interface
  SetRootUrl s -> do
    modify_
      ( case _ of
          CollectionContext { collectionId } -> RootContext { rootUrl: Just s, knownCollections: mempty }
          RootContext _ -> RootContext { rootUrl: Just s, knownCollections: mempty }
      )
      ctxRef
    let
      newPrompt = greenPrompt $ "stac " <> s <> " > "
    setPrompt newPrompt interface
    prompt interface
  LocateCollection -> do
    ctx <- read ctxRef
    validFor toCollectionContext ctx \{ rootUrl, collectionId } ->
      launchAff_
        $ do
            response <- getCollection rootUrl collectionId
            case response of
              Left err ->
                error
                  $ "Could not fetch collection "
                  <> toString collectionId
                  <> ": "
                  <> printError err
              Right (Collection { id, extent: { spatial } }) ->
                let
                  centroid = collectionCentroid spatial
                in
                  fromMaybe (log $ "Collection " <> id <> " had no spatial extent")
                    $ (\(Tuple lat lon) -> toAffE $ drawMap lat lon)
                    <$> centroid
            liftEffect $ prompt interface
  ListCollections -> do
    ctx <- read ctxRef
    validFor toRootUrl ctx \rootUrl ->
      launchAff_
        $ do
            response <- getCollections rootUrl
            case response of
              Left err ->
                error
                  $ "Could not list collections for "
                  <> rootUrl
                  <> ": "
                  <> printError err
              Right resp@(CollectionsResponse { collections }) -> do
                updateKnownCollections ctxRef collections
                prettyPrintCollections resp
            liftEffect $ prompt interface
  GetConformance root -> do
    ctx <- read ctxRef
    validFor toRootUrl ctx \rootUrl ->
      launchAff_
        $ do
            response <- getConformance rootUrl
            case response of
              Left err -> error $ "Could not get conformance for " <> rootUrl <> ": " <> printError err
              Right conformance -> prettyPrintConformance conformance
            liftEffect $ prompt interface
  ListItems pageSize -> do
    ctx <- read ctxRef
    validFor toCollectionContext ctx \{ rootUrl, collectionId } ->
      launchAff_ do
        response <- getCollectionItems rootUrl collectionId
        case response of
          Left err ->
            error
              $ "Could not get items for "
              <> rootUrl
              <> " collection "
              <> toString collectionId
              <> ": "
              <> printError err
          Right resp@{ features } -> do
            updateItemsResponse ctxRef resp
            prettyPrintItems features
            liftEffect $ prompt interface
        liftEffect $ prompt interface
  NextItemsPage -> do
    ctx <- read ctxRef
    validFor toCollectionContext ctx \{ collectionId, itemsResponse } ->
      launchAff_ do
        response <- nextCollectionItemsPage itemsResponse
        case response of
          Left err ->
            error
              $ "Could not get next items page for "
              <> toString collectionId
              <> ": "
              <> printError err
          Right resp@{ features } -> do
            updateItemsResponse ctxRef resp
            prettyPrintItems features
            liftEffect $ prompt interface

lineHandler :: Ref Context -> Interface -> String -> Effect Unit
lineHandler ctxRef interface s = do
  parser <- getParser <$> read ctxRef
  let
    cmdParseResult = runParser s parser
  case Tuple s cmdParseResult of
    Tuple "" _ -> prompt interface
    Tuple _ (Left _) -> do
      error
        $ "I didn't recognize the command "
        <> withGraphics (foreground Red) s
        <> ". Try <TAB><TAB> to see available commands."
      prompt interface
    Tuple _ (Right cmd) -> execute interface ctxRef cmd

replProgram :: Effect Interface
replProgram = do
  ctxRef <- new $ RootContext { rootUrl: Nothing, knownCollections: mempty }
  interface <- createConsoleInterface $ getCompletions ctxRef
  setPrompt (greenPrompt "stac > ") interface
  setLineHandler (lineHandler ctxRef interface) interface
  pure $ interface
