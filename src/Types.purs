module Types where

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Set (Set)
import Data.Show.Generic (genericShow)
import Data.String.NonEmpty (NonEmptyString)
import Prelude (class Show)
import Text.Parsing.Parser (Parser)

type RootUrl
  = String

type CollectionId
  = String

type CollectionContextRecord
  = { rootUrl :: RootUrl, collectionId :: NonEmptyString }

type RootContextRecord
  = { rootUrl :: Maybe RootUrl, knownCollections :: Set CollectionId }

data Context
  = RootContext RootContextRecord
  | CollectionContext CollectionContextRecord

derive instance genericContext :: Generic Context _

instance showContext :: Show Context where
  show = genericShow

data Cmd
  = GetCollection NonEmptyString
  | SetCollection NonEmptyString
  | ListCollections
  | ViewCollection NonEmptyString
  | LocateCollection
  | SetRootUrl RootUrl
  | UnsetCollection

type StringParser
  = Parser String
