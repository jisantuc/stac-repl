module Command (getParser, collectionIdParser) where

import Control.Alt ((<|>))
import Control.Apply ((*>), (<$))
import Data.List (List, toUnfoldable)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (fromCharArray)
import Data.String.NonEmpty (fromString)
import Prelude (pure, (<$>), (<<<), (>>=))
import Text.Parsing.Parser (fail)
import Text.Parsing.Parser.Combinators (manyTill)
import Text.Parsing.Parser.String (anyChar, eof, skipSpaces, string)
import Types (Cmd(..), Context(..), StringParser, RootUrl)

fromList :: forall a. List a -> Array a
fromList = toUnfoldable

collectionIdParser :: StringParser String
collectionIdParser = fromCharArray <<< fromList <$> (string "set collection" *> skipSpaces *> manyTill anyChar eof)

setCollectionParser :: StringParser Cmd
setCollectionParser =
  SetCollection
    <$> ( collectionIdParser
          >>= \s -> case fromString s of
              Just ne -> pure ne
              Nothing -> fail "Cannot set an empty string as collection"
      )

setRootUrlParser :: StringParser Cmd
setRootUrlParser = SetRootUrl <<< fromCharArray <<< fromList <$> (string "set root url" *> skipSpaces *> manyTill anyChar eof)

locateCollectionParser :: StringParser Cmd
locateCollectionParser = LocateCollection <$ (string "locate" *> skipSpaces *> eof)

listCollectionsParser :: StringParser Cmd
listCollectionsParser = ListCollections <$ (string "list collections" *> skipSpaces *> manyTill anyChar eof)

getConformanceParser :: RootUrl -> StringParser Cmd
getConformanceParser root = GetConformance root <$ (string "get conformance" *> skipSpaces *> eof)

getParser :: Context -> StringParser Cmd
getParser ctx = case ctx of
  RootContext { rootUrl: Nothing } -> setRootUrlParser
  RootContext { rootUrl: Just url } ->
    setCollectionParser
      <|> setRootUrlParser
      <|> listCollectionsParser
      <|> getConformanceParser url
  CollectionContext { collectionId, rootUrl } ->
    ViewCollection collectionId <$ (string "view" *> skipSpaces *> eof)
      <|> UnsetCollection
      <$ (string "unset collection" *> skipSpaces *> eof)
      <|> setCollectionParser
      <|> setRootUrlParser
      <|> locateCollectionParser
      <|> getConformanceParser rootUrl
