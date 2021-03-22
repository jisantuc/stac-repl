module Command (getParser, collectionIdParser, locateItemIdParser) where

import Control.Alt ((<|>))
import Control.Apply ((*>), (<$))
import Data.Array as Array
import Data.CodePoint.Unicode (hexDigitToInt)
import Data.List (List, some, toUnfoldable)
import Data.Maybe (Maybe(..), maybe)
import Data.String (codePointFromChar)
import Data.String.CodeUnits (fromCharArray)
import Data.String.NonEmpty (fromString)
import Prelude (bind, pure, ($), (*), (+), (<$>), (<*), (<<<), (>>=))
import Text.Parsing.Parser (fail)
import Text.Parsing.Parser.Combinators (manyTill)
import Text.Parsing.Parser.String (anyChar, eof, skipSpaces, string)
import Text.Parsing.Parser.Token (digit)
import Types (Cmd(..), Context(..), RootUrl, StringParser)

-- more or less ripped off from
-- https://github.com/purescript-contrib/purescript-parsing/blob/v6.0.0/src/Text/Parsing/Parser/Token.purs#L598-L605
-- trying to create the object containing all the special parsers
-- was failing to compile with a kind unification error 🤔
decimal :: StringParser Int
decimal = do
  digits <- Array.some digit
  ( maybe (fail "not digits") pure
      $ Array.foldl folder (Just 0) digits
  )
  where
  base = 10

  folder :: Maybe Int -> Char -> Maybe Int
  folder Nothing _ = Nothing

  folder (Just x) d = ((base * x) + _) <$> hexDigitToInt (codePointFromChar d)

fromList :: forall a. List a -> Array a
fromList = toUnfoldable

terminalIdParser :: String -> StringParser String
terminalIdParser s = fromCharArray <<< fromList <$> (string s *> skipSpaces *> manyTill anyChar eof)

collectionIdParser :: StringParser String
collectionIdParser = terminalIdParser "set collection"

locateItemIdParser :: StringParser String
locateItemIdParser = terminalIdParser "locate item"

setCollectionParser :: StringParser Cmd
setCollectionParser =
  SetCollection
    <$> ( collectionIdParser
          >>= \s -> case fromString s of
              Just ne -> pure ne
              Nothing -> fail "Cannot set an empty string as collection"
      )

setRootUrlParser :: StringParser Cmd
setRootUrlParser = SetRootUrl <<< fromCharArray <<< fromList <$> (string "set root url" *> skipSpaces *> some anyChar)

locateCollectionParser :: StringParser Cmd
locateCollectionParser = LocateCollection <$ (string "locate collection" *> skipSpaces *> eof)

listCollectionsParser :: StringParser Cmd
listCollectionsParser = ListCollections <$ (string "list collections" *> skipSpaces *> manyTill anyChar eof)

getConformanceParser :: RootUrl -> StringParser Cmd
getConformanceParser root = GetConformance root <$ (string "get conformance" *> skipSpaces *> eof)

listItemsParser :: StringParser Cmd
listItemsParser =
  ListItems 10 <$ (string "list items")
    <|> ( ListItems
          <$> (string "list" *> skipSpaces *> decimal <* skipSpaces <* string "items")
      )

nextPageParser :: StringParser Cmd
nextPageParser = NextItemsPage <$ string "next page"

locateItemParser :: StringParser Cmd
locateItemParser =
  LocateItem
    <$> ( locateItemIdParser
          >>= \s -> case fromString s of
              Just ne -> pure ne
              Nothing -> fail "Cannot set an empty string as item id to locate"
      )

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
      <|> getConformanceParser rootUrl
      <|> listItemsParser
      <|> nextPageParser
      <|> locateCollectionParser
      <|> locateItemParser
