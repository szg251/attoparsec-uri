{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Data.URI.ByteString where

import Control.Applicative (optional, (<|>))
import Control.Monad (void, when)
import Data.Attoparsec.ByteString (Parser, sepBy, string, takeWhile, takeWhile1, (<?>))
import Data.Attoparsec.ByteString.Char8 (char)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Internal (c2w)
import Data.Data (Typeable)
import Data.Strict.Maybe (Maybe (..), maybe)
import Data.Strict.Tuple (Pair (..))
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word8 (isControl, isSpace)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Gen (elements, listOf, listOf1, oneof)
import Test.QuickCheck.Instances ()
import Prelude hiding (Maybe (..), maybe, takeWhile)
import qualified Prelude as P

data URI = URI
  { uriScheme :: !(Maybe ByteString)
  -- ^ the scheme without the colon - @https://hackage.haskell.org/@ has a scheme of @https@
  , uriSlashes :: !Bool
  -- ^ are the slashes present? - @https://hackage.haskell.org/@ is @True@
  , uriPath :: !(Maybe (Vector ByteString))
  -- ^ slash-separated list - @https://hackage.haskell.org/foo@ is @["foo"]@
  , uriQuery :: !(Vector (Pair ByteString (Maybe ByteString)))
  -- ^ list of key-value pairs - @https://hackage.haskell.org/?foo=bar&baz&qux=@ is
  -- @[("foo", Just "bar"), ("baz", Nothing), ("qux", Just "")]@
  , uriFragment :: !(Maybe ByteString)
  -- ^ uri suffix - @https://hackage.haskell.org/#some-header@ is @Just "some-header"@
  }
  deriving (Show, Eq, Typeable, Generic)

instance Arbitrary URI where
  arbitrary =
    URI
      <$> arbitraryScheme
      <*> arbitrary
      <*> arbitraryPath
      <*> arbitraryQuery
      <*> arbitraryScheme
   where
    arbitraryScheme = oneof [pure Nothing, Just <$> arbitraryNonEmptyBS]
    arbitraryNonEmptyBS = BS.pack <$> listOf1 (elements (map c2w ['a' .. 'z']))
    arbitraryPath =
      oneof [pure Nothing, Just . V.fromList <$> listOf1 arbitraryNonEmptyBS]
    arbitraryQuery =
      V.fromList <$> listOf go
     where
      go = do
        a <- arbitraryNonEmptyBS
        mb <- oneof [pure Nothing, Just <$> arbitraryNonEmptyBS]
        pure (a :!: mb)

printURI :: URI -> ByteString
printURI URI{..} =
  maybe "" (<> ":") uriScheme
    <> (if uriSlashes then "//" else "")
    <> ( case uriPath of
          Just xs -> "/" <> BS.intercalate "/" (V.toList xs)
          Nothing -> ""
       )
    <> ( if null uriQuery
          then ""
          else
            "?"
              <> BS.intercalate
                "&"
                ( V.toList $
                    ( \(k :!: mV) ->
                        let v' = case mV of
                              Nothing -> ""
                              Just v -> "=" <> v
                         in k <> v'
                    )
                      <$> uriQuery
                )
       )
    <> case uriFragment of
      Nothing -> ""
      Just f -> "#" <> f

parseURI :: Parser URI
parseURI =
  URI
    <$> (toStrictMaybe <$> optional parseScheme)
    <*> parseSlashes
    <*> parsePath
    <*> parseQuery
    <*> (toStrictMaybe <$> optional parseFragment)
 where
  parseScheme :: Parser ByteString
  parseScheme = do
    sch <- takeWhile1 (\c -> c `notElem` (map c2w [':', '/', '@', '.', '[', '*'])) <?> "scheme value"
    when (sch == "localhost") (fail "can't be localhost")
    void (char ':') <?> "scheme colon"
    pure sch
  parseSlashes :: Parser Bool
  parseSlashes = do
    mS <- optional (string "//") <?> "slashes"
    case mS of
      P.Nothing -> pure False
      P.Just _ -> pure True
  parsePath :: Parser (Maybe (Vector ByteString))
  parsePath =
    let withRoot = do
          void (char '/') <?> "root"
          (Just . V.fromList <$> parseChunkWithout ['/', '?', '=', '&', '#'] `sepBy` char '/') <?> "path"
        withoutRoot = pure Nothing <?> "empty path"
     in withRoot <|> withoutRoot
  parseQuery :: Parser (Vector (Pair ByteString (Maybe ByteString)))
  parseQuery =
    ( do
        void (char '?') <?> "uri query init"
        let parse1 = do
              k <- parseChunkWithout ['=', '&', '#'] <?> "uri query key"
              mV <-
                ( Just <$> do
                    void (char '=') <?> "uri query sep"
                    parseChunkWithout ['&', '#'] <?> "uri query val"
                  )
                  <|> pure Nothing
              pure (k :!: mV)
        qs <- parse1 `sepBy` char '&' <?> "query params"
        pure (V.fromList qs)
    )
      <|> pure V.empty
  parseFragment :: Parser ByteString
  parseFragment = do
    void (char '#') <?> "fragment init"
    parseChunkWithout [] <?> "fragment value"
  parseChunkWithout :: [Char] -> Parser ByteString
  parseChunkWithout xs =
    takeWhile (\c -> not (isControl c || isSpace c) && c `notElem` (map c2w xs))

  toStrictMaybe P.Nothing = Nothing
  toStrictMaybe (P.Just x) = Just x
