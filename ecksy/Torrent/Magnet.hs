{-# LANGUAGE TupleSections #-}
module Torrent.Magnet ( MagnetLink
                      , InfoHash
                      , DisplayName
                      , TrackerURL
                      , parseMagnetLink
                      ) where

import Control.Applicative
import Data.Attoparsec.Text as A
import Data.Char
import Data.Function
import qualified Data.HashSet as S
import Data.List as L
import Data.Maybe
import Data.Text as T

import Prelude ( (>>=)
               , (==)
               , (>=)
               , (<=)
               , (||)
               , (&&)
               , return
               , Char
               , Bool
               , Int
               , String
               , Num(..)
               , otherwise
               , not
               )

type MagnetLink  = Text

type InfoHash    = Text
type DisplayName = Text
type TrackerURL  = Text

-- | Only uses the first two characters.
unhex :: String -> Int
unhex [x,y] = x' * 0x10 + y'
    where (x', y') = (ord x, ord y)
unhex _     = 0

-- | Percent-encoded reserved characters.
reservedIfEncoded :: S.HashSet Char
reservedIfEncoded = S.fromList "!#$&'()*,/:;=?@[]"
{-# NOINLINE reservedIfEncoded #-}

-- | Returns Nothing on a failed parse.
parseMagnetLink :: MagnetLink -> Maybe (InfoHash, Maybe DisplayName, [TrackerURL])
parseMagnetLink = maybeResult' . A.parse parser
    where
        -- The format of the magnet URI is:
        -- magnet:?xt=urn:btih: Base32 encoded info-hash [ &dn= name of download ] [ &tr= tracker URL ]*
        parser :: A.Parser (InfoHash, Maybe DisplayName, [TrackerURL])
        parser = do _     <- A.stringCI "magnet:?xt=urn:btih:"
                    (,,) <$> infoHashParser
                         <*> optional (decodeURIComponent "dn")
                         <*> many     (decodeURIComponent "tr")

        infoHashParser :: A.Parser InfoHash
        infoHashParser = T.toLower <$> A.takeWhile isHexDigit

        decodeURIComponent :: Text -> A.Parser Text
        decodeURIComponent compName = char '&'
                                   *> string compName
                                   *> char '='
                                   *> (uriDecode <$> takeWhile1 validEncodedChar)

        uriDecode :: Text -> Text
        uriDecode = pack . run . unpack
            where
                run :: String -> String
                run [] = []
                run [x] = [x]
                run (x:xs) | x == '%'   = (chr . unhex $ L.take 2 xs) : run (L.drop 2 xs)
                           | x == '+'  = ' ': run xs
                           | otherwise = x: run xs

        validEncodedChar :: Char -> Bool
        validEncodedChar c = isAscii c
                          && (not $ S.member c reservedIfEncoded)

        -- Same as A.maybeResult, but feeds an empty string into partial results.
        maybeResult' :: A.Result r -> Maybe r
        maybeResult' (A.Fail _ _ _) = Nothing
        maybeResult' (A.Partial f)  = maybeResult $ f ""
        maybeResult' (A.Done _ r)   = Just r
