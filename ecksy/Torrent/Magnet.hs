module Torrent.Magnet ( MagnetLink
                      , InfoHash
                      , magnetInfoHash
                      ) where

import Control.Applicative
import Data.Attoparsec.Text as A
import Data.Function
import Data.Maybe
import Data.Text

import Prelude ( (>>=)
               , (>=)
               , (<=)
               , (||)
               , (&&)
               , return
               , Char
               , Bool
               )

type MagnetLink = Text
type InfoHash   = Text

magnetInfoHash :: MagnetLink -> Maybe InfoHash
magnetInfoHash = (toLower <$>) . maybeResult' . A.parse infoHashParser
    where
        infoHashParser :: A.Parser InfoHash
        infoHashParser = do _ <- A.stringCI "magnet:?xt=urn:btih:"
                            ihash <- A.takeWhile isHexDigit
                            -- a magnet link has other shit, but we don't care.
                            _ <- A.takeLazyText
                            return $ toLower ihash

        -- Same as A.maybeResult, but feeds an empty string into partial results.
        maybeResult' :: A.Result r -> Maybe r
        maybeResult' (A.Fail _ _ _) = Nothing
        maybeResult' (A.Partial f)  = maybeResult $ f ""
        maybeResult' (A.Done _ r)   = Just r

        isHexDigit :: Char -> Bool
        isHexDigit c = (c >= '0' && c <= '9') ||
                       (c >= 'a' && c <= 'f') ||
                       (c >= 'A' && c <= 'F')
