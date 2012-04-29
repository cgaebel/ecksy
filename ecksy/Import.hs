module Import
    ( module Prelude
    , module Yesod
    , module Foundation
    , module Settings.StaticFiles
    , module Data.Char
    , module Data.Functor
    , module Data.Monoid
    , module Control.Applicative
    , module Control.Monad
    , module Control.Monad.Trans.Maybe
    , Text
    , (++)
    , map
#if __GLASGOW_HASKELL__ < 704
    , (<>)
#endif
    ) where

import Prelude hiding ( writeFile, readFile, head, tail, init, last, (++), map )
import Yesod   hiding (Route(..))
import Foundation
import Data.Char
import Data.Functor
import Data.Monoid ( Monoid (mappend, mempty, mconcat)
                   , (<>)
                   )
import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad
import Control.Monad.Trans.Maybe
import Data.Text (Text)
import Settings.StaticFiles

infixr 5 ++
(++) :: Monoid m => m -> m -> m
(++) = (<>)
{-# INLINE (++) #-}

map :: Functor f => (a -> b) -> f a -> f b
map = fmap
{-# INLINE map #-}

#if __GLASGOW_HASKELL__ < 704
infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
#endif
