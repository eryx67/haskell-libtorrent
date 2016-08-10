-- | Monad lenses, stolen from <http://zrho.me/posts/2014-11-13-rays.html>

module Network.Libtorrent.H.Rays where

import           Control.Applicative   (Const (..))
import           Data.Functor.Identity (Identity (..))

type Ray m f t a b = (a -> f b) -> m (f t)


viewR :: Functor m => Ray m (Const a) t a b -> m a
viewR e = fmap getConst $ e Const

overR :: Functor m => Ray m Identity t a b -> (a -> b) -> m t
overR e f = fmap runIdentity $ e $ Identity . f

setR :: Functor m => Ray m Identity t a a -> m t
setR e = fmap runIdentity $ e $ Identity . id

infixr 4 >=^
(>=^) :: Functor m => Ray m (Const a) t a b -> m a
(>=^) = viewR

infixr 4 =<%~
(=<%~) :: Functor m => Ray m Identity t a b -> (a -> b) -> m t
(=<%~) = overR

infixr 4 =<~
(=<~) :: Functor m => Ray m Identity t a a -> m t
(=<~) = setR
