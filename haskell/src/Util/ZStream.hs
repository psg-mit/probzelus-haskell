{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecursiveDo #-}

module Util.ZStream (
  module Util.ZStream,
  module Control.Arrow
) where

import Control.Arrow
import Control.Category hiding ((.))
import qualified Control.Category as C
import Control.Monad.Fix
import qualified Control.Monad.Trans as MT

data ZStream f a b = ZStream { step :: a -> f (ZStream f a b, b) }

deriving instance Functor f => Functor (ZStream f a)

fromStep :: Functor f => (s -> a -> f (s, b)) -> s -> ZStream f a b
fromStep s initState = ZStream $ \a -> first (fromStep s) <$> s initState a

lift :: Monad m => Functor (t m) => MT.MonadTrans t => ZStream m a b -> ZStream (t m) a b
lift = liftM MT.lift

liftM :: Functor m' => (forall z. m z -> m' z) -> ZStream m a b -> ZStream m' a b
liftM f = fromStep (\(ZStream s) a -> f (s a))

zconstM :: Functor f => (a -> f b) -> ZStream f a b
zconstM f = ZStream $ \a -> (\x -> (zconstM f, x)) <$> f a

zconst :: Applicative f => (a -> b) -> ZStream f a b
zconst f = zconstM (pure . f)

delay :: Applicative f => a -> ZStream f a a
delay = fromStep (\x a -> pure (a, x))

zcons :: Functor f => (a -> f b) -> ZStream f a b -> ZStream f a b
zcons f s = ZStream $ \a -> (\b -> (s, b)) <$> f a

fby :: Functor f => (a -> f b) -> ZStream f a b -> ZStream f a b
fby = zcons

run :: Monad m => ZStream m (m a) a
run = zconstM Prelude.id

rightArrow :: Applicative m => ZStream m (a, a) a
rightArrow = ZStream $ \(x, _) -> pure (zconst snd, x)

rightArrow1 :: Monad f => (a -> f b) -> ZStream f a b -> ZStream f a b
rightArrow1 f (ZStream s) = ZStream $ \a -> do
  b <- f a
  (s', _) <- s a
  pure (s', b)

runStream :: Monad m => (a -> m ()) -> ZStream m () a -> m ()
runStream act (ZStream s) = do
  (s', a) <- s ()
  act a
  runStream act s'

compose :: Monad m => ZStream m a b -> ZStream m b c -> ZStream m a c
compose (ZStream ab) (ZStream bc) = ZStream $ \a -> do
  (ab', b) <- ab a
  (bc', c) <- bc b
  pure (compose ab' bc', c)

zcombine :: Applicative m => ZStream m a b -> ZStream m a' b' -> ZStream m (a, a') (b, b')
zcombine (ZStream f) (ZStream f') = ZStream $ \(a, a') ->
  combine <$> f a <*> f' a'
  where
  combine (fnew, b) (f'new, b') = (zcombine fnew f'new, (b, b'))

-- zabstract (the "converse") is not possible
-- so ZStream  is not Cartesian closed
zapply :: Functor m => ZStream m gamma (a -> b) -> ZStream m (gamma, a) b
zapply (ZStream f) = ZStream $ \(g, x) -> (\(f', k) -> (zapply f', k x)) <$> f g

present :: Functor m => ZStream m a b -> ZStream m a b -> ZStream m (Bool, a) b
present (ZStream t) (ZStream f) = ZStream $ \(b, x) ->
  if b
    then (\(t', y) -> (present t' (ZStream f), y)) <$> t x
    else (\(f', y) -> (present (ZStream t) f', y)) <$> f x

init :: Applicative m => (a -> m b) -> ZStream m a b
init f = ZStream $ \x -> (\y -> (zconst (const y), y)) <$> f x


instance Monad m => Category (ZStream m) where
  id = zconst Prelude.id
  (.) = flip compose

instance Monad m => Arrow (ZStream m) where
  arr = zconst
  (***) = zcombine

instance MonadFix m => ArrowLoop (ZStream m) where
  loop (ZStream f) = ZStream $ \x -> mdo
    (str', (y, s)) <- f (x, s)
    pure (loop str', y)

instance Applicative m => Applicative (ZStream m a) where
  pure = zconst . const
  ZStream f <*> ZStream x = ZStream $ \a -> combine <$> f a <*> x a
    where combine (sf', f') (sx', x') = (sf' <*> sx', f' x')