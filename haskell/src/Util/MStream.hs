-- MStreams are just effectful coroutines

{-# LANGUAGE DeriveFunctor #-}

module Util.MStream where

import Control.Monad (join)
import Control.Monad.State (StateT)
import qualified Control.Monad.State as S
import System.IO (isEOF)

data MStream f y a =
    Ret a
  | Yield y (MStream f y a)
  | Act (f (MStream f y a))

deriving instance Functor f => Functor (MStream f y)

instance Monad f => Applicative (MStream f y) where
  pure = Ret
  Ret f <*> x = fmap f x
  Yield y f <*> x = Yield y (f <*> x)
  Act f <*> x = Act (fmap (<*> x) f)

instance Monad f => Monad (MStream f y) where
  Ret x >>= f  = f x
  Yield y x >>= f = Yield y (x >>= f)
  Act x >>= f = Act (fmap (>>= f) x)

liftM :: Functor m' => (forall z. m z -> m' z) -> MStream m a b -> MStream m' a b
liftM _ (Ret x) = Ret x
liftM f (Yield y x) = Yield y (liftM f x)
liftM f (Act x) = Act (liftM f <$> f x)

lift :: Functor m => m a -> MStream m b a
lift = Act . fmap Ret

smap :: Functor f => (a -> a') -> (b -> b') -> MStream f a b -> MStream f a' b'
smap f g (Ret x) = Ret (g x)
smap f g (Yield y x)= Yield (f y) (smap f g x)
smap f g (Act x) = Act (smap f g <$> x)

yield :: Monad m => a -> MStream m a ()
yield y = Yield y (Ret ())

listToStream :: Applicative f => [a] -> MStream f a ()
listToStream [] = Ret ()
listToStream (x : xs) = Yield x (listToStream xs)

streamIterate :: Monad m => s -> (s -> o -> m s) -> MStream m o () -> MStream m s s
streamIterate i f = go i where
  go s (Ret ()) = pure s
  go s (Yield y x) = do
    s' <- lift (f s y)
    Yield s' (go s' x)
  go s (Act x) = Act (go s <$> x)

runStream :: Monad m => (a -> m ()) -> MStream m a b -> m b
runStream act (Ret x) = pure x
runStream act (Yield y x) = act y >> runStream act x
runStream act (Act x) = x >>= runStream act

runState :: Monad m => s -> MStream (StateT s m) a b -> MStream m a (s, b)
runState st = go st where
  go s (Ret x) = Ret (s, x)
  go s (Yield y x) = Yield y (go s x)
  go s (Act x) = Act $ do
    (y, s') <- S.runStateT x s
    pure (go s' y)

stdinStream :: MStream IO Double ()
stdinStream = do
  eof <- lift isEOF
  if eof then return () else do
    x <- lift (read <$> getLine)
    yield x
    stdinStream

-- If everything in the list is a Left or Right,
-- convert to the list of an appropriate type, and
-- return `Nothing` if there is a mix.
allOneSide :: [Either b c] -> Maybe (Either [b] [c])
allOneSide [] = Just (Left [])
allOneSide [Left a] = Just (Left [a])
allOneSide [Right b] = Just (Right [b])
allOneSide (Left a : xs) = case allOneSide xs of
  Just (Left xs') -> Just (Left (a : xs'))
  _ -> Nothing
allOneSide (Right b : xs) = case allOneSide xs of
  Just (Right xs') -> Just (Right (b : xs'))
  _ -> Nothing

step :: Monad m => MStream m y a -> m (Either (y, MStream m y a) a)
step (Ret x) = pure (Right x)
step (Yield y x) = pure (Left (y, x))
step (Act x) = step =<< x

mapyieldM :: Monad m => (y -> m y') -> MStream m y a -> MStream m y' a
mapyieldM f (Ret x) = Ret x
mapyieldM f (Yield y x) =
  Act ((\y' -> Yield y' (mapyieldM f x)) <$> f y)
mapyieldM f (Act x) = Act (mapyieldM f <$> x)

stepUntil :: Monad m => (y -> Either y' ys) -> MStream m y a -> MStream m y' (Either (ys, MStream m y a) a)
stepUntil f (Ret x) = Ret (Right x)
stepUntil f (Yield y x) = case f y of
  Left y' -> Yield y' (stepUntil f x)
  Right ys -> Ret (Left (ys, x))
stepUntil f (Act x) = Act (stepUntil f <$> x)

merge :: Monad m => [MStream m y a] -> MStream m [y] [a]
merge xs = do
  results <- lift (mapM step xs)
  case allOneSide results of
    Just (Right as) -> pure as
    Just (Left bs) -> let (xs, fs) = unzip bs in
      Yield xs (merge fs)
    _ -> error "Streams not uniform"

mergeWith :: Monad m => ([(y, MStream m y a)] -> m [(y', MStream m y a)]) -> [MStream m y a] -> MStream m [y'] [a]
mergeWith f xs = do
  results <- lift (mapM step xs)
  case allOneSide results of
    Just (Right as) -> pure as
    Just (Left bs) -> do
      bs' <- lift (f bs)
      let (ys, ss) = unzip bs'
      Yield ys (mergeWith f ss)
    _ -> error "Streams not uniform"