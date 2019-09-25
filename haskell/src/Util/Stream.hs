module Util.Stream where

import Control.Monad (join)
import System.IO (isEOF)

data Stream f a b = a :< f (Stream f a b) | NoMore b

data Void

instance Functor f => Functor (Stream f a) where
  fmap f (NoMore x) = NoMore (f x)
  fmap f (x :< m) = x :< fmap (fmap f) m

instance Monad f => Applicative (Stream f a) where
  pure = NoMore
  f <*> x = do
    x' <- x
    f' <- f
    return (f' x')

instance Monad m => Monad (Stream m a) where
  NoMore x >>= f = f x
  (x :< f) >>= g = x :< (do {f' <- f; return (f' >>= g)})

newtype MStream f a b = MStream {unMStream :: f (Stream f a b) }

instance Monad f => Functor (MStream f a) where
  fmap f (MStream x) = MStream (fmap (fmap f) x)

instance Monad f => Applicative (MStream f a) where
  pure x = MStream (pure (pure x))
  f <*> x = do
    x' <- x
    f' <- f
    return (f' x')

instance Monad f => Monad (MStream f a) where
  MStream x >>= f = MStream $ do
    x' <- x
    case x' of
      NoMore x'' -> let MStream y = f x'' in y
      (x'' :< m) -> let MStream m' = MStream m >>= f in
        pure (x'' :< m')

liftM :: Functor m' => (forall z. m z -> m' z) -> Stream m a b -> Stream m' a b
liftM _ (NoMore b) = NoMore b
liftM f (x :< y) = x :< fmap (liftM f) (f y)

mliftM :: (Functor m, Functor m') => (forall z. m z -> m' z) -> MStream m a b -> MStream m' a b
mliftM f (MStream x) = MStream (f $ fmap (liftM f) x)

liftMStream :: Functor m => m a -> MStream m b a
liftMStream x = MStream $ fmap NoMore x

smap :: Functor f => (a -> a') -> (b -> b') -> Stream f a b -> Stream f a' b'
smap f g (NoMore x) = NoMore (g x)
smap f g (x :< xs) = f x :< fmap (smap f g) xs

msmap :: Monad f => (a -> a') -> (b -> b') -> MStream f a b -> MStream f a' b'
msmap f g (MStream x) = MStream $ fmap (smap f g) x

yield :: Monad m => a -> MStream m a ()
yield = MStream . pure . syield

syield :: Applicative m => a -> Stream m a ()
syield x = x :< pure (NoMore ())

listToStream :: Applicative f => [a] -> Stream f a ()
listToStream [] = NoMore ()
listToStream (x : xs) = x :< pure (listToStream xs)

listToMStream :: Monad f => [a] -> MStream f a ()
listToMStream [] = return ()
listToMStream (x : xs) = do
  yield x
  listToMStream xs

streamIterate :: Monad m => s -> (s -> o -> m s) -> Stream m o () -> MStream m s s
streamIterate i f = go i where
  go s (NoMore ()) = pure s
  go s (x :< xs) = do
      s' <- liftMStream (f s x)
      yield s'
      xs' <- liftMStream xs
      go s' xs'

mstreamIterate :: Monad m => s -> (s -> o -> m s) -> MStream m o () -> MStream m s s
mstreamIterate i f (MStream s) = do
  xs <- liftMStream s
  streamIterate i f xs

runStream :: Monad m => (a -> m ()) -> Stream m a b -> m b
runStream act (NoMore x) = return x
runStream act (x :< f) = do
  act x
  f' <- f
  runStream act f'

runMStream :: Monad m => (a -> m ()) -> MStream m a b -> m b
runMStream act (MStream x) = do
  s <- x
  runStream act s

stdinStream :: IO (Stream IO Double ())
stdinStream = do
  eof <- isEOF
  if eof then return (NoMore ()) else fmap (:< stdinStream) (read <$> getLine)

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

merge :: Monad m => [Stream m y a] -> Stream m [y] [a]
merge xs = case allOneSide (map unfold xs) of
  Just (Right as) -> NoMore as
  Just (Left bs) -> let (xs, fs) = unzip bs in
    xs :< (merge <$> sequence fs)
  _ -> error "Streams not uniform"
  where
  unfold (NoMore a) = Right a
  unfold (x :< f) = Left (x, f)

mergeWith :: Monad m => ([(y, MStream m y a)] -> m [(y', MStream m y a)]) -> [Stream m y a] -> MStream m [y'] [a]
mergeWith f xs = case allOneSide (map unfold xs) of
  Just (Right as) -> pure as
  Just (Left bs) -> do
    bs' <- liftMStream (f bs)
    let (ys, ss) = unzip bs'
    yield ys
    mergeWithM f ss
  _ -> error "Streams not uniform"
  where
  unfold (NoMore a) = Right a
  unfold (x :< f) = Left (x, MStream f)

mergeM :: Monad m => [MStream m y a] -> MStream m [y] [a]
mergeM xs = MStream $ fmap merge (sequence [ x | MStream x <- xs ])

mergeWithM :: Monad m => ([(y, MStream m y a)] -> m [(y', MStream m y a)]) -> [MStream m y a] -> MStream m [y'] [a]
mergeWithM f xs = do
  xs' <- liftMStream (sequence [ x | MStream x <- xs ])
  mergeWith f xs'
