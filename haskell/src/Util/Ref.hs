-- Mutable references, achieved in a purely functional manner, so that state can be duplicated

{-# LANGUAGE ExistentialQuantification, FlexibleContexts #-}
module Util.Ref (
  module Util.Ref,
  MonadState
)

where

import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad.State
import Unsafe.Coerce (unsafeCoerce)

data Any = forall a. Any a

type Heap = (Int, Map Int Any)

newtype Ref a = Ref Int
  deriving (Eq, Show)

emptyHeap :: Heap
emptyHeap = (0, M.empty)

newRef :: MonadState Heap m => a -> m (Ref a)
newRef x = do
  (maxRef, h) <- get
  let nRef = maxRef + 1
  put (nRef, M.insert nRef (Any x)  h)
  return (Ref nRef)

readRef :: MonadState Heap m => Ref a -> m a
readRef (Ref i) = do
  (_, h) <- get
  case h M.! i of
    Any x -> pure (unsafeCoerce x)

writeRef :: MonadState Heap m => Ref a -> a -> m ()
writeRef (Ref i) x = do
  (n, h) <- get
  put (n, M.insert i (Any x) h)

modifyRef :: MonadState Heap m => Ref a -> (a -> a) -> m ()
modifyRef ref f = readRef ref >>= writeRef ref . f

modifyRef' :: MonadState Heap m => Ref a -> (a -> a) -> m ()
modifyRef' ref f = do
  x <- readRef ref
  let x' = f x
  x' `seq` writeRef ref x'

freeRef ::  MonadState Heap m => Ref a -> m ()
freeRef (Ref i) = do
  (n, h) <- get
  put (n, M.delete i h)