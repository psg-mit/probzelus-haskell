module Util.Bijection where

import qualified Control.Category as C

data Bijection a b = Bijection
  { to :: a -> b
  , from :: b -> a }

instance C.Category Bijection where
  id = Bijection id id
  Bijection f1 t1 . Bijection f2 t2 = Bijection (f1 . f2) (t2 . t1)