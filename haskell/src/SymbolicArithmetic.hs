module SymbolicArithmetic where

import Data.Set (Set)
import qualified Data.Set as Set

data UnOp =
    Log | Exp
  | Sin | Cos
  | Asin | Acos | Atan
  | Sinh | Cosh
  | Asinh | Acosh | Atanh
  deriving (Eq, Show, Ord)

data Exp env a where
  Var :: env -> Exp env a
  Sum :: [Exp env a] -> Exp env a
  Prod :: [Exp env a] -> Exp env a
  Const :: a -> Exp env a
  Pow :: Exp env a -> Int -> Exp env a
  UnOp :: UnOp -> Exp env a -> Exp env a
  deriving (Eq, Show, Ord)

instance Functor (Exp env) where
  fmap f (Var i) = Var i
  fmap f (Sum xs) = Sum $ map (fmap f) xs
  fmap f (Prod xs) = Prod $ map (fmap f) xs
  fmap f (Const x) = Const (f x)
  fmap f (Pow e k) = Pow (fmap f e) k
  fmap f (UnOp o e) = UnOp o (fmap f e)


sum1 :: Num a => [Exp env a] -> Exp env a
sum1 [] = Const 0
sum1 [x] = x
sum1 xs = Sum xs

prod1 :: Num a => [Exp env a] -> Exp env a
prod1 [] = Const 1
prod1 [x] = x
prod1 xs = Prod xs

getSum :: (Eq a, Num a) => Exp env a -> [Exp env a]
getSum (Sum xs) = concatMap getSum xs
getSum (Const 0) = []
getSum x = [x]

getSum1 :: (Eq a, Num a) => Exp env a -> (Maybe a, [Exp env a])
getSum1 e = go Nothing [] [e] where
  go mk xs [] = (mk, xs)
  go mk xs (e : es) = case e of
    Sum as -> go mk xs (as ++ es)
    Const k' -> go (case mk of
      Nothing -> Just k'
      Just k -> Just (k + k')) xs es
    _ -> go mk (e : xs) es

getProd1 :: (Eq a, Num a) => Exp env a -> (Maybe a, [Exp env a])
getProd1 e = go Nothing [] [e] where
  go mk xs [] = (mk, xs)
  go mk xs (e : es) = case e of
    Prod as -> go mk xs (as ++ es)
    Const k' -> case mk of
      Nothing -> go (Just k') xs es
      Just 0 -> (Nothing, [])
      Just k -> go (Just (k * k')) xs es
    _ -> go mk (e : xs) es

foldMaybe :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
foldMaybe f (Just x) (Just y) = Just (f x y)
foldMaybe f (Just x) Nothing = Just x
foldMaybe f Nothing (Just y) = Just y
foldMaybe f Nothing Nothing = Nothing

sum1' :: (Eq a, Num a) => (Maybe a, [Exp env a]) -> (Maybe a, [Exp env a]) -> Exp env a
sum1' (mk, es) (mk', es') = sum1 $ (case foldMaybe (+) mk mk' of
  Just 0 -> id
  Just k -> (Const k :)
  Nothing -> id
  ) (es ++ es')

prod1' :: (Eq a, Num a) => (Maybe a, [Exp env a]) -> (Maybe a, [Exp env a]) -> Exp env a
prod1' (mk, es) (mk', es') = case foldMaybe (*) mk mk' of
  Just 0 -> 0
  Just 1 -> prod1 xs
  Just k -> prod1 (Const k : xs)
  Nothing -> prod1 xs
  where xs = es ++ es'

getProd :: (Eq a, Num a) => Exp env a -> [Exp env a]
getProd (Prod xs) = concatMap getProd xs
getProd (Const 1) = []
getProd x = [x]

getSumOfProd :: (Eq a, Num a) => Exp env a -> [[Exp env a]]
getSumOfProd = map getProd . getSum

getConstants :: (a -> a -> a) -> [Exp env a] -> (Maybe a, [Exp env a])
getConstants combine = go Nothing [] where
  go mk xs [] = (mk, xs)
  go mk xs (e : es) = case e of
    Const k' -> go (case mk of
      Nothing -> Just k'
      Just k -> Just (combine k k')) xs es
    _ -> go mk (e : xs) es

negateProd :: (Eq a, Num a) => [Exp env a] -> [Exp env a]
negateProd = f . getConstants (*) where
  f (Nothing, xs) = Const (-1) : xs
  f (Just k, xs) = Const (-k) : xs

negateExp :: (Eq a, Num a) => Exp env a -> Exp env a
negateExp = prod1 . negateProd . getProd

recipExp :: Fractional a => Exp env a -> Exp env a
recipExp (Const x) = Const (recip x)
recipExp (Pow e k) = Pow e (-k)
recipExp e = Pow e (-1)

instance (Eq a, Num a) => Num (Exp env a) where
  x + y = sum1' (getSum1 x) (getSum1 y)
  x * y = prod1' (getProd1 x) (getProd1 y)
  x - y = let (ky, ys) = getSum1 y in
    sum1' (getSum1 x) (fmap negate ky, map negate ys)
  fromInteger = Const . fromInteger
  negate = negateExp
  abs = error "no abs"
  signum = error "no signum"

instance (Eq a, Fractional a) => Fractional (Exp env a) where
  x / y = prod1 (getProd x ++ map recip (getProd y))
  recip = recipExp
  fromRational = Const . fromRational

instance (Eq a, Floating a) => Floating (Exp env a) where
  log = UnOp Log
  exp = UnOp Exp
  pi = Const pi
  sin = UnOp Sin
  cos = UnOp Cos
  asin = UnOp Asin
  acos = UnOp Acos
  atan = UnOp Atan
  sinh = UnOp Sinh
  cosh = UnOp Cosh
  asinh = UnOp Asinh
  acosh = UnOp Acosh
  atanh = UnOp Atanh


evalUnOp :: Floating a => UnOp -> a -> a
evalUnOp Log = log
evalUnOp Exp = exp
evalUnOp Sin = sin
evalUnOp Cos = cos
evalUnOp Asin = asin
evalUnOp Acos = acos
evalUnOp Atan = atan
evalUnOp Sinh = sinh
evalUnOp Cosh = cosh
evalUnOp Asinh = asinh
evalUnOp Acosh = acosh
evalUnOp Atanh = atanh

eval :: Floating a => (env -> a) -> Exp env a -> a
eval env = f where
  f (Var i) = env i
  f (Sum xs) = sum $ map f xs
  f (Prod xs) = product $ map f xs
  f (Const k) = k
  f (Pow e k) = f e ^ k
  f (UnOp g x) = evalUnOp g (f x)

getVar :: Exp env a -> Maybe env
--getVar e | trace "getVar" False = undefined
getVar (Var i) = Just i
getVar (Sum [x]) = getVar x
getVar (Prod [x]) = getVar x
getVar (Pow x 1) = getVar x
getVar (Pow x _) = Nothing
getVar (UnOp f x) = Nothing
getVar (Sum []) = Nothing
getVar (Sum (x : y : xs)) = Nothing
getVar (Prod []) = Nothing
getVar (Prod (x : y : xs)) = Nothing
getVar (Const _) = Nothing

getConstant :: Floating a => Exp env a -> Maybe a
--getConstant e | trace "getConstant" False = undefined
getConstant (Var i) = Nothing
getConstant (Const x) = Just x
getConstant (Sum xs) = fmap sum $ mapM getConstant xs
getConstant (Prod xs) = fmap product $ mapM getConstant xs
getConstant (Pow x k) = (^ k) <$> getConstant x
getConstant (UnOp f x) = evalUnOp f <$> getConstant x

varIn :: Eq env => env -> Exp env a -> Bool
varIn i (Var j) = i == j
varIn i (UnOp f e) = varIn i e
varIn i (Sum xs) = any (varIn i) xs
varIn i (Prod xs) = any (varIn i) xs
varIn i (Pow e k) = varIn i e
varIn i (Const k) = False

notIn :: (Eq env, Eq a) => Exp env a -> Exp env a -> Bool
notIn (Const k) _ = True
notIn e e' | e == e' = False
notIn e (UnOp f e') = notIn e e'
notIn e (Sum xs) = all (notIn e) xs
notIn e (Prod xs) = all (notIn e) xs
notIn e (Pow e' k) = notIn e e'
notIn e (Const k) = True
notIn e (Var i) = True

allVars :: Ord env => Exp env a -> Set env
allVars (Var i) = Set.singleton i
allVars (Sum xs) = Set.unions (map allVars xs)
allVars (Prod xs) = Set.unions (map allVars xs)
allVars (Const k) = Set.empty
allVars (Pow e k) = allVars e
allVars (UnOp f e) = allVars e

mapVars :: (env -> env') -> Exp env a -> Exp env' a
mapVars fenv = f where
  f (Var i) = Var (fenv i)
  f (Sum xs) = Sum $ map f xs
  f (Prod xs) = Prod $ map f xs
  f (Const k) = Const k
  f (Pow e k) = Pow (f e) k
  f (UnOp op e) = UnOp op (f e)