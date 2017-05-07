import Data.Monoid

----------------------------------------------------------
filterFirst :: (a -> Bool) -> [a] -> [a]
filterFirst _ [] = []
filterFirst p (x : xs)
  | p x       = x : filterFirst p xs
  | otherwise = xs

-- 1

data AElem a = A a | R a deriving Eq

revA :: AElem a -> AElem a
revA (A a) = R a
revA (R a) = A a

mapA' :: (a -> b) -> AElem a -> AElem b
mapA' f (A a) = A (f a)
mapA' f (R a) = R (f a)

mapA :: AElem (a -> b) -> AElem a -> AElem b
mapA (A f) (A a) = A (f a)
mapA (A f) (R a) = R (f a)
mapA (R f) (A a) = R (f a)
mapA (R f) (R a) = A (f a)

data Abelian a = AList {aList :: [AElem a]}

simplifyA :: Eq a => [AElem a] -> [AElem a]
simplifyA [] = []
simplifyA (A a : as)
  | elem (R a) as = simplifyA $ filterFirst (\x -> x == R a) as
  | otherwise = A a : simplifyA as
simplifyA (R a : as)
  | elem (A a) as = simplifyA $ filterFirst (\x -> x == A a) as
  | otherwise = R a : simplifyA as

compareA :: Eq a => [AElem a] -> [AElem a] -> Bool
compareA [] [] = True
compareA [] _ = False
compareA _ [] = False
compareA (a : as) bs
  | elem a bs = compareA as (filterFirst (\x -> x == a) bs)
  | otherwise = False

instance Eq a => Eq (Abelian a) where
    (AList as) == (AList bs) = compareA (simplifyA as) (simplifyA bs)

instance Functor Abelian where
    fmap f (AList as) = AList $ map (\x -> mapA' f x) as

instance Applicative Abelian where
    pure a = AList [A a]
    (AList fs) <*> (AList as) = AList [mapA f a | f <- fs, a <- as]

instance Monad Abelian where
    return = pure
    (AList as) >>= f = AList (bind as f)
      where
        bind :: [AElem a] -> (a -> Abelian b) -> [AElem b]
        bind [] _ = []
        bind (A a : as) f = (aList $ f a) ++ bind as f
        bind (R a : as) f = (map revA (aList $ f a)) ++ bind as f

----------------------------------------------------------
-- 2

data SemiModuleOverMonoid r a = SList {sList :: [(r, a)]}

instance Functor (SemiModuleOverMonoid r) where
    fmap f (SList ps) = SList $ map (\(x, y) -> (x, f y)) ps

instance Monoid r => Applicative (SemiModuleOverMonoid r) where
    pure a = SList [(mempty, a)]
    (SList pfs) <*> (SList as) = SList [(mappend r1 r2, f a) | (r1, f) <- pfs, (r2, a) <- as]

instance Monoid r => Monad (SemiModuleOverMonoid r) where
    return = pure
    (SList as) >>= f = SList $ concat [map (\(x, y) -> (mappend r x, y)) (sList $ f a) | (r, a) <- as]

----------------------------------------------------------
-- 3

class Ring r where
    zero :: r
    add :: r -> r -> r
    neg :: r -> r
    one :: r
    mul :: r -> r -> r

data SemiModuleOverRing r a = RList {rList :: [(r, a)]}

instance Functor (SemiModuleOverRing r) where
    fmap f (RList ps) = RList $ map (\(x, y) -> (x, f y)) ps

instance Ring r => Applicative (SemiModuleOverRing r) where
    pure a = RList [(one, a)]
    (RList pfs) <*> (RList as) = RList [(mul r1 r2, f a) | (r1, f) <- pfs, (r2, a) <- as]

instance Ring r => Monad (SemiModuleOverRing r) where
    return = pure
    (RList as) >>= f = RList $ concat [map (\(x, y) -> (mul r x, y)) (rList $ f a) | (r, a) <- as]

simplifyR :: (Ring r, Eq r, Eq a) => [(r, a)] -> [(r, a)]
simplifyR ((a, _) : as) | a == zero = as
simplifyR ((a, x) : (b, y): as) | x == y = simplifyR $ (add a b, x) : as
simplifyR (a : as) = a : (simplifyR as)

instance (Ring r, Eq r, Eq a) => Eq (SemiModuleOverRing r a) where
    (RList as) == (RList bs) = (simplifyR as) == (simplifyR bs)

----------------------------------------------------------
-- 4

data Term a = Var a | App (Term a) (Term a) | Lam (Term (Maybe a))

-- Индексы де Брёйна
free = Just
-- v0 -- переменная, связанная ближайшей лямбдой.
v0 = Nothing
-- v1 -- переменная, связанная следующей лямбдой.
v1 = free v0
-- v2 -- переменная, связанная следующей лямбдой.
v2 = free v1

-- Примеры замкнутых термов
i,k,s :: Term a
-- I = \x.x
i = Lam (Var v0)
-- K = \xy.x
k = Lam $ Lam (Var v1)
-- S = \xyz.xz(yz)
s = Lam $ Lam $ Lam $ App (Var v2 `App` Var v0) (Var v1 `App` Var v0)

instance Functor Term where
    fmap = undefined

instance Applicative Term where
    pure = undefined
    (<*>) = undefined

instance Monad Term where
    return = undefined
    (>>=) = undefined

nf :: Term a -> Term a
nf = undefined
