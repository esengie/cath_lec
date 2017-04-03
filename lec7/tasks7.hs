{-# LANGUAGE DeriveFunctor #-}

import Control.Monad (join, ap)


-- Task 4

data Term a
  = Var a
  | App (Term a) (Term a)
  | Lam (Term (Maybe a))
  deriving (Show, Eq, Functor)

instance Applicative Term where
  pure = Var
  (<*>) = ap

instance Monad Term where
  Var a >>= f = f a
  App l r >>= f = App (l >>= f) (r >>= f)
  Lam xs >>= f = Lam (xs >>= \v -> case v of
    Nothing -> return Nothing
    Just xs' -> fmap Just $ f xs')

abstract :: Eq a => a -> Term a -> Term a
abstract x = Lam . fmap (match x)

match :: Eq a => a -> a -> Maybe a
match x y = if x == y then Nothing else (Just y)

instantiate :: Term a -> Term (Maybe a) -> Term a
instantiate t = join . fmap (subst t . fmap Var)

subst :: a -> Maybe a -> a
subst x Nothing = x
subst x (Just y) = y

nf :: Term a -> Term a
nf e@Var{} = e
nf (Lam b) = Lam $ nf b
nf (App f a) = case nf f of
  Lam b -> nf (instantiate a b)
  f' -> App f' $ nf a

lxly'x = abstract "x" $ abstract "y" $ (Var "x")
nfer = App (App lxly'x (Var "x")) (Var "y")







----
