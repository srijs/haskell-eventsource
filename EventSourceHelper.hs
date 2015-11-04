{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module EventSourceHelper
  ( Fold
  , QueryT, Query, CommandT, TransactionT
  , fold, fold1
  , trans, query
  , runQ, runC, runTX
  , ensure, find
  , lastHappenedAfter, lastHappenedBefore
  ) where

import Data.Functor.Identity
import Data.Profunctor
import Control.Arrow ((***))
import Control.Applicative ((<**>), liftA2)
import Control.Monad (join)

data Ap f a where
  Pure :: a -> Ap f a
  Ap :: f a -> Ap f (a -> b) -> Ap f b

instance Functor (Ap f) where
  fmap f (Pure a) = Pure (f a)
  fmap f (Ap x y) = Ap x ((f .) <$> y)

instance Applicative (Ap f) where
  pure = Pure
  Pure f <*> y = fmap f y
  Ap x y <*> z = Ap x $ flip <$> y <*> z

retractAp :: Applicative f => Ap f a -> f a
retractAp (Pure a) = pure a
retractAp (Ap x y) = x <**> retractAp y

transAp :: (forall x. f x -> g x) -> Ap f a -> Ap g a
transAp _ (Pure a) = Pure a
transAp f (Ap x y) = Ap (f x) (transAp f y)

data Fold f e a where
  Fold :: (e -> x -> x) -> x -> (x -> f a) -> Fold f e a

instance Functor f => Profunctor (Fold f) where
  lmap f (Fold cons nil fin) = Fold (cons . f) nil fin
  rmap f (Fold cons nil fin) = Fold cons nil (fmap f . fin)

instance Functor f => Functor (Fold f e) where
  fmap = rmap

instance Applicative f => Applicative (Fold f e) where
  pure = liftf . pure
  (Fold cons nil fin) <*> (Fold cons' nil' fin') =
    Fold (liftA2 (***) cons cons') (nil, nil') (\(x, x') -> fin x <*> fin' x')

joinf :: Monad f => Fold f e (f a) -> Fold f e a
joinf (Fold cons nil fin) = Fold cons nil (join . fin)

liftf :: f a -> Fold f e a
liftf a = Fold (\_ _ -> ()) () (const a)

runf :: Fold f e a -> [e] -> f a
runf (Fold cons nil fin) = fin <$> foldl (flip cons) nil

transf :: (forall x. f x -> g x) -> (forall x y. Fold f x y -> Fold g x y)
transf f (Fold cons nil fin) = Fold cons nil (rmap f fin)

emapf :: (e' -> e) -> Fold f e a -> Fold f e' a
emapf f (Fold cons nil fin) = Fold (cons . f) nil fin

-- Free Applicative of Folds.
newtype QueryT f e a = Query (Ap (Fold f e) (f a))

toFold :: Monad f => QueryT f e a -> Fold f e a
toFold (Query x) = joinf . retractAp $ x

fold :: Applicative f => (e -> a -> a) -> a -> QueryT f e a
fold f a = Query $ Ap (Fold f a pure) $ Pure pure

fold1 :: (Applicative f, Monoid a) => (e -> a -> a) -> QueryT f e a
fold1 f = fold f mempty

type Query e a = QueryT Identity e a

joinq :: Monad f => QueryT f e (f a) -> QueryT f e a
joinq (Query (Pure a)) = Query $ Ap (liftf a) $ Pure id
joinq (Query (Ap l qf)) = Query $ Ap l $ fmap join <$> qf

trans :: Functor f => (forall x. f x -> g x) -> QueryT f e a -> QueryT g e a
trans f (Query x) = Query $ transAp (transf f) (fmap f x)

query :: Applicative f => Query e a -> QueryT f e a
query = trans $ pure . runIdentity

instance Functor f => Profunctor (QueryT f) where
  lmap f (Query x) = Query $ transAp (emapf f) x
  rmap f (Query x) = Query $ fmap (fmap f) x

instance Functor f => Functor (QueryT f e) where
  fmap = rmap

instance Applicative f => Applicative (QueryT f e) where
  pure = Query . Pure . pure
  Query x <*> Query y = Query $ liftA2 (<*>) x y

runQ :: Monad f => QueryT f e a -> [e] -> f a
runQ = runf . toFold

-- A command is a query that returns an event to be appended.
type CommandT f e = QueryT f e [e]

type TransactionT f e = [CommandT f e]

runC :: Monad f => CommandT f e -> [e] -> f [e]
runC q es = (es ++) <$> runQ q es

runTX :: Monad f => TransactionT f e -> [e] -> f [e]
runTX = sequ . map runC
  where sequ (f : fs) es = f es >>= sequ fs
        sequ [] es = pure es

ensure :: Query e Bool -> a -> QueryT (Either a) e ()
ensure q a = joinq $ rmap ensure' $ query q
  where ensure' True = Right ()
        ensure' _ = Left a

find :: (e -> Bool) -> Query e (Maybe e)
find p = fold findE Nothing
  where findE e Nothing | p e = Just e
        findE _ s = s

lastIndexOf :: (Eq e, Applicative f) => e -> QueryT f e (Maybe Integer)
lastIndexOf e = rmap snd $ fold findLastIndex (0, Nothing)
  where findLastIndex e' (i, _) | e == e' = (i, Just i)
        findLastIndex _ x = x

lastHappenedBefore :: (Eq e, Applicative f) => e -> e -> QueryT f e Bool
lastHappenedBefore e e' = (<) <$> lastIndexOf e <*> lastIndexOf e'

lastHappenedAfter :: (Eq e, Applicative f) => e -> e -> QueryT f e Bool
lastHappenedAfter e e' = (>) <$> lastIndexOf e <*> lastIndexOf e'
