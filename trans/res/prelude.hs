{-# LANGUAGE GADTs, KindSignatures, MagicHash #-}
module Prelude where

--import Prelude (undefined)
--import GHC.Prim

data Bool :: * where
  False :: Bool
  True :: Bool

data Int :: * where
  I# :: Int# -> Int

data Char :: * where
  C# :: Char# -> Char

data Either :: * -> * -> * where
  Left :: a -> Either a b
  Right :: b -> Either a b

data Maybe :: * -> * where
  Nothing :: Maybe a
  Just :: a -> Maybe a

-- [a] 語法特殊, 不用 Haskell 定義

id = \x -> x

flip = \f a b -> f b a

($) = id

(++) = \as bs -> case as of
  [] -> bs
  (:) a as -> (:) a ((++) as bs)

take = \n ls -> case n of
  0 -> []
  _ -> case ls of
    [] -> []
    (:) a as -> a : take (n - 1) as

(+) = \a b -> case a of
  I# a# -> case b of
    I# b# -> I# ((+#) a# b#)

(-) = \a b -> case a of
  I# a# -> case b of
    I# b# -> I# ((-#) a# b#)

(<=) = \a b -> case a of
  I# a# -> case b of
    I# b# -> case (<=#) a# b# of
      0# -> False
      _ -> True

div = \a b -> case a of
  I# a# -> case b of
    I# b# -> I# (quotInt# a# b#)

mod = \a b -> case a of
  I# a# -> case b of
    I# b# -> I# (remInt# a# b#)
