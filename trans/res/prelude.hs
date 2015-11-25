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

data Pair :: * -> * -> * where
  Pair :: a -> b -> Pair a b

-- [a] 語法特殊, 不用 Haskell 定義

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
