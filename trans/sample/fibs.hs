{-# LANGUAGE GADTs, KindSignatures #-}

data List :: * -> * where
  Nil :: List a
  Cons :: a -> List a -> List a

concat' = \as bs -> case as of
  [] -> bs
  (:) a as -> (:) a (concat' as bs)

showPosInt = \n ->
  let
    showDigit = \d -> case d of
      0 -> '0'
      1 -> '1'
      2 -> '2'
      3 -> '3'
      4 -> '4'
      5 -> '5'
      6 -> '6'
      7 -> '7'
      8 -> '8'
      9 -> '9'

    go = \n -> case n of
      0 -> []
      _ -> (:) (showDigit (mod n 10)) (go (div n 10))

    reverse = \ls ->
      let
        go = \ls rs -> case ls of
          [] -> rs
          (:) a as -> go as ((:) a rs)
      in
        go ls []
  in
    reverse (go n)

showPosIntList = \ls ->
  let
    go = \as -> case as of
      Nil -> (:) ']' []
      Cons a as ->
        concat' (showPosInt a) ( case as of
          Nil -> (:) ']' []
          Cons _ _ -> (:) ',' (go as)
        )
  in
    (:) '[' (go ls)

zipWithList = \f ->
  let
    go = \as bs -> case as of
      Nil -> Nil
      Cons a as -> case bs of
        Nil -> Nil
        Cons b bs -> Cons (f a b) (go as bs)
  in
    go

tailList = \ls -> case ls of
  Nil -> Nil
  Cons a as -> as

headList = \ls -> case ls of
  Nil -> 0
  Cons a as -> a

takeList = \n ls -> case n of
  0 -> Nil
  _ -> case ls of
    Nil -> Nil
    Cons a as -> Cons a (takeList ((-) n 1) as)

fibs = Cons 1 (Cons 1 (zipWithList (+) fibs (tailList fibs)))

main = putStrLn (showPosIntList (takeList 30 fibs))
