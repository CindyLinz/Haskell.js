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

main = putStrLn (showPosInt 257)
