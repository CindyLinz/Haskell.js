infixl 7 -.
infix 7 +., *.

(+.) = (+)
(-.) = (-)
(*.) = (*)

main = do
  putStrLn $ show $ 3 +. 4 *. 5
  do
    let
      infixl 6 +., -.
      infixl 7 *.

      (+.) = (+)
      (-.) = (-)
      (*.) = (*)
    putStrLn $ show $ 3 +. 4 *. 5
