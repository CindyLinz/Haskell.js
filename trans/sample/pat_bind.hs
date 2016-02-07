data Triple :: * -> * -> * -> * where
  Triple :: a -> b -> c -> Triple a b c

main =
  let
    Just (Right a) | 1 <= 0 = Just (Right "3")
                   | otherwise = Just (Right "4")
    Triple d (Just e) (Left f)
      | 3 <= 2 = Triple "D" (Just "E") (Left "F")
      | otherwise = Triple "d" (Just "e") (Left "f")
  in
    putStrLn (a ++ " (" ++ d ++ "," ++ e ++ "," ++ f ++ ")")
