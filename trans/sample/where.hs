f = putStrLn str where
  str = ['d', 'e', 'f']

main = case 2 of
  2 -> putStrLn str where
    str = ['a', 'b', 'c']
  3 -> f
