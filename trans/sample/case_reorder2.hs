data Pair :: * -> * -> * where
  Pair :: a -> b -> Pair a b

--main = putStrLn ('A' : 'n' : 'y' : [])

--main = case Nothing of
--  Just a | True -> putStrLn ('A' : 'n' : 'Y' : [])
--  Nothing | True -> putStrLn ('A' : 'n' : 'y' : [])

main = case Just (Just (Pair (Pair 4 2) 5)) of
  _ | False -> putStrLn ('A' : 'n' : 'y' : [])


  Just Nothing | False -> putStrLn ('J' : 'u' : 's' : 't' : ' ' : 'N' : 'o' : 't' : 'h' : 'i' : 'n' : 'g' : ' ' : 'F' : 'a' : 'l' : 's' : 'e' : [])
  Just (Just (Pair (Pair _ a) c)) | c <= 3 -> putStrLn ('J' : 'u' : 's' : 't' : ' ' : '(' : 'J' : 'u' : 's' : 't' : ' ' : '(' : 'P' : 'a' : 'i' : 'r' : ' ' : 'a' : ' ' : 'c' : ')' : ')' : ' ' : '|' : ' ' : 'a' : ' ' : '<' : '=' : ' ' : '3' : [])
  Just (Just (Pair (Pair b _) _)) | b <= 3 -> putStrLn ('J' : 'u' : 's' : 't' : ' ' : '(' : 'J' : 'u' : 's' : 't' : ' ' : '(' : 'P' : 'a' : 'i' : 'r' : ' ' : 'b' : ' ' : '_' : ')' : ')' : ' ' : '|' : ' ' : 'b' : ' ' : '<' : '=' : ' ' : '3' : [])
  Just (Just (Pair _ 5)) -> putStrLn ('J' : 'u' : 's' : 't' : ' ' : '(' : 'J' : 'u' : 's' : 't' : ' ' : '(' : 'P' : 'a' : 'i' : 'r' : ' ' : '_' : ' ' : '5' : ')' : ')' : [])

  Just a | False -> putStrLn ('J' : 'u' : 's' : 't' : ' ' : 'a' : ' ' : '|' : ' ' : 'F' : 'a' : 'l' : 's' : 'e' : [])

  Just Nothing | True -> putStrLn ('J' : 'u' : 's' : 't' : ' ' : 'N' : 'o' : 't' : 'h' : 'i' : 'n' : 'g' : ' ' : 'T' : 'r' : 'u' : 'e' : [])

  Nothing -> putStrLn ('N' : 'o' : 't' : 'h' : 'i' : 'n' : 'g' : [])


  a | otherwise, False -> putStrLn ('a' : ' ' : 'a' : 'l' : 'l' : ' ' : 'F' : 'a' : 'l' : 's' : 'e' : [])
    | a <= 2 -> putStrLn ('a' : ' ' : 'a' : 'l' : 'l' : ' ' : 'T' : 'r' : 'u' : 'e' : [])
    | otherwise -> putStrLn ('a' : ' ' : 'a' : 'l' : 'l' : ' ' : 'o' : 't' : 'h' : 'e' : 'r' : 'w' : 'i' : 's' : 'e' : [])
    where a = 3

-- let
--   fallback- = undefined
--   x1- = Just (Just (Pair 3 5))
-- in
--   let
--     fallback+ = fallback-
--   in
--     let
--       fallback- =
--         let
--           fallback- =
--             let
--               fallback- = fallback+
--             in
--               let
--                 a = x1-
--               in
--                 let
--                   fallback+ = fallback-
--                 in
--                   let
--                     fallback- =
--                       let
--                         fallback- =
--                           let
--                             fallback- = fallback+
--                           in
--                             putStrLn "a all otherwise"
--                       in
--                         case True of
--                           False -> fallback-
--                           True -> putStrLn "a all True"
--                   in
--                     case False of
--                       False -> fallback-
--                       True -> putStrLn "a all False"
--         in
--           case x1- of
--             Nothing -> putStrLn "Nothing"
--             Just x1- ->
--               let
--                 fallback+ = fallback-
--               in
--                 let
--                   fallback- =
--                     let
--                       fallback- =
--                         let
--                           fallback- = fallback+
--                         in
--                           case x1- of
--                             Nothing ->
--                               case True of
--                                 False -> fallback-
--                                 True -> putStrLn "Just Nothing True"
--                             Just x1- -> fallback-
--                     in
--                       let
--                         a = x1-
--                       in -- 只有一個 guard branch, 可以考慮把 fallback+ fallback- 的耦極生成省掉
--                         case False of
--                           False -> fallback-
--                           True -> putStrLn "Just a | False"
--                 in
--                   case x1- of
--                     Nothing ->
--                       case False of
--                         False -> fallback-
--                         True -> putStrLn "Just Nothing"
--                     Just x1- ->
--                       let -- 只有一種 branch 的話, 可以考慮把 fallback+ fallback- 的耦極生成省掉
--                         fallback+ = fallback-
--                       in
--                         let
--                           fallback- = fallback+
--                         in
--                           case x1- of
--                             Pair x1- x2- ->
--                               let
--                                 fallback+ = fallback-
--                               in
--                                 let
--                                   fallback- =
--                                     let
--                                       fallback- =
--                                         let
--                                           fallback- = fallback+
--                                         in
--                                           let
--                                             fallback+ = fallback-
--                                           in
--                                             let
--                                               fallback- = fallback+
--                                             in
--                                               case x2- of
--                                                 4 -> putStrLn "Just (Just (Pair _ 4))"
--                                                 _ -> fallback-
--                                     in
--                                       let
--                                         b = x1-
--                                       in
--                                         case b <= 3 of
--                                           False -> fallback-
--                                           True -> putStrLn "Just (Just (Pair b _)) | b <= 3"
--                                 in
--                                   let
--                                     a = x1-
--                                   in
--                                     let
--                                       c = x2-
--                                     in
--                                       case a <= 3 of
--                                         False -> fallback-
--                                         True -> putStrLn "Just (Just (Pair a c)) | a <= 3"
----                         let
----                           fallback- =
----                             let
----                               fallback- = fallback+
----                             in
----                               let
----                                 b = x1-
----                               in
----                                 case b <= 3 of
----                                   False -> fallback-
----                                   True -> putStrLn "Just (Just b) | b <= 3"
----                         in
----                           let
----                             a = x1-
----                           in
----                             case a <= 3 of
----                               False -> fallback-
----                               True -> putStrLn "Just (Just a) | a <= 3"
--     in
--       case False of
--         False -> fallback-
--         True -> putStrLn "Any"
--
