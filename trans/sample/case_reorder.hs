data Pair :: * -> * -> * where
  Pair :: a -> b -> Pair a b

--main = case "abc" of
--  "abc" -> putStrLn "abc-"
--  "def" -> putStrLn "def"

main = flip id (Just (Just (Pair 3 5))) $ \case
  _ | False -> putStrLn "Any"


  Just Nothing | False -> putStrLn "Just Nothing False"
  Just (Just (Pair a c)) | a <= 3 -> putStrLn "Just (Just (Pair a c)) | a <= 3"
  Just (Just (Pair b _)) | b <= 3 -> putStrLn "Just (Just (Pair b _)) | b <= 3"
  Just (Just (Pair _ 4)) -> putStrLn "Just (Just (Pair _ 4))"

  Just a | False -> putStrLn "Just a | False"

  Just Nothing | True -> putStrLn "Just Nothing True"

  Nothing -> putStrLn "Nothing"


  a | False -> putStrLn "a all False"
    | True -> putStrLn "a all True"
    | otherwise -> putStrLn "a all otherwise"

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
