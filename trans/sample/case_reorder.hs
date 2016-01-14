main = case Just (Just 3) of
  _ | False -> putStrLn "Any"


  Just Nothing | False -> putStrLn "Just Nothing False"
  Just (Just a) | a <= 3 -> putStrLn "Just (Just a) | a <= 3"
  Just (Just b) | b <= 3 -> putStrLn "Just (Just b) | b <= 3"

  Just a | False -> putStrLn "Just a | False"

  Just Nothing | True -> putStrLn "Just Nothing True"

  Nothing -> putStrLn "Nothing"


  a | False -> putStrLn "a all False"
    | True -> putStrLn "a all True"
    | otherwise -> putStrLn "a all otherwise"

-- let
--   fallback- = undefined
--   x1- = Just (Just 3)
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
--                 case False of
--                   False ->
--                     case True of
--                       False -> putStrLn "a all otherwise"
--                       True -> putStrLn "a all True"
--                   True -> putStrLn "a all False"
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
--                       in
--                         case False of
--                           False -> fallback-
--                           True -> putStrLn "Just a | False"
--                 in
--                   case x1- of
--                     Nothing ->
--                       case False of
--                         False -> fallback-
--                         True -> putStrLn "Just Nothing"
--                     Just x2- ->
--                       let
--                         fallback+ = fallback-
--                       in
--                         let
--                           fallback- =
--                             let
--                               fallback- = fallback+
--                             in
--                               let
--                                 b = x2-
--                               in
--                                 case b <= 3 of
--                                   False -> fallback-
--                                   True -> putStrLn "Just (Just b) | b <= 3"
--                         in
--                           let
--                             a = x2-
--                           in
--                             case a <= 3 of
--                               False -> fallback-
--                               True -> putStrLn "Just (Just a) | a <= 3"
--     in
--       case False of
--         False -> fallback-
--         True -> putStrLn "Any"
--
