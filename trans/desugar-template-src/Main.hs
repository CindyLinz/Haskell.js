module Main where

import System.Environment
import Language.Haskell.Exts.Syntax
import DeriveTemplate
import Control.Arrow
import Control.Applicative
import Data.Functor

$(deriveDesugarTemplate "xx")

main = do
  progName <- getProgName
  args <- getArgs
  case args of
    [modName, funPrefix] -> xx modName funPrefix
    _ -> putStrLn $ "usage: ./" ++ progName ++ " module_name function_prefix"
