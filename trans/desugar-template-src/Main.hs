module Main where

import System.Environment
import Language.Haskell.Exts.Syntax
import DeriveTemplate
import Control.Arrow
import Control.Applicative
import Data.Functor
import Data.Monoid

$(deriveDesugarTemplate "xx")

main = do
  progName <- getProgName
  args <- getArgs
  case args of
    [modName, funPrefix] -> do
      let code = xx modName funPrefix
      putStrLn code
    _ -> putStrLn $ "usage: ./" ++ progName ++ " module_name function_prefix"
