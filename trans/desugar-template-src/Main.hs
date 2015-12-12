module Main where

import System.Environment
import qualified Language.Haskell.Exts.Annotated.Syntax
import qualified Language.Haskell.Exts.Syntax
import DeriveTemplate
import Control.Arrow
import Control.Applicative
import Data.Functor
import Data.Monoid

$(deriveDesugarTemplate "genNormal" "Language.Haskell.Exts.Syntax.Module")
$(deriveDesugarTemplate "genAnnotated" "Language.Haskell.Exts.Annotated.Syntax.Module")

main = do
  progName <- getProgName
  args <- getArgs
  case args of
    [modName, funPrefix, mode, additionalArgNum] -> do
      let
        code = (if mode=="1" then genAnnotated else genNormal) modName funPrefix (read additionalArgNum)
      putStrLn code
    _ -> putStrLn $ "usage: ./" ++ progName ++ " module_name function_prefix normal:0/annotated:1 num_of_additional_args"
