module Parse
  ( getAllModules
  , prettyPrintAllModules
  ) where

import Language.Haskell.Exts.Annotated
import Language.Haskell.Exts.Pretty
import qualified Data.Map.Strict as M
import System.IO
import System.FilePath
import Control.Exception

moduleImports :: Module l -> [String]
moduleImports (Module _ _ _ imports _) = map takeOne imports where
  takeOne imp = case importModule imp of
    ModuleName _ modName -> modName

modNameToPath :: String -> FilePath
modNameToPath = go where
  go [] = extSeparator : "hs"
  go ('.' : ns) = pathSeparator : go ns
  go (n : ns) = n : go ns

getModule :: String -> IO (ParseResult (Module SrcSpanInfo))
getModule modName = do
  let filename = modNameToPath modName
  catch
    ( do
      src <- readFile filename
      return (parseModule src)
    )
    (\e -> return $ ParseFailed (SrcLoc filename 0 0) (show (e :: SomeException)))

addMoreModules :: M.Map String (Module SrcSpanInfo) -> [String] -> IO (ParseResult (M.Map String (Module SrcSpanInfo)))
addMoreModules = go where
  go acc [] = return $ pure acc
  go acc (m : ms)
    | M.member m acc = go acc ms
    | m == "Prelude" = go acc ms
    | otherwise = do
      res <- getModule m
      case res of
        ParseFailed loc msg -> return (ParseFailed loc{srcFilename=m} msg)
        ParseOk mod -> go (M.insert m mod acc) (moduleImports mod ++ ms)

getAllModules :: String -> IO (ParseResult (M.Map String (Module SrcSpanInfo)))
getAllModules src =
  case parseModule src of
    ParseFailed loc msg -> return $ ParseFailed loc msg
    ParseOk mod -> do
      let
        modName = case mod of
          Module _ Nothing _ _ _ -> "Main"
          Module _ (Just (ModuleHead _ (ModuleName _ modName) _ _)) _ _ _ -> modName

        imports = moduleImports mod

      addMoreModules (M.singleton modName mod) imports

prettyPrintAllModules :: M.Map String (Module SrcSpanInfo) -> IO ()
prettyPrintAllModules = go . M.toAscList where
  go [] = return ()
  go ((modName, mod) : ms) = do
    putStrLn $ modName ++ ":"
    putStrLn $ prettyPrint mod
    putStrLn ""
    go ms
