module SymbolTableData where

import Language.Haskell.Exts.Annotated.Syntax
import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.SrcLoc

data SymbolProperty = SymbolProperty
  { sympName :: !(QName ())
  , sympPrio :: {-# UNPACKED #-} !Int -- infix operator priority
  , sympAssoc :: !(Assoc ())
  }

type SymbolTable = SrcInfo l => QName l -> Either String SymbolProperty

emptySymbolTable :: SymbolTable
emptySymbolTable name = Left ("symbol " ++ prettyPrint name ++ " not found.")
