module SymbolTableData where

import qualified Data.Map.Strict as M
import Lens.Micro.Type

import Language.Haskell.Exts.Annotated.Syntax
import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.SrcLoc

data SymbolProperty = SymbolProperty
  { sympName :: !(QName SrcSpan)
  , sympPrio :: {-# UNPACKED #-} !Int -- infix operator priority
  , sympAssoc :: !(Assoc SrcSpan)
  }

data SymbolQueryResult
  = SymbolQuerySuccess !SymbolProperty
  | SymbolQueryDup [SrcSpan]
  | SymbolQueryNotFound

type SymbolTableLayer = M.Map String SymbolQueryResult
emptySymbolTableLayer :: SymbolTableLayer
emptySymbolTableLayer = M.empty

data SymbolTable = SymbolTable
  { symtExternal :: M.Map String SymbolTableLayer
  , symtLocal :: [SymbolTableLayer]
  }
emptySymbolTable :: SymbolTable
emptySymbolTable = SymbolTable
  { symtExternal = M.empty
  , symtLocal = []
  }
symtExternalL :: Lens' SymbolTable (M.Map String SymbolTableLayer)
symtExternalL 
