module SymbolTableData where

import qualified Data.Map.Strict as M
import Lens.Micro.Type
import Data.Functor

import Language.Haskell.Exts.Annotated.Syntax
import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.SrcLoc

data SymbolProperty = SymbolProperty
  { sympName :: !(Name SrcSpan)
  , sympPrio :: {-# UNPACK #-} !Int -- infix operator priority
  , sympAssoc :: !(Assoc SrcSpan)
  }

data SymbolQueryResult
  = SymbolQuerySuccess !SymbolProperty
  | SymbolQueryDup [SrcSpan]
  | SymbolQueryNotFound

type SymbolTableLayer = M.Map (Name ()) SymbolQueryResult
emptySymbolTableLayer :: SymbolTableLayer
emptySymbolTableLayer = M.empty

data SymbolTable = SymbolTable
  { symtExternal :: M.Map (ModuleName ()) SymbolTableLayer
  , symtLocal :: [SymbolTableLayer]
  }
emptySymbolTable :: SymbolTable
emptySymbolTable = SymbolTable
  { symtExternal = M.empty
  , symtLocal = []
  }
symtExternalL :: Lens' SymbolTable (M.Map (ModuleName ()) SymbolTableLayer)
symtExternalL f symt = (\a -> symt {symtExternal = a}) <$> f (symtExternal symt)
symtLocalL :: Lens' SymbolTable [SymbolTableLayer]
symtLocalL f symt = (\a -> symt {symtLocal = a}) <$> f (symtLocal symt)

type GlobalSymbolTableSet = M.Map (ModuleName ()) SymbolTableLayer
type ModuleReexport = M.Map (ModuleName ()) [ModuleName ()]

