module SymbolTable
  (
  ) where

import Data.Functor
import qualified Data.Map.Strict as M
import Lens.Micro

import Language.Haskell.Exts.Annotated.Syntax

import SymbolTableData
import DesugarClass

lookupSymbolTableLayer :: SymbolTableLayer -> String -> SymbolQueryResult
lookupSymbolTableLayer syml name =
  maybe SymbolQueryNotFound id (M.lookup name syml)

lookupSymbolTable :: SymbolTable -> Maybe String -> String -> SymbolQueryResult
lookupSymbolTable symt Nothing name = localQuery (symtLocal symt) where
  localQuery [] = SymbolQueryNotFound
  localQuery (syml : symls) = case lookupSymbolTableLayer syml name of
    SymbolQueryNotFound -> localQuery symls
    others -> others

addSymbolTableLayer :: SymbolTable -> SymbolTable
addSymbolTableLayer symt = symt
  {symtLocal = emptySymbolTableLayer : symtLocal symt}

addLocalSymbolToLayer :: SymbolProperty -> SymbolTableLayer -> SymbolTableLayer
addLocalSymbolToLayer prop syml = undefined

addLocalSymbolToTable :: SymbolProperty -> SymbolTable -> SymbolTable
addLocalSymbolToTable = undefined
--addLocalSymbolToTable prop symt = symt {symtLocal = head (symtLocal symt)

addLocalSymbol :: SymbolProperty -> SymbolTable -> SymbolTable
addLocalSymbol = addLocalSymbolToTable

importSymbols :: ImportDecl l -> SymbolTable -> SymbolTable -> SymbolTable
importSymbols = undefined
