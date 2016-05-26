module SymbolTable
  (
  ) where

import Data.Functor

import Language.Haskell.Exts.Annotated.Syntax

import SymbolTableData
import DesugarClass

addSymbolTableLayer :: SymbolTable -> SymbolTable
addSymbolTableLayer = undefined

addLocalSymbol :: SymbolProperty -> SymbolTable -> SymbolTable
addLocalSymbol = undefined

importSymbols :: ImportDecl l -> SymbolTable -> SymbolTable -> SymbolTable
importSymbols = undefined
