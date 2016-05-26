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
addSymbolTableLayer symt = symt & symtLocalL %~ (emptySymbolTableLayer :)

addLocalSymbol :: SymbolProperty -> SymbolTable -> SymbolTable
addLocalSymbol prop symt = symt & symtLocalL . ix 0 %~ M.insertWith amend name (SymbolQuerySuccess prop) where
  name = case sympName prop of
    Ident _ name -> name
    Symbol _ name -> name
  amend _ (SymbolQuerySuccess old) = SymbolQueryDup (map (ann . sympName) [prop, old])
  amend _ (SymbolQueryDup dups) = SymbolQueryDup (ann (sympName prop) : dups)

importSymbols :: ImportDecl l -> SymbolTable -> SymbolTable -> SymbolTable
importSymbols = undefined
