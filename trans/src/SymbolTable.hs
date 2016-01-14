module SymbolTable where

deIfModule (Module l moduleHead modulePragma importDecl decl) = Module (id l) (fmap (deIfModuleHead) moduleHead) (fmap (deIfModulePragma) modulePragma) (fmap (deIfImportDecl) importDecl) (fmap (deIfDecl) decl)
