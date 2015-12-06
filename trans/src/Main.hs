module Main where

import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Extension
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Fixity (preludeFixities)

import Data.Monoid
import Data.Char
-- import qualified Data.Map.Strict as M

import RuntimeSource

import DeIf
import DeList

myParseMode filename = ParseMode
  { parseFilename = filename
  , baseLanguage = Haskell2010
  , extensions = map EnableExtension
    [ OverlappingInstances
    , UndecidableInstances
    , IncoherentInstances
    , InstanceSigs
    , DoRec
    , RecursiveDo
    , ParallelListComp
    , MultiParamTypeClasses
    , FunctionalDependencies
    , RankNTypes
    , PolymorphicComponents
    , ExistentialQuantification
    , ScopedTypeVariables
    , ImplicitParams --
    , FlexibleContexts
    , FlexibleInstances
    , EmptyDataDecls
    , CPP
    , KindSignatures --
    , BangPatterns
    , TypeSynonymInstances
    , TemplateHaskell --
    , ForeignFunctionInterface --
    , Arrows
    , Generics
    , ImplicitPrelude --
    , NamedFieldPuns
    , PatternGuards
    , GeneralizedNewtypeDeriving
    , HereDocuments -- Hugs98 的功能, 看想不想做
    , MagicHash --
    , BinaryLiterals
    , TypeFamilies
    , StandaloneDeriving
    , UnicodeSyntax
    , LiberalTypeSynonyms
    , TypeOperators
    , ParallelArrays
    , RecordWildCards
    , DisambiguateRecordFields
    , OverloadedStrings
    , GADTs
    , RelaxedPolyRec
    , ExtendedDefaultRules
    , UnboxedTuples
    , DeriveDataTypeable
    , ConstrainedClassMethods
    , PackageImports --
    , LambdaCase
    , NewQualifiedOperators
    , PostfixOperators
    , QuasiQuotes --
    , TransformListComp
    , ViewPatterns
    , TupleSections
    , DoAndIfThenElse
    , RebindableSyntax
    , ExplicitForAll
    , DeriveFunctor
    , DeriveGeneric
    , DeriveTraversable
    , DeriveFoldable
    , NondecreasingIndentation
    , ExplicitNamespaces
    , DataKinds --
    , PolyKinds --
    , MultiWayIf
    , DefaultSignatures
    , ConstraintKinds
    ]
  , ignoreLanguagePragmas = True
  , ignoreLinePragmas = False
  , fixities = Just preludeFixities
  , ignoreFunctionArity = True
  }

escapeJSString :: String -> String
escapeJSString ('\'' : others) = '\\' : '\'' : escapeJSString others
escapeJSString ('\\' : others) = '\\' : '\\' : escapeJSString others
escapeJSString (ch : others) = ch : escapeJSString others
escapeJSString _ = ""

transName :: Name -> String
transName (Ident name) = "'" ++ escapeJSString name ++ "'"
transName (Symbol symbol) = "'(" ++ escapeJSString symbol ++ ")'"

transQName :: QName -> String
transQName (UnQual name) = "['var'," ++ transName name ++ "]"
transQName (Special UnitCon) = "['var','()']"
transQName (Special ListCon) = "['var','[]']"
transQName (Special (TupleCon boxed n)) = "['var','(" ++ replicate (n - 1) ',' ++ ")']"
transQName (Special Cons) = "['var','(:)']"
transQName (Special UnboxedSingleCon) = "['var','()']"
transQName (Qual _ _) = error "qualified name unimplemented"

transLit :: Literal -> String
transLit (Char ch) = "['app', ['var', 'C#'], ['dat', String.fromCharCode(" ++ show (ord ch) ++ ")]]"
transLit (Int i) = "['app', ['var', 'I#'], ['dat', " ++ show i ++ "]]"

transLam :: [Pat] -> Exp -> String
transLam (PVar name : ps) body = "['lam'," ++ transName name ++ "," ++ transLam ps body ++ "]"
transLam _ body = transExpr body

transSign :: Sign -> String
transSign Signless = ""
transSign Negative = "-"

transExpr :: Exp -> String
transExpr (Var qName) = transQName qName
transExpr (Con qName) = transQName qName
transExpr (Lit lit) = transLit lit
transExpr (InfixApp a (QVarOp qName) b) = transExpr (App (App (Var qName) a) b)
transExpr (InfixApp a (QConOp qName) b) = transExpr (App (App (Var qName) a) b)
transExpr (App f x) = "['app'," ++ transExpr f ++ "," ++ transExpr x ++ "]"
transExpr (NegApp x) = error "NegApp not defined"
transExpr (Lambda loc pats body) = transLam pats body
transExpr (Paren expr) = transExpr expr
transExpr (List []) = "['var','[]']"
transExpr (Let (BDecls binds) expr) =
  "['app', ['app', ['var', 'Y#'], ['lam', 'gen#', ['lam', 'tuple#', " ++ genApp (reverse binds) ++ "]]], " ++ genIn binds ++ "]"
  where
    genApp (PatBind _ (PVar name) _ _ : bs) = "['app', " ++ genApp bs ++ ", ['app', ['var', 'gen#'], " ++ genExtract binds ++ "]]"
      where
        genExtract (PatBind _ (PVar lamName) _ _ : bs) = "['lam', " ++ transName lamName ++ ", " ++ genExtract bs ++ "]"
        genExtract _ = "['var', " ++ transName name ++ "]"
    genApp _ = genDestruct binds
    genDestruct (PatBind _ (PVar name) _ _ : bs) = "['lam', " ++ transName name ++ ", " ++ genDestruct bs ++ "]"
    genDestruct _ = genDef (reverse binds)
    genDef (PatBind _ _ (UnGuardedRhs expr) _ : bs) = "['app', " ++ genDef bs ++ ", " ++ transExpr expr ++ "]"
    genDef _ = "['var', 'tuple#']"
    genIn (PatBind _ (PVar name) _ _ : bs) = "['lam', " ++ transName name ++ ", " ++ genIn bs ++ "]"
    genIn _ = transExpr expr
transExpr (Case target alts) = case alts of
  (Alt _ (PVar name) (UnGuardedRhs expr) Nothing : _) -> -- case target of name -> expr
    "['app',['lam'," ++ transName name ++ "," ++ transExpr expr ++ "]," ++ transExpr target ++ "]"
  (Alt _ (PLit _ (Int _)) _ _ : _) -> -- 整數 literal: 1, 2, 3, ..
    "['app', " ++ transExpr target ++ ", " ++ genPrimIntMatch alts ++ "]"
  (Alt _ (PLit _ (PrimInt _)) _ _ : _) -> -- unbox 整數 literal: 1#, 2#, 3#, ...
    "['app', " ++ genPrimIntMatch alts ++ ", " ++ transExpr target ++ "]"
  [Alt _ (PList []) (UnGuardedRhs exprNil) _, Alt _ (PApp _ [PVar aName, PVar asName]) (UnGuardedRhs exprCons) _ ] -> -- (G)ADT (for list)
    "['app', ['app', " ++ transExpr target ++ ", " ++ transExpr exprNil ++ "], ['lam', " ++ transName aName ++ ", ['lam', " ++ transName asName ++ ", " ++ transExpr exprCons ++ "]]]"
  (Alt _ (PApp _ _) _ _ : _) -> -- (G)ADT
    genApp (reverse alts)
    where
      genApp (Alt _ (PApp (UnQual conName) vars) (UnGuardedRhs expr) Nothing : as) =
        "['app', " ++ genApp as ++ ", " ++ genLam vars ++ "]"
        where
          genLam (PVar name : vs) = "['lam', " ++ transName name ++ ", " ++ genLam vs ++ "]"
          genLam (PWildCard : vs) = "['lam', '_', " ++ genLam vs ++ "]"
          genLam _ = transExpr expr
      genApp _ = transExpr target
  _ -> error $ show alts ++ " unimplemented case pattern"
transExpr others = error $ show others ++ " not implemented"

genPrimIntMatch :: [Alt] -> String
genPrimIntMatch alts =
  "['int', 'match-int#', 1, function(target){\n\
    \  var env = this;\n\
    \  target = weak_normal_form(target);\n\
    \  switch(target.expr[1]){\n" ++
    mconcat (flip map alts $ genBranch) ++
    "  }\n\
  \}]"
  where
    genBranch (Alt _ (PLit sign (PrimInt n)) rhs Nothing) =
      "case " ++ transSign sign ++ show n ++ ": " ++ genRHS rhs
    genBranch (Alt _ (PLit sign (Int n)) rhs Nothing) =
      "case " ++ transSign sign ++ show n ++ ": " ++ genRHS rhs
    genBranch (Alt _ (PVar name) rhs Nothing) =
      "default: env = clone_env(env); env[" ++ transName name ++ "] = target; " ++ genRHS rhs
    genBranch (Alt _ PWildCard rhs Nothing) =
      "default: " ++ genRHS rhs
    genBranch alt = error $ show alt ++ " not implemented PrimInt branch"
    genRHS (UnGuardedRhs expr) = "return weak_normal_form({env: env, expr: " ++ transExpr expr ++ "});\n"

transDecl :: Decl -> String
transDecl (PatBind loc (PVar name) (UnGuardedRhs expr) Nothing) =
  "env[" ++ transName name ++ "] = {env: env, expr: " ++ transExpr expr ++ "};\n\n"
transDecl (GDataDecl loc DataType [] name [] mKinds gDecls []) = mconcat $ flip map gDecls $ \(GadtDecl loc name [] ty) ->
  let
    slotCount = count 0 ty where
      count !acc (TyFun _ other) = count (acc + 1) other
      count !acc _ = acc

    genSlots 0 = genBody gDecls
    genSlots n = "['lam','a" ++ show (slotCount - n + 1) ++ "'," ++ genSlots (n - 1) ++ "]"

    genBody (GadtDecl _ name _ _ : ds) = "['lam'," ++ transName name ++ "," ++ genBody ds ++ "]"
    genBody _ = genApp slotCount

    genApp 0 = "['var'," ++ transName name ++ "]"
    genApp n = "['app'," ++ genApp (n - 1) ++ ",['var','a" ++ show n ++ "']]"
  in
    "env[" ++ transName name ++ "] = {env: env, expr: " ++ genSlots slotCount ++ "};\n\n"
transDecl decl = error $ show decl ++ " not implemented"

transModule :: Module -> String
transModule (Module moduleLoc moduleName pragmas mWarnings moduleExports moduleImports moduleDecls) =
  mconcat $ map transDecl moduleDecls

--newtype GadtSet = GadtSet [(Name, Int)]

--buildConType :: [Module] -> M.Map Name GadtSet
--buildConType mods = mconcat $ flip map mods $ \(Module moduleLoc moduleName pragmas mWarnings moduleExports moduleImports moduleDecls) ->
--  mconcat $ flip map moduleDecls $ \case
--    GDataDecl _ _ _ _ _ _ decls _ ->
--      let
--        countSlot :: Type -> Int
--        countSlot ty = go 0 ty where
--          go !acc (TyApp _ others) = go (acc + 1) others
--          go !acc _ = acc
--
--        set = GadtSet $ flip map decls $ \(GadtDecl _ name _ ty) -> (name, countSlot ty)
--      in
--        M.fromList $ flip map decls $ \(GadtDecl _ name _ _) -> (name, set)
--
--    _ -> M.empty

desugarModule = deIfModule . deListModule

main = interact $ \inputStr ->
  case parseWithMode (myParseMode "mySource.hs") inputStr of
    ParseFailed loc msg -> "parse failed at " ++ show loc ++ ": " ++ msg
    ParseOk mod ->
      case parseWithMode (myParseMode "Prelude.hs") srcPrelude of
        ParseFailed loc msg -> "parse Prelude failed at " ++ show loc ++ ": " ++ msg
        ParseOk preludeMod ->
          genInit ++ genPreludeNative ++ transModule (desugarModule preludeMod) ++ transModule (desugarModule mod) ++ genRun
