module Main where

import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Extension
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Fixity (preludeFixities)

import Data.Monoid
import Data.Char

import RuntimeSource

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

transName :: Name -> String
transName (Ident name) = name
transName (Symbol symbol) = "(" ++ symbol ++ ")"

transQName :: QName -> String
transQName (UnQual name) = "['var','" ++ transName name ++ "']"
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
--transExpr (Case target alts) = 
transExpr others = error $ show others ++ " not implemented"

transDecl :: Decl -> String
transDecl (PatBind loc (PVar name) (UnGuardedRhs expr) Nothing) =
  "env['" ++ transName name ++ "'] = {env: env, expr: " ++ transExpr expr ++ "};\n\n"
transDecl (GDataDecl loc DataType [] name [] mKinds gDecls []) = mconcat $ flip map gDecls $ \(GadtDecl loc name [] ty) ->
  let
    slotCount = count 0 ty where
      count !acc (TyFun _ other) = count (acc + 1) other
      count !acc _ = acc

    genSlots 0 = genBody gDecls
    genSlots n = "['lam','a" ++ show (slotCount - n + 1) ++ "'," ++ genSlots (n - 1) ++ "]"

    genBody (GadtDecl _ name _ _ : ds) = "['lam','is-" ++ transName name ++ "'," ++ genBody ds ++ "]"
    genBody _ = genApp slotCount

    genApp 0 = "['var','is-" ++ transName name ++ "']"
    genApp n = "['app'," ++ genApp (n - 1) ++ ",['var','a" ++ show n ++ "']]"
  in
    "env['" ++ transName name ++ "'] = {env: env, expr: " ++ genSlots slotCount ++ "};\n\n"
transDecl decl = error $ show decl ++ " not implemented"

transModule :: Module -> String
transModule (Module moduleLoc moduleName pragmas mWarnings moduleExports moduleImports moduleDecls) =
  mconcat $ map transDecl moduleDecls

main = interact $ \inputStr ->
  case parseWithMode (myParseMode "mySource.hs") inputStr of
    ParseFailed loc msg -> "parse failed at " ++ show loc ++ ": " ++ msg
    ParseOk mod ->
      case parseWithMode (myParseMode "Prelude.hs") srcPrelude of
        ParseFailed loc msg -> "parse Prelude failed at " ++ show loc ++ ": " ++ msg
        ParseOk preludeMod ->
          genInit ++ genPreludeNative ++ transModule preludeMod ++ transModule mod ++ genRun
