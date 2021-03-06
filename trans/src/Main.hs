module Main where

import Language.Haskell.Exts.Annotated

import Data.Monoid

import Opt
import Parse

import RuntimeSource
import CollectData
import SymbolTable
import BasicTrans

import Desugar
import DesugarClass

import Desugar.Where
import Desugar.CaseReorder
import Desugar.String
import Desugar.List
import Desugar.Tuple
import Desugar.LambdaCase
import Desugar.PatBind

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

desugarModule dataConSymTable =
  deCaseReorderModule dataConSymTable .
  deTupleModule .
  deListModule .
  deStringModule .
  deLambdaCaseModule .
  dePatBindModule .
  deWhereModule

modName :: Module l -> ModuleName l
modName (Module l Nothing _ _ _) = ModuleName l "Main"
modName (Module _ (Just (ModuleHead _ modName _ _)) _ _ _) = modName

modExport :: Module l -> Maybe (ExportSpecList l)
modExport (Module _ Nothing _ _ _) = Nothing
modExport (Module _ (Just (ModuleHead _ _ _ exp)) _ _ _) = exp

modImport :: Module l -> [ImportDecl l]
modImport (Module _ _ _ imps _) = imps

importPrelude :: ImportDecl l
importPrelude = ImportDecl
  { importAnn = undefined
  , importModule = ModuleName undefined "Prelude"
  , importQualified = False
  , importSrc = False
  , importSafe = True
  , importPkg = Nothing
  , importAs = Nothing
  , importSpecs = Nothing
  }

main = do
  Options{..} <- getOpts
  inputStr <- getContents
  res <- getAllModules inputStr
  case res of
    ParseOk allMods ->
      prettyPrintAllModules (fmap (\mod -> snd (unDesugar (desugar mod) initDesugarState)) allMods)
    ParseFailed loc msg ->
      putStrLn $ msg ++ " at " ++ show loc

--  interact $ \inputStr ->
--    case parseModuleWithMode (myParseMode "mySource.hs") inputStr of
--      ParseFailed loc msg -> "parse failed at " ++ show loc ++ ": " ++ msg
--      ParseOk mod ->
--        case parseModuleWithMode (myParseMode "Prelude.hs") srcPrelude of
--          ParseFailed loc msg -> "parse Prelude failed at " ++ show loc ++ ": " ++ msg
--          ParseOk preludeMod ->
--            let
--              preludeData = collectData preludeMod
--              exportedPreludeData = exportData (modName preludeMod) (modExport preludeMod) preludeData
--
--              mainData = collectData mod
--              exportedMainData = exportData (modName mod) (modExport mod) mainData
--
--              exportedAllData = exportedPreludeData <> exportedMainData
--
--              desugarredPrelude = desugarModule (queryDataCon' mempty (dataConToShape preludeData) (modName preludeMod) []) preludeMod
--              desugarredMain = desugarModule (queryDataCon' (dataConToShape exportedAllData) (dataConToShape mainData) (modName mod) (importPrelude : modImport mod)) mod
--            in
--              {-
--              show preludeData ++
--              "\n\n" ++
--              show mainData ++
--              "\n\n" ++
--              show allData ++
--              "\n\n" ++
--              -}
--              if optDesugar then
--                prettyPrint desugarredMain ++ "\n"
--              else
--                genInit ++ genPreludeNative ++ transModule desugarredPrelude ++ transModule desugarredMain ++ genRun
