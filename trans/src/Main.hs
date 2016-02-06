module Main where

import Language.Haskell.Exts.Annotated

import Data.Monoid

import Opt

import RuntimeSource
import CollectData
import BasicTrans

import Desugar.If
import Desugar.List
import Desugar.Where
import Desugar.CaseReorder

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

desugarModule0 = deIfModule . deListModule . deWhereModule
desugarModule dataShapes = deCaseReorderModule dataShapes . deIfModule . deListModule . deWhereModule

main = do
  Options{..} <- getOpts
  interact $ \inputStr ->
    case parseModuleWithMode (myParseMode "mySource.hs") inputStr of
      ParseFailed loc msg -> "parse failed at " ++ show loc ++ ": " ++ msg
      ParseOk mod ->
        case parseModuleWithMode (myParseMode "Prelude.hs") srcPrelude of
          ParseFailed loc msg -> "parse Prelude failed at " ++ show loc ++ ": " ++ msg
          ParseOk preludeMod ->
            let
              preludeData = collectData preludeMod
              mainData = collectData mod
              --preludeData = collectDataResultAddModule (ModuleName () "Prelude") $ collectData preludeMod
              --mainData = collectDataResultAddModule (ModuleName () "Main") $ collectData mod
              allData = preludeData <> mainData

              desugarredPrelude = desugarModule0 preludeMod
              desugarredMain = desugarModule allData mod
            in
              {-
              show preludeData ++
              "\n\n" ++
              show mainData ++
              "\n\n" ++
              show allData ++
              "\n\n" ++
              -}
              if optDesugar then
                prettyPrint desugarredMain ++ "\n"
              else
                genInit ++ genPreludeNative ++ transModule desugarredPrelude ++ transModule desugarredMain ++ genRun
