module Main where

import Language.Haskell.Exts.Annotated

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

main = interact $ \inputStr ->
  let res = parseWithMode (myParseMode "mySource.hs") inputStr :: ParseResult (Module SrcSpanInfo)
  in show res
