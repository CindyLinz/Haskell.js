module SymbolTable
  ( queryDataCon
  , queryDataCon'
  , SymbolTableDataCon
  , SymbolTableDataCon'
  ) where

import Language.Haskell.Exts.Annotated

import qualified Data.Map.Strict as M
--import Data.Foldable

import ForgetL
import CollectData

type SymbolTableDataCon l = QName l -> Maybe (Int, DataShape)
type SymbolTableDataCon' l = QName l -> (Int, DataShape)

dummySrcSpanInfo :: SrcSpanInfo
dummySrcSpanInfo = SrcSpanInfo
  { srcInfoSpan = SrcSpan
    { srcSpanFilename = "(heaven)"
    , srcSpanStartLine = -1
    , srcSpanStartColumn = -1
    , srcSpanEndLine = -1
    , srcSpanEndColumn = -1
    }
  , srcInfoPoints = []
  }

listShape :: DataShape
listShape = DataShape
  { dataLoc = dummySrcSpanInfo
  , dataName = Special () (ListCon ())
  , dataCons =
    [ (Special () (ListCon ()), 0, M.empty)
    , (Special () (Cons ()), 2, M.empty)
    ]
  }

queryDataCon
  :: IndexDataShapes -- all exported data constructors
  -> IndexDataShapes -- data constructors of this module
  -> ModuleName l1
  -> [ImportDecl l2]
  -> QName l3
  -> Maybe (Int, DataShape)
queryDataCon expConMap selfConMap selfModName' imports' qname' =
  let
    imports = map forgetL imports'
    qname = forgetL qname'
    selfModName = forgetL selfModName'
  in
    case qname of
      Qual _ modName name -> -- M.lookup (forgetL qname) conMap
        let
          founds = flip foldMap (map forgetL imports) $ \ImportDecl{..} ->
            let
              needLookThisMod = modName == maybe importModule id importAs
            in
              if needLookThisMod then
                maybe [] pure (M.lookup (Qual () importModule name) expConMap)
              else
                []
        in
          if length founds == 1 then
            Just (head founds)
          else
            Nothing

      UnQual _ name ->
        let
          foundExp = flip foldMap (map forgetL imports) $ \ImportDecl{..} ->
            if not importQualified then
              maybe [] pure (M.lookup (Qual () importModule name) expConMap)
            else
              []
          foundCurr = maybe [] pure (M.lookup (Qual () selfModName name) selfConMap)
          founds = foundCurr ++ foundExp
        in
          if length founds == 1 then
            Just (head founds)
          else
            Nothing

      Special _ spc -> case spc of
        UnitCon _ -> Just
          ( 0
          , DataShape
            { dataLoc = dummySrcSpanInfo
            , dataName = Special () (UnitCon ())
            , dataCons = [(Special () (UnitCon ()), 0, M.empty)]
            }
          )
        ListCon _ -> Just
          ( 0
          , listShape
          )
        FunCon _ -> Nothing
        TupleCon _ boxed size -> Just
          ( 0
          , DataShape
            { dataLoc = dummySrcSpanInfo
            , dataName = Special () (TupleCon () boxed size)
            , dataCons = [(Special () (TupleCon () boxed size), size, M.empty)]
            }
          )
        Cons _ -> Just
          ( 1
          , listShape
          )
        UnboxedSingleCon _ -> Just
          ( 0
          , DataShape
            { dataLoc = dummySrcSpanInfo
            , dataName = Special () (UnboxedSingleCon ())
            , dataCons = [(Special () (UnboxedSingleCon ()), 1, M.empty)]
            }
          )

queryDataCon'
  :: IndexDataShapes -- all exported data constructors
  -> IndexDataShapes -- data constructors of this module
  -> ModuleName l1
  -> [ImportDecl l2]
  -> QName l3 -> (Int, DataShape)
queryDataCon' all curr currMod i q =
  case queryDataCon all curr currMod i q of
    Just res -> res
    _ -> error $ "Can't find " ++ show (forgetL q) ++ " in " ++ show all ++ " and " ++ show curr
