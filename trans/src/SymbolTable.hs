module SymbolTable
  ( queryDataCon
  , queryDataCon'
  ) where

import Language.Haskell.Exts.Annotated

import qualified Data.Map.Strict as M

import ForgetL
import CollectData

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

queryDataCon
  :: IndexDataShapes -- all exported data constructors
  -> IndexDataShapes -- data constructors of this module
  -> [ImportDecl l1]
  -> QName l2
  -> Maybe (Int, DataShape)
queryDataCon expConMap selfConMap imports qname =
  case qname of
    Qual _ _ _ -> M.lookup (forgetL qname) conMap
    UnQual _ name -> undefined
    Special _ spc -> case spc of
      UnitCon _ -> Just
        ( 0
        , DataShape
          { dataLoc = dummySrcSpanInfo
          , dataName = Special () (UnitCon ())
          , dataCons = [(Special () (UnitCon ()), 0, M.empty)]
          }
        )
      ListCon _ -> Nothing
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
        , DataShape
          { dataLoc = dummySrcSpanInfo
          , dataName = Special () (ListCon ())
          , dataCons =
            [ (Special () (ListCon ()), 0, M.empty)
            , (Special () (Cons ()), 2, M.empty)
            ]
          }
        )
      UnboxedSingleCon _ -> Just
        ( 0
        , DataShape
          { dataLoc = dummySrcSpanInfo
          , dataName = Special () (UnboxedSingleCon ())
          , dataCons = [(Special () (UnboxedSingleCon ()), 1, M.empty)]
          }
        )


queryDataCon' :: CollectDataResult -> [ImportDecl l1] -> QName l2 -> (Int, DataShape)
queryDataCon' c i q =
  case queryDataCon c i q of
    Just res -> res
    _ -> error $ "Can't find " ++ show (forgetL q) ++ " in " ++ show (dataConToShape c)
