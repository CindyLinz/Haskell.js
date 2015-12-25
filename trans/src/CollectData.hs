module CollectData
  ( collectData
  , CollectDataResult (..)
  , DataShape (..)
  , DataShapes
  , qNameAddModule
  , dataShapeAddModule
  , dataShapesAddModule
  , collectDataResultAddModule
  ) where
import Language.Haskell.Exts.Annotated
import Control.Arrow

import qualified Data.Map.Strict as M
import Data.Monoid
import ForgetL

data DataShape = DataShape
  { dataLoc :: SrcSpanInfo
  , dataName :: QName ()
  , dataCons :: [(QName (), Int, M.Map (Name ()) Int)] -- [(data constructor name, number of slot, record field's index)]
  } deriving Show
type DataShapes = M.Map (QName ()) DataShape -- data type name to data shape

qNameAddModule :: ModuleName () -> QName () -> QName ()
qNameAddModule mod = \case
  UnQual _ name -> Qual () mod name
  others -> others

dataShapeAddModule :: ModuleName () -> DataShape -> DataShape
dataShapeAddModule mod (DataShape loc name cons) = DataShape
  loc
  (qNameAddModule mod name)
  (map (\(conName, slotN, rIndices) -> (qNameAddModule mod conName, slotN, rIndices)) cons)

dataShapesAddModule :: ModuleName () -> DataShapes -> DataShapes
dataShapesAddModule mod shapes = M.fromList $ map (qNameAddModule mod *** dataShapeAddModule mod) $ M.toList shapes

collectDataResultAddModule :: ModuleName () -> CollectDataResult -> CollectDataResult
collectDataResultAddModule mod (CollectDataResult tyToShapes conToShapes err) =
  CollectDataResult (dataShapesAddModule mod tyToShapes) (dataShapesAddModule mod conToShapes) err

data CollectDataResult = CollectDataResult
  { dataTypeToShape :: DataShapes
  , dataConToShape :: DataShapes
  , dataError :: [String]
  } deriving Show
instance Monoid CollectDataResult where
  mempty = CollectDataResult M.empty M.empty []
  mappend (CollectDataResult formerType formerCon formerErr) (CollectDataResult laterType laterCon laterErr) =
    CollectDataResult (formerType <> laterType) (formerCon <> laterCon) (formerErr <> laterErr <> mergeTypeErr <> mergeConErr) where
      mergeTypeErr = map (genTypeMsg . fst) . M.toList $ M.intersection formerType laterType
      mergeConErr = map (genConMsg . fst) . M.toList $ M.intersection formerCon laterCon
      genTypeMsg name = "Duplicated data definitions for " ++ show name ++ " at " ++ show (dataLoc (formerType M.! name)) ++ " and " ++ show (dataLoc (laterType M.! name))
      genConMsg name = "Duplicated data constructors for " ++ show name ++ " at " ++ show (dataLoc (formerCon M.! name)) ++ " and " ++ show (dataLoc (laterCon M.! name))

declHeadName :: DeclHead l -> QName l
declHeadName (DHead l name) = UnQual l name
declHeadName (DHInfix l tyVarBind name) = UnQual l name
declHeadName (DHParen l head) = declHeadName head
declHeadName (DHApp l head tyVarBind) = declHeadName head

collectData :: Module SrcSpanInfo -> CollectDataResult
collectData (Module _loc _name _pragmas _imports decls) =
  mconcat (map collectDataDecl decls)

collectDataDecl :: Decl SrcSpanInfo -> CollectDataResult
collectDataDecl (DataDecl loc dn cxt (forgetL . declHeadName -> name) cons derivings) = CollectDataResult typeShapes conShapes errs
  where
    typeShapes = M.singleton name shape
    conShapes = M.fromList $ map (\(name, _, _) -> (name, shape)) (dataCons shape)
    errs =
      if M.size conShapes == length cons then
        []
      else
        ["Duplicated data constructors in " ++ show name ++ " at " ++ show loc]

    shape = DataShape
      { dataLoc = loc
      , dataName = name
      , dataCons = map extract cons
      }

    extract (QualConDecl _loc _tyVars cxt decl) = case decl of
      ConDecl l name tys -> (forgetL (UnQual l name), length tys, M.empty)
      InfixConDecl l ty1 name ty2 -> (forgetL (UnQual l name), 2, M.empty)
      RecDecl l name fields -> (forgetL (UnQual l name), consSlotsNum, fieldsIndices) where
        consSlotsNum = sumFieldsSlotCount fields
        fieldsIndices = collectFieldsIndices fields
collectDataDecl (GDataDecl loc dn cxt (forgetL . declHeadName -> name) kind cons derivings) = CollectDataResult typeShapes conShapes errs
  where
    typeShapes = M.singleton name shape
    conShapes = M.fromList $ map (\(name, _, _) -> (name, shape)) (dataCons shape)
    errs =
      if M.size conShapes == length cons then
        []
      else
        ["Duplicated data constructors in " ++ show name ++ " at " ++ show loc]

    shape = DataShape
      { dataLoc = loc
      , dataName = name
      , dataCons = map extract cons
      }

    extract (GadtDecl _loc name recs ty) = (forgetL (UnQual undefined name), consSlotsNum, fieldsIndices) where
      consSlotsNum = maybe 0 sumFieldsSlotCount recs + countTySlots 0 ty
      fieldsIndices = maybe M.empty collectFieldsIndices recs
    countTySlots !acc (TyFun l _ remain) = countTySlots (acc + 1) remain
    countTySlots acc _ = acc
collectDataDecl _ = CollectDataResult M.empty M.empty []

collectFieldsIndices :: [FieldDecl l] -> M.Map (Name ()) Int
collectFieldsIndices = M.fromList . flip zip [0..] . map forgetL . mconcat . map (\(FieldDecl l names ty) -> names)

sumFieldsSlotCount :: [FieldDecl l] -> Int
sumFieldsSlotCount = sum . map (\(FieldDecl l names ty) -> length names)
