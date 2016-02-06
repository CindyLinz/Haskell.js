module CollectData
  ( collectData
  , CollectDataResult (..)
  , DataShape (..)
  , DataShapes
  , IndexDataShapes
  , exportData
--  , qNameAddModule
--  , dataShapeAddModule
--  , dataShapesAddModule
--  , collectDataResultAddModule
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
type IndexDataShapes = M.Map (QName ()) (Int, DataShape)

--qNameAddModule :: ModuleName () -> QName () -> QName ()
--qNameAddModule mod = \case
--  UnQual _ name -> Qual () mod name
--  others -> others
--
--dataShapeAddModule :: ModuleName () -> DataShape -> DataShape
--dataShapeAddModule mod (DataShape loc name cons) = DataShape
--  loc
--  (qNameAddModule mod name)
--  (map (\(conName, slotN, rIndices) -> (qNameAddModule mod conName, slotN, rIndices)) cons)
--
--dataShapesAddModule :: ModuleName () -> DataShapes -> DataShapes
--dataShapesAddModule mod shapes = M.fromList $ map (qNameAddModule mod *** dataShapeAddModule mod) $ M.toList shapes
--
--indexDataShapesAddModule :: ModuleName () -> IndexDataShapes -> IndexDataShapes
--indexDataShapesAddModule mod shapes = M.fromList $ map (qNameAddModule mod *** second (dataShapeAddModule mod)) $ M.toList shapes
--
--collectDataResultAddModule :: ModuleName () -> CollectDataResult -> CollectDataResult
--collectDataResultAddModule mod (CollectDataResult tyToShapes conToShapes err) =
--  CollectDataResult (dataShapesAddModule mod tyToShapes) (indexDataShapesAddModule mod conToShapes) err

data CollectDataResult = CollectDataResult
  { dataTypeToShape :: DataShapes
  , dataConToShape :: IndexDataShapes
  , dataError :: [String]
  } deriving Show
instance Monoid CollectDataResult where
  mempty = CollectDataResult mempty mempty mempty
  mappend (CollectDataResult formerType formerCon formerErr) (CollectDataResult laterType laterCon laterErr) =
    CollectDataResult (formerType <> laterType) (formerCon <> laterCon) (formerErr <> laterErr <> mergeTypeErr <> mergeConErr) where
      mergeTypeErr = map (genTypeMsg . fst) . M.toList $ M.intersection formerType laterType
      mergeConErr = map (genConMsg . fst) . M.toList $ M.intersection formerCon laterCon
      genTypeMsg name = "Duplicated data definitions for " ++ show name ++ " at " ++ show (dataLoc (formerType M.! name)) ++ " and " ++ show (dataLoc (laterType M.! name))
      genConMsg name = "Duplicated data constructors for " ++ show name ++ " at " ++ show (dataLoc (snd $ formerCon M.! name)) ++ " and " ++ show (dataLoc (snd $ laterCon M.! name))

exportData :: forall l. ModuleName l -> Maybe (ExportSpecList l) -> CollectDataResult -> CollectDataResult
exportData _ Nothing = id
exportData modName' (Just (ExportSpecList _ specs)) = \CollectDataResult{dataTypeToShape=allType, dataConToShape=allCon} ->
  let
    modName = forgetL modName'

    extract :: ExportSpec l -> CollectDataResult
    extract (EVar _ _) =
      mempty
    extract (EAbs _ (NoNamespace _) qname) = case forgetL qname of
      UnQual _ name -> CollectDataResult
        { dataTypeToShape = M.singleton key shape
        , dataConToShape = mempty
        , dataError = mempty
        }
        where
          shape = allType M.! key
          key = Qual () modName name
      Qual _ exportModName name -> error $ "export with qualified name is not supported (" ++ show (forgetL qname) ++ ")"
      Special _ name -> error $ "export special type is not supported (" ++ show name ++ ")"
    extract (EThingAll _ qname) = case forgetL qname of
      UnQual _ name -> CollectDataResult
        { dataTypeToShape = M.singleton key shape
        , dataConToShape = M.fromList $ map (\(name, i, _) -> (name, (i, shape))) (dataCons shape)
        , dataError = mempty
        }
        where
          shape = allType M.! key
          key = Qual () modName name
      Qual _ exportModName name -> error $ "export with qualified name is not supported (" ++ show (forgetL qname) ++ ")"
      Special _ name -> error $ "export special type is not supported (" ++ show name ++ ")"
    extract (EThingWith _ qname fnames) = case forgetL qname of
      UnQual _ name -> CollectDataResult
        { dataTypeToShape = M.singleton key shape
        , dataConToShape =
          M.fromList $
            filter (isVisibleCon . fst) $
            map (\(name, i, _) -> (name, (i, shape)))
            (dataCons shape)
        , dataError = mempty
        }
        where
          shape = allType M.! key
          key = Qual () modName name
          isVisibleCon conName = getAny $ flip foldMap (map forgetL fnames) $ \case
            VarName _ _ -> mempty
            ConName _ fname -> Any $ name == fname
      Qual _ exportModName name -> error $ "export with qualified name is not supported (" ++ show (forgetL qname) ++ ")"
      Special _ name -> error $ "export special type is not supported (" ++ show name ++ ")"
    extract (EModuleContents _ exportModName) = error $ "module re-export is not supported (" ++ show (forgetL exportModName) ++ ")"
  in
    foldMap extract specs

declHeadName :: DeclHead l -> QName l
declHeadName (DHead l name) = UnQual l name
declHeadName (DHInfix l tyVarBind name) = UnQual l name
declHeadName (DHParen l head) = declHeadName head
declHeadName (DHApp l head tyVarBind) = declHeadName head

collectData :: Module SrcSpanInfo -> CollectDataResult
collectData (Module loc modHead _pragmas _imports decls) =
  mconcat (map (collectDataDecl modName) decls)
  where
    modName = maybe (ModuleName loc "Main") (\(ModuleHead _ modName _ _) -> modName) modHead

collectDataDecl :: ModuleName SrcSpanInfo -> Decl SrcSpanInfo -> CollectDataResult
collectDataDecl modName (DataDecl loc dn cxt (forgetL . declHeadName -> name) cons derivings) = CollectDataResult typeShapes conShapes errs
  where
    typeShapes = M.singleton name shape
    conShapes = M.fromList $ zipWith (\i (name, _, _) -> (name, (i, shape))) [0..] (dataCons shape)
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
      ConDecl l name tys -> (Qual () (forgetL modName) (forgetL name), length tys, M.empty)
      InfixConDecl l ty1 name ty2 -> (Qual () (forgetL modName) (forgetL name), 2, M.empty)
      RecDecl l name fields -> (Qual () (forgetL modName) (forgetL name), consSlotsNum, fieldsIndices) where
        consSlotsNum = sumFieldsSlotCount fields
        fieldsIndices = collectFieldsIndices fields
collectDataDecl modName (GDataDecl loc dn cxt (forgetL . declHeadName -> name) kind cons derivings) = CollectDataResult typeShapes conShapes errs
  where
    typeShapes = M.singleton name shape
    conShapes = M.fromList $ zipWith (\i (name, _, _) -> (name, (i, shape))) [0..] (dataCons shape)
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

    extract (GadtDecl _loc name recs ty) = (Qual () (forgetL modName) (forgetL name), consSlotsNum, fieldsIndices) where
      consSlotsNum = maybe 0 sumFieldsSlotCount recs + countTySlots 0 ty
      fieldsIndices = maybe M.empty collectFieldsIndices recs
    countTySlots !acc (TyFun l _ remain) = countTySlots (acc + 1) remain
    countTySlots acc _ = acc
collectDataDecl _ _ = CollectDataResult M.empty M.empty []

collectFieldsIndices :: [FieldDecl l] -> M.Map (Name ()) Int
collectFieldsIndices = M.fromList . flip zip [0..] . map forgetL . mconcat . map (\(FieldDecl l names ty) -> names)

sumFieldsSlotCount :: [FieldDecl l] -> Int
sumFieldsSlotCount = sum . map (\(FieldDecl l names ty) -> length names)
