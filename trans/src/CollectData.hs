module CollectData
  ( collectData
  , CollectDataResult (..)
  , DataShape (..)
  , DataShapes
  ) where
import Language.Haskell.Exts.Annotated
import Control.Arrow

import qualified Data.Map.Strict as M
import Data.Monoid

data DataShape = DataShape
  { dataLoc :: SrcSpanInfo
  , dataName :: Name ()
  , dataCons :: [(Name (), Int)] -- [(data constructor name, number of slot)]
  } deriving Show
type DataShapes = M.Map (Name ()) DataShape -- data type name to data shape

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

forgotNameL :: Name l -> Name ()
forgotNameL (Ident l str) = Ident () str
forgotNameL (Symbol l str) = Symbol () str

declHeadName :: DeclHead l -> Name l
declHeadName (DHead l name) = name
declHeadName (DHInfix l tyVarBind name) = name
declHeadName (DHParen l head) = declHeadName head
declHeadName (DHApp l head tyVarBind) = declHeadName head

collectData :: Module SrcSpanInfo -> CollectDataResult
collectData (Module _loc _name _pragmas _imports decls) =
  mconcat (map collectDataDecl decls)

collectDataDecl :: Decl SrcSpanInfo -> CollectDataResult
collectDataDecl (DataDecl loc dn cxt (forgotNameL . declHeadName -> name) cons derivings) = CollectDataResult typeShapes conShapes errs
  where
    typeShapes = M.singleton name shape
    conShapes = M.fromList $ map (second (const shape)) (dataCons shape)
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
      ConDecl l name tys -> (forgotNameL name, length tys)
      InfixConDecl l ty1 name ty2 -> (forgotNameL name, 2)
      RecDecl l name fields -> (forgotNameL name, sum (map (\(FieldDecl l names ty) -> length names) fields))
collectDataDecl (GDataDecl loc dn cxt (forgotNameL . declHeadName -> name) kind cons derivings) = CollectDataResult typeShapes conShapes errs
  where
    typeShapes = M.singleton name shape
    conShapes = M.fromList $ map (second (const shape)) (dataCons shape)
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

    extract (GadtDecl _loc name recs ty) = (forgotNameL name, maybe 0 (sum . map (\(FieldDecl l names ty) -> length names)) recs + countTySlots 0 ty)
    countTySlots !acc (TyFun l _ remain) = countTySlots (acc + 1) remain
    countTySlots acc _ = acc
collectDataDecl _ = CollectDataResult M.empty M.empty []
