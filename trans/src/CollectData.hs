module CollectData where
import Language.Haskell.Exts.Annotated
import Control.Arrow ((***))

import qualified Data.Map.Strict as M
import Data.Monoid

data DataShape = DataShape
  { dataLoc :: SrcSpanInfo
  , dataName :: Name ()
  , dataCons :: [(Name (), Int)] -- [(data constructor name, number of slot)]
  } deriving Show
type DataShapes = M.Map (Name ()) DataShape -- data type name to data shape

data CollectDataResult = CollectDataResult DataShapes [String]
  deriving Show
instance Monoid CollectDataResult where
  mempty = CollectDataResult M.empty []
  mappend (CollectDataResult former formerErr) (CollectDataResult later laterErr) =
    CollectDataResult (former <> later) (formerErr <> laterErr <> mergeErr) where
      mergeErr = map (genMsg . fst) . M.toList $ M.intersection former later
      genMsg name = "Duplicated data definition for " ++ show name ++ " at " ++ show (dataLoc (former M.! name)) ++ " and " ++ show (dataLoc (later M.! name))

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
  mconcat (map (\decl -> CollectDataResult (collectDataDecl decl) []) decls)

collectDataDecl :: Decl SrcSpanInfo -> DataShapes
collectDataDecl (DataDecl loc dn cxt (forgotNameL . declHeadName -> name) cons derivings) = M.singleton name $ DataShape
  { dataLoc = loc
  , dataName = forgotNameL name
  , dataCons = map extract cons
  }
  where
    extract (QualConDecl _loc _tyVars cxt decl) = case decl of
      ConDecl l name tys -> (forgotNameL name, length tys)
      InfixConDecl l ty1 name ty2 -> (forgotNameL name, 2)
      RecDecl l name fields -> (forgotNameL name, sum (map (\(FieldDecl l names ty) -> length names) fields))
collectDataDecl (GDataDecl loc dn cxt (forgotNameL . declHeadName -> name) kind cons derivings) = M.singleton name $ DataShape
  { dataLoc = loc
  , dataName = forgotNameL name
  , dataCons = map extract cons
  }
  where
    extract (GadtDecl _loc name recs ty) = (forgotNameL name, maybe 0 (sum . map (\(FieldDecl l names ty) -> length names)) recs + countTySlots 0 ty)
    countTySlots !acc (TyFun l _ remain) = countTySlots (acc + 1) remain
    countTySlots acc _ = acc
collectDataDecl _ = M.empty
