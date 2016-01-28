module DeCaseReorder where
import Language.Haskell.Exts.Annotated.Syntax
import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.SrcLoc
import Control.Arrow ((***))
import qualified Data.Map.Strict as M

import CollectData
import ForgetL
import Debug.Trace

data OrderedCase l
  = OrderedCase
    l
    (Name l) -- target variable name x0-, x1-..
    [FallbackGroup l]
  | OrderedCaseRHS (Rhs l) (Maybe (Binds l))
  | OrderedCaseFallback l
  deriving Show
instance Functor OrderedCase where
  fmap f (OrderedCase l name groups) = OrderedCase (f l) (fmap f name) (fmap (fmap f) groups)
  fmap f (OrderedCaseRHS rhs binds) = OrderedCaseRHS (fmap f rhs) (fmap (fmap f) binds)
  fmap f (OrderedCaseFallback l) = OrderedCaseFallback (f l)

data FallbackGroup l
  = GroupWildCard l (OrderedCase l)
  | GroupVar l (Name l) (OrderedCase l)
  | GroupCon l [FallbackGroupConBranch l]
  | GroupLit l [FallbackGroupLitBranch l]
  deriving Show
instance Functor FallbackGroup where
  fmap f (GroupWildCard l c) = GroupWildCard (f l) (fmap f c)
  fmap f (GroupVar l name c) = GroupVar (f l) (fmap f name) (fmap f c)
  fmap f (GroupCon l bs) = GroupCon (f l) (fmap (fmap f) bs)
  fmap f (GroupLit l bs) = GroupLit (f l) (fmap (fmap f) bs)

data FallbackGroupConBranch l = GroupConBranch
  l
  (QName l) -- constructor
  {-# UNPACK #-} !Int -- slot begin
  {-# UNPACK #-} !Int -- slot number
  (OrderedCase l)
  deriving Show
instance Functor FallbackGroupConBranch where
  fmap f (GroupConBranch l con b n c) = GroupConBranch (f l) (fmap f con) b n (fmap f c)
data FallbackGroupConBranchBuilding l = GroupConBranchBuilding
  l
  (QName l) -- constructor
  {-# UNPACK #-} !Int -- slot number
  [AltPartial l]
  deriving Show
instance Functor FallbackGroupConBranchBuilding where
  fmap f (GroupConBranchBuilding l con n alts) = GroupConBranchBuilding (f l) (fmap f con) n (fmap (fmap f) alts)

data AltPartial l = AltPartial
  [Pat l]
  (Rhs l)
  (Maybe (Binds l))
  deriving Show
instance Functor AltPartial where
  fmap f (AltPartial pats rhs binds) = AltPartial (fmap (fmap f) pats) (fmap f rhs) (fmap (fmap f) binds)

data FallbackGroupLitBranch l = GroupLitBranch
  l
  (Sign l)
  (Literal l)
  (OrderedCase l)
  deriving Show
instance Functor FallbackGroupLitBranch where
  fmap f (GroupLitBranch l s lit c) = GroupLitBranch (f l) (fmap f s) (fmap f lit) (fmap f c)

dataShapeToConBranches :: l -> DataShape -> [FallbackGroupConBranchBuilding l]
dataShapeToConBranches l shape =
  map (\(conName, slotN, _) -> GroupConBranchBuilding l (fmap (const l) conName) slotN []) (dataCons shape)

modifyListElem :: [a] -> Int -> (a -> a) -> [a]
modifyListElem as n f = go n as where
  go 0 (a:as) = f a : as
  go n (a:as) = a : go (n-1) as

altToPartial :: Alt l -> AltPartial l
altToPartial (Alt l pat rhs binds) = AltPartial [pat] rhs binds

buildReorder :: forall l. IndexDataShapes -> l -> [AltPartial l] -> OrderedCase l
buildReorder conMap l = sortPat 0 1
  where
    sortPat :: Int -> Int -> [AltPartial l] -> OrderedCase l
    sortPat b e [] = error "sortPat: empty alts"
    sortPat b e [AltPartial [] rhs binds] = OrderedCaseRHS rhs binds
    sortPat b e (AltPartial [] rhs binds : _) = error "buildReorder-sortPat: non-singleton leading empty pattern"
    sortPat b e alts = OrderedCase l (Ident l ("x" ++ show b ++ "-")) (grouping alts) where
      grouping :: [AltPartial l] -> [FallbackGroup l]
      grouping [] = []
      grouping (g:gs) = case g of
        AltPartial [] rhs binds -> error "buildReorder-grouping: empty pattern"
        AltPartial (PParen _ p : patsLater) rhs binds -> grouping (AltPartial (p : patsLater) rhs binds : gs)
        AltPartial (PWildCard _ : patsLater) rhs binds -> GroupWildCard l (sortPat (b+1) e [AltPartial patsLater rhs binds]) : grouping gs
        AltPartial (PVar _ name : patsLater) rhs binds -> GroupVar l name (sortPat (b+1) e [AltPartial patsLater rhs binds]) : grouping gs
        AltPartial (PApp _ name pats : patsLater) rhs binds ->
          let
            shape = snd $ conMap M.! {- traceShowId -} (forgetL name)
            branchesSeed = dataShapeToConBranches l shape

            eatCons :: [FallbackGroupConBranchBuilding l] -> [AltPartial l] -> ([FallbackGroupConBranch l], [AltPartial l])
            eatCons acc [] = (map buildBranch acc, [])
            eatCons acc gg@(g:gs) = {- trace ("eatCons " ++ show (length acc) ++ " " ++ show (forgetL g)) $ -} case g of
              AltPartial (PApp _ name pats : patsLater) rhs binds ->
                let
                  i = fst $ conMap M.! {- traceShowId -} (forgetL name)
                  acc' = modifyListElem acc i $ \br@(GroupConBranchBuilding l con slotNum os) ->
                    GroupConBranchBuilding l con slotNum (os ++ [AltPartial (pats ++ patsLater) rhs binds])
                in
                  eatCons acc' gs
              _ -> (map buildBranch acc, gg)

            buildBranch :: FallbackGroupConBranchBuilding l -> FallbackGroupConBranch l
            buildBranch (GroupConBranchBuilding l con slotNum os) = case os of
              [] -> GroupConBranch l con e slotNum (OrderedCaseFallback l)
              _ -> {- trace ("buildBranch " ++ show (length os)) $ -} GroupConBranch l con e slotNum (sortPat e (e+slotNum) os)

            (brs, gs') = eatCons branchesSeed (g:gs)
          in
            GroupCon l brs : grouping gs'
        other -> error $ "sortPat:grouping: " ++ show (forgetL other) ++ " not supported"

orderedCaseToExp :: OrderedCase l -> Exp l
orderedCaseToExp (OrderedCaseFallback l) = Var l (UnQual l (Ident l "fallback-"))
orderedCaseToExp (OrderedCaseRHS rhs binds) = rhsToExp rhs -- XXX 沒有處理 binds
orderedCaseToExp (OrderedCase l target groups) =
  Let l (BDecls l [PatBind l (PVar l (Ident l "fallback+")) (UnGuardedRhs l (Var l (UnQual l (Ident l "fallback-")))) Nothing]) bodyExp where
    bodyExp = go groups
    go [] = Var l (UnQual l (Ident l "fallback+"))
    go (g:gs) =
      Let l (BDecls l [PatBind l (PVar l (Ident l "fallback-")) (UnGuardedRhs l (go gs)) Nothing]) bodyExp where
        bodyExp = case g of
          GroupWildCard l cs ->
            orderedCaseToExp cs
          GroupVar l var cs ->
            Let l (BDecls l [PatBind l (PVar l var) (UnGuardedRhs l (Var l (UnQual l target))) Nothing]) (orderedCaseToExp cs)
          GroupCon l bs ->
            Case l (Var l (UnQual l target)) (map bsToAlt bs)
            where
              bsToAlt (GroupConBranch l con b slotNum cs) =
                Alt l (PApp l con [ PVar l (Ident l ("x" ++ show i ++ "-")) | i <- [b .. b+slotNum-1]]) (UnGuardedRhs l (orderedCaseToExp cs)) Nothing
          other -> error $ "orderedCaseToExp:go: " ++ show (forgetL other) ++ " not supported"

--fallbackGroup :: [Alt l] -> [[Alt l]]
--fallbackGroup alts = go alts where
--  mergeable (Alt _ (PWildCard _) _ _) = False
--  mergeable (Alt _ (PVar _ _) _ _) = False
--  mergeable _ = True
--
--  go [] = []
--  go (a:as)
--    | mergeable a = go2 (a :) as
--    | otherwise = [a] : go as
--
--  go2 prevs [] = [prevs []]
--  go2 prevs (a:as)
--    | mergeable a = go2 (prevs . (a :)) as
--    | otherwise = prevs [] : [a] : go as

rhsToExp :: Rhs l -> Exp l
rhsToExp (UnGuardedRhs l exp) = exp
rhsToExp (GuardedRhss l guards) =
  Let l (BDecls l [PatBind l (PVar l (Ident l "fallback+")) (UnGuardedRhs l (Var l (UnQual l (Ident l "fallback-")))) Nothing]) bodyExp where
    bodyExp = go guards
    go (GuardedRhs l1 [Qualifier l2 cond] exp : gs) =
      Let l1 (BDecls l1 [PatBind l1 (PVar l1 (Ident l1 "fallback-")) (UnGuardedRhs l1 (go gs)) Nothing]) bodyExp where
        bodyExp = Case l2 cond
          [ Alt l2 (PApp l2 (Qual l2 (ModuleName l2 "Prelude") (Ident l2 "False")) []) (UnGuardedRhs l2 (Var l2 (UnQual l2 (Ident l2 "fallback-")))) Nothing
          , Alt l2 (PApp l2 (Qual l2 (ModuleName l2 "Prelude") (Ident l2 "True")) []) (UnGuardedRhs l2 exp) Nothing
          ]
    go (g : gs) =
      error $ "Unsupported guard stmt: " ++ show (forgetL g)
    go [] =
      Var l (UnQual l (Ident l "fallback+"))

--groupsToExp :: l -> Int -> [FallbackGroup l] -> Exp l
--groupsToExp l expN groups = Let l (BDecls l [PatBind l (PVar l (Ident l "fallback+")) (UnGuardedRhs l (Var l (UnQual l (Ident l "fallback-")))) Nothing]) bodyExp where
--  bodyExp = genBody groups
--  genBody [] = Var l (UnQual l (Ident l "fallback+"))
--  genBody (g:gs) = Let l (BDecls l [PatBind l (PVar l (Ident l "fallback-")) (UnGuardedRhs l (genBody gs)) Nothing]) groupExp where
--    groupExp = case g of -- 未處理 binds (where)
--      GroupWildCard l1 rhs binds -> rhsToExp rhs
--      GroupVar l1 var rhs binds ->
--        Let l1 (BDecls l1 [PatBind l1 (PVar l1 var) (UnGuardedRhs l1 (Var l1 (UnQual l1 (Ident l1 ("x" ++ show expN))))) Nothing]) (rhsToExp rhs)

--reorder newVarNum fallbackNum (Case l exp alts) = case alts of
--  [] -> Var l (Qual l (ModuleName l "Prelude") (Ident l "undefined"))
--  (Alt l1 (PWildCard _) rhs binds : alts) -> undefined -- tryRHS rhs binds alts
--  (Alt l1 (PVar l2 var) rhs binds : alts) -> undefined

deCaseReorderActivation :: SrcInfo l => CollectDataResult -> Activation l -> Activation l
deCaseReorderActivation a1 (ActiveFrom l int) = ActiveFrom (id l) (id int)
deCaseReorderActivation a1 (ActiveUntil l int) = ActiveUntil (id l) (id int)
deCaseReorderAlt :: SrcInfo l => CollectDataResult -> Alt l -> Alt l
deCaseReorderAlt a1 (Alt l pat rhs binds) = Alt (id l) (deCaseReorderPat a1 pat) (deCaseReorderRhs a1 rhs) (fmap (deCaseReorderBinds a1) binds)
deCaseReorderAnnotation :: SrcInfo l => CollectDataResult -> Annotation l -> Annotation l
deCaseReorderAnnotation a1 (Ann l name exp) = Ann (id l) (deCaseReorderName a1 name) (deCaseReorderExp a1 exp)
deCaseReorderAnnotation a1 (TypeAnn l name exp) = TypeAnn (id l) (deCaseReorderName a1 name) (deCaseReorderExp a1 exp)
deCaseReorderAnnotation a1 (ModuleAnn l exp) = ModuleAnn (id l) (deCaseReorderExp a1 exp)
deCaseReorderAssoc :: SrcInfo l => CollectDataResult -> Assoc l -> Assoc l
deCaseReorderAssoc a1 (AssocNone l) = AssocNone (id l)
deCaseReorderAssoc a1 (AssocLeft l) = AssocLeft (id l)
deCaseReorderAssoc a1 (AssocRight l) = AssocRight (id l)
deCaseReorderAsst :: SrcInfo l => CollectDataResult -> Asst l -> Asst l
deCaseReorderAsst a1 (ClassA l qName type0) = ClassA (id l) (deCaseReorderQName a1 qName) (fmap (deCaseReorderType a1) type0)
deCaseReorderAsst a1 (AppA l name type0) = AppA (id l) (deCaseReorderName a1 name) (fmap (deCaseReorderType a1) type0)
deCaseReorderAsst a1 (InfixA l type1 qName type2) = InfixA (id l) (deCaseReorderType a1 type1) (deCaseReorderQName a1 qName) (deCaseReorderType a1 type2)
deCaseReorderAsst a1 (IParam l iPName type0) = IParam (id l) (deCaseReorderIPName a1 iPName) (deCaseReorderType a1 type0)
deCaseReorderAsst a1 (EqualP l type1 type2) = EqualP (id l) (deCaseReorderType a1 type1) (deCaseReorderType a1 type2)
deCaseReorderAsst a1 (ParenA l asst) = ParenA (id l) (deCaseReorderAsst a1 asst)
deCaseReorderAsst a1 (WildCardA l name) = WildCardA (id l) (fmap (deCaseReorderName a1) name)
deCaseReorderBangType :: SrcInfo l => CollectDataResult -> BangType l -> BangType l
deCaseReorderBangType a1 (BangedTy l) = BangedTy (id l)
deCaseReorderBangType a1 (UnpackedTy l) = UnpackedTy (id l)
deCaseReorderBinds :: SrcInfo l => CollectDataResult -> Binds l -> Binds l
deCaseReorderBinds a1 (BDecls l decl) = BDecls (id l) (fmap (deCaseReorderDecl a1) decl)
deCaseReorderBinds a1 (IPBinds l iPBind) = IPBinds (id l) (fmap (deCaseReorderIPBind a1) iPBind)
deCaseReorderBooleanFormula :: SrcInfo l => CollectDataResult -> BooleanFormula l -> BooleanFormula l
deCaseReorderBooleanFormula a1 (VarFormula l name) = VarFormula (id l) (deCaseReorderName a1 name)
deCaseReorderBooleanFormula a1 (AndFormula l booleanFormula) = AndFormula (id l) (fmap (deCaseReorderBooleanFormula a1) booleanFormula)
deCaseReorderBooleanFormula a1 (OrFormula l booleanFormula) = OrFormula (id l) (fmap (deCaseReorderBooleanFormula a1) booleanFormula)
deCaseReorderBooleanFormula a1 (ParenFormula l booleanFormula) = ParenFormula (id l) (deCaseReorderBooleanFormula a1 booleanFormula)
deCaseReorderBoxed :: CollectDataResult -> Boxed -> Boxed
deCaseReorderBoxed a1 (Boxed) = Boxed
deCaseReorderBoxed a1 (Unboxed) = Unboxed
deCaseReorderBracket :: SrcInfo l => CollectDataResult -> Bracket l -> Bracket l
deCaseReorderBracket a1 (ExpBracket l exp) = ExpBracket (id l) (deCaseReorderExp a1 exp)
deCaseReorderBracket a1 (PatBracket l pat) = PatBracket (id l) (deCaseReorderPat a1 pat)
deCaseReorderBracket a1 (TypeBracket l type0) = TypeBracket (id l) (deCaseReorderType a1 type0)
deCaseReorderBracket a1 (DeclBracket l decl) = DeclBracket (id l) (fmap (deCaseReorderDecl a1) decl)
deCaseReorderCName :: SrcInfo l => CollectDataResult -> CName l -> CName l
deCaseReorderCName a1 (VarName l name) = VarName (id l) (deCaseReorderName a1 name)
deCaseReorderCName a1 (ConName l name) = ConName (id l) (deCaseReorderName a1 name)
deCaseReorderCallConv :: SrcInfo l => CollectDataResult -> CallConv l -> CallConv l
deCaseReorderCallConv a1 (StdCall l) = StdCall (id l)
deCaseReorderCallConv a1 (CCall l) = CCall (id l)
deCaseReorderCallConv a1 (CPlusPlus l) = CPlusPlus (id l)
deCaseReorderCallConv a1 (DotNet l) = DotNet (id l)
deCaseReorderCallConv a1 (Jvm l) = Jvm (id l)
deCaseReorderCallConv a1 (Js l) = Js (id l)
deCaseReorderCallConv a1 (JavaScript l) = JavaScript (id l)
deCaseReorderCallConv a1 (CApi l) = CApi (id l)
deCaseReorderClassDecl :: SrcInfo l => CollectDataResult -> ClassDecl l -> ClassDecl l
deCaseReorderClassDecl a1 (ClsDecl l decl) = ClsDecl (id l) (deCaseReorderDecl a1 decl)
deCaseReorderClassDecl a1 (ClsDataFam l context declHead kind) = ClsDataFam (id l) (fmap (deCaseReorderContext a1) context) (deCaseReorderDeclHead a1 declHead) (fmap (deCaseReorderKind a1) kind)
deCaseReorderClassDecl a1 (ClsTyFam l declHead kind) = ClsTyFam (id l) (deCaseReorderDeclHead a1 declHead) (fmap (deCaseReorderKind a1) kind)
deCaseReorderClassDecl a1 (ClsTyDef l type1 type2) = ClsTyDef (id l) (deCaseReorderType a1 type1) (deCaseReorderType a1 type2)
deCaseReorderClassDecl a1 (ClsDefSig l name type0) = ClsDefSig (id l) (deCaseReorderName a1 name) (deCaseReorderType a1 type0)
deCaseReorderConDecl :: SrcInfo l => CollectDataResult -> ConDecl l -> ConDecl l
deCaseReorderConDecl a1 (ConDecl l name type0) = ConDecl (id l) (deCaseReorderName a1 name) (fmap (deCaseReorderType a1) type0)
deCaseReorderConDecl a1 (InfixConDecl l type1 name type2) = InfixConDecl (id l) (deCaseReorderType a1 type1) (deCaseReorderName a1 name) (deCaseReorderType a1 type2)
deCaseReorderConDecl a1 (RecDecl l name fieldDecl) = RecDecl (id l) (deCaseReorderName a1 name) (fmap (deCaseReorderFieldDecl a1) fieldDecl)
deCaseReorderContext :: SrcInfo l => CollectDataResult -> Context l -> Context l
deCaseReorderContext a1 (CxSingle l asst) = CxSingle (id l) (deCaseReorderAsst a1 asst)
deCaseReorderContext a1 (CxTuple l asst) = CxTuple (id l) (fmap (deCaseReorderAsst a1) asst)
deCaseReorderContext a1 (CxEmpty l) = CxEmpty (id l)
deCaseReorderDataOrNew :: SrcInfo l => CollectDataResult -> DataOrNew l -> DataOrNew l
deCaseReorderDataOrNew a1 (DataType l) = DataType (id l)
deCaseReorderDataOrNew a1 (NewType l) = NewType (id l)
deCaseReorderDecl :: SrcInfo l => CollectDataResult -> Decl l -> Decl l
deCaseReorderDecl a1 (TypeDecl l declHead type0) = TypeDecl (id l) (deCaseReorderDeclHead a1 declHead) (deCaseReorderType a1 type0)
deCaseReorderDecl a1 (TypeFamDecl l declHead kind) = TypeFamDecl (id l) (deCaseReorderDeclHead a1 declHead) (fmap (deCaseReorderKind a1) kind)
deCaseReorderDecl a1 (ClosedTypeFamDecl l declHead kind typeEqn) = ClosedTypeFamDecl (id l) (deCaseReorderDeclHead a1 declHead) (fmap (deCaseReorderKind a1) kind) (fmap (deCaseReorderTypeEqn a1) typeEqn)
deCaseReorderDecl a1 (DataDecl l dataOrNew context declHead qualConDecl deriving0) = DataDecl (id l) (deCaseReorderDataOrNew a1 dataOrNew) (fmap (deCaseReorderContext a1) context) (deCaseReorderDeclHead a1 declHead) (fmap (deCaseReorderQualConDecl a1) qualConDecl) (fmap (deCaseReorderDeriving a1) deriving0)
deCaseReorderDecl a1 (GDataDecl l dataOrNew context declHead kind gadtDecl deriving0) = GDataDecl (id l) (deCaseReorderDataOrNew a1 dataOrNew) (fmap (deCaseReorderContext a1) context) (deCaseReorderDeclHead a1 declHead) (fmap (deCaseReorderKind a1) kind) (fmap (deCaseReorderGadtDecl a1) gadtDecl) (fmap (deCaseReorderDeriving a1) deriving0)
deCaseReorderDecl a1 (DataFamDecl l context declHead kind) = DataFamDecl (id l) (fmap (deCaseReorderContext a1) context) (deCaseReorderDeclHead a1 declHead) (fmap (deCaseReorderKind a1) kind)
deCaseReorderDecl a1 (TypeInsDecl l type1 type2) = TypeInsDecl (id l) (deCaseReorderType a1 type1) (deCaseReorderType a1 type2)
deCaseReorderDecl a1 (DataInsDecl l dataOrNew type0 qualConDecl deriving0) = DataInsDecl (id l) (deCaseReorderDataOrNew a1 dataOrNew) (deCaseReorderType a1 type0) (fmap (deCaseReorderQualConDecl a1) qualConDecl) (fmap (deCaseReorderDeriving a1) deriving0)
deCaseReorderDecl a1 (GDataInsDecl l dataOrNew type0 kind gadtDecl deriving0) = GDataInsDecl (id l) (deCaseReorderDataOrNew a1 dataOrNew) (deCaseReorderType a1 type0) (fmap (deCaseReorderKind a1) kind) (fmap (deCaseReorderGadtDecl a1) gadtDecl) (fmap (deCaseReorderDeriving a1) deriving0)
deCaseReorderDecl a1 (ClassDecl l context declHead funDep classDecl) = ClassDecl (id l) (fmap (deCaseReorderContext a1) context) (deCaseReorderDeclHead a1 declHead) (fmap (deCaseReorderFunDep a1) funDep) (fmap (fmap (deCaseReorderClassDecl a1)) classDecl)
deCaseReorderDecl a1 (InstDecl l overlap instRule instDecl) = InstDecl (id l) (fmap (deCaseReorderOverlap a1) overlap) (deCaseReorderInstRule a1 instRule) (fmap (fmap (deCaseReorderInstDecl a1)) instDecl)
deCaseReorderDecl a1 (DerivDecl l overlap instRule) = DerivDecl (id l) (fmap (deCaseReorderOverlap a1) overlap) (deCaseReorderInstRule a1 instRule)
deCaseReorderDecl a1 (InfixDecl l assoc int op) = InfixDecl (id l) (deCaseReorderAssoc a1 assoc) (fmap (id) int) (fmap (deCaseReorderOp a1) op)
deCaseReorderDecl a1 (DefaultDecl l type0) = DefaultDecl (id l) (fmap (deCaseReorderType a1) type0)
deCaseReorderDecl a1 (SpliceDecl l exp) = SpliceDecl (id l) (deCaseReorderExp a1 exp)
deCaseReorderDecl a1 (TypeSig l name type0) = TypeSig (id l) (fmap (deCaseReorderName a1) name) (deCaseReorderType a1 type0)
deCaseReorderDecl a1 (PatSynSig l name tyVarBind context1 context2 type0) = PatSynSig (id l) (deCaseReorderName a1 name) (fmap (fmap (deCaseReorderTyVarBind a1)) tyVarBind) (fmap (deCaseReorderContext a1) context1) (fmap (deCaseReorderContext a1) context2) (deCaseReorderType a1 type0)
deCaseReorderDecl a1 (FunBind l match) = FunBind (id l) (fmap (deCaseReorderMatch a1) match)
deCaseReorderDecl a1 (PatBind l pat rhs binds) = PatBind (id l) (deCaseReorderPat a1 pat) (deCaseReorderRhs a1 rhs) (fmap (deCaseReorderBinds a1) binds)
deCaseReorderDecl a1 (PatSyn l pat1 pat2 patternSynDirection) = PatSyn (id l) (deCaseReorderPat a1 pat1) (deCaseReorderPat a1 pat2) (deCaseReorderPatternSynDirection a1 patternSynDirection)
deCaseReorderDecl a1 (ForImp l callConv safety string name type0) = ForImp (id l) (deCaseReorderCallConv a1 callConv) (fmap (deCaseReorderSafety a1) safety) (fmap (id) string) (deCaseReorderName a1 name) (deCaseReorderType a1 type0)
deCaseReorderDecl a1 (ForExp l callConv string name type0) = ForExp (id l) (deCaseReorderCallConv a1 callConv) (fmap (id) string) (deCaseReorderName a1 name) (deCaseReorderType a1 type0)
deCaseReorderDecl a1 (RulePragmaDecl l rule) = RulePragmaDecl (id l) (fmap (deCaseReorderRule a1) rule)
deCaseReorderDecl a1 (DeprPragmaDecl l name) = DeprPragmaDecl (id l) (fmap (((fmap (deCaseReorderName a1)) *** (id))) name)
deCaseReorderDecl a1 (WarnPragmaDecl l name) = WarnPragmaDecl (id l) (fmap (((fmap (deCaseReorderName a1)) *** (id))) name)
deCaseReorderDecl a1 (InlineSig l bool activation qName) = InlineSig (id l) (id bool) (fmap (deCaseReorderActivation a1) activation) (deCaseReorderQName a1 qName)
deCaseReorderDecl a1 (InlineConlikeSig l activation qName) = InlineConlikeSig (id l) (fmap (deCaseReorderActivation a1) activation) (deCaseReorderQName a1 qName)
deCaseReorderDecl a1 (SpecSig l activation qName type0) = SpecSig (id l) (fmap (deCaseReorderActivation a1) activation) (deCaseReorderQName a1 qName) (fmap (deCaseReorderType a1) type0)
deCaseReorderDecl a1 (SpecInlineSig l bool activation qName type0) = SpecInlineSig (id l) (id bool) (fmap (deCaseReorderActivation a1) activation) (deCaseReorderQName a1 qName) (fmap (deCaseReorderType a1) type0)
deCaseReorderDecl a1 (InstSig l instRule) = InstSig (id l) (deCaseReorderInstRule a1 instRule)
deCaseReorderDecl a1 (AnnPragma l annotation) = AnnPragma (id l) (deCaseReorderAnnotation a1 annotation)
deCaseReorderDecl a1 (MinimalPragma l booleanFormula) = MinimalPragma (id l) (fmap (deCaseReorderBooleanFormula a1) booleanFormula)
deCaseReorderDecl a1 (RoleAnnotDecl l qName role) = RoleAnnotDecl (id l) (deCaseReorderQName a1 qName) (fmap (deCaseReorderRole a1) role)
deCaseReorderDeclHead :: SrcInfo l => CollectDataResult -> DeclHead l -> DeclHead l
deCaseReorderDeclHead a1 (DHead l name) = DHead (id l) (deCaseReorderName a1 name)
deCaseReorderDeclHead a1 (DHInfix l tyVarBind name) = DHInfix (id l) (deCaseReorderTyVarBind a1 tyVarBind) (deCaseReorderName a1 name)
deCaseReorderDeclHead a1 (DHParen l declHead) = DHParen (id l) (deCaseReorderDeclHead a1 declHead)
deCaseReorderDeclHead a1 (DHApp l declHead tyVarBind) = DHApp (id l) (deCaseReorderDeclHead a1 declHead) (deCaseReorderTyVarBind a1 tyVarBind)
deCaseReorderDeriving :: SrcInfo l => CollectDataResult -> Deriving l -> Deriving l
deCaseReorderDeriving a1 (Deriving l instRule) = Deriving (id l) (fmap (deCaseReorderInstRule a1) instRule)
deCaseReorderExp :: SrcInfo l => CollectDataResult -> Exp l -> Exp l
deCaseReorderExp a1 (Var l qName) = Var (id l) (deCaseReorderQName a1 qName)
deCaseReorderExp a1 (IPVar l iPName) = IPVar (id l) (deCaseReorderIPName a1 iPName)
deCaseReorderExp a1 (Con l qName) = Con (id l) (deCaseReorderQName a1 qName)
deCaseReorderExp a1 (Lit l literal) = Lit (id l) (deCaseReorderLiteral a1 literal)
deCaseReorderExp a1 (InfixApp l exp1 qOp exp2) = InfixApp (id l) (deCaseReorderExp a1 exp1) (deCaseReorderQOp a1 qOp) (deCaseReorderExp a1 exp2)
deCaseReorderExp a1 (App l exp1 exp2) = App (id l) (deCaseReorderExp a1 exp1) (deCaseReorderExp a1 exp2)
deCaseReorderExp a1 (NegApp l exp) = NegApp (id l) (deCaseReorderExp a1 exp)
deCaseReorderExp a1 (Lambda l pat exp) = Lambda (id l) (fmap (deCaseReorderPat a1) pat) (deCaseReorderExp a1 exp)
deCaseReorderExp a1 (Let l binds exp) = Let (id l) (deCaseReorderBinds a1 binds) (deCaseReorderExp a1 exp)
deCaseReorderExp a1 (If l exp1 exp2 exp3) = If (id l) (deCaseReorderExp a1 exp1) (deCaseReorderExp a1 exp2) (deCaseReorderExp a1 exp3)
deCaseReorderExp a1 (MultiIf l guardedRhs) = MultiIf (id l) (fmap (deCaseReorderGuardedRhs a1) guardedRhs)
--deCaseReorderExp a1 c@(Case l exp alts@(Alt _ (PList _ _) _ _ : _)) = Case l (deCaseReorderExp a1 exp) (fmap (deCaseReorderAlt a1) alts)
--deCaseReorderExp a1 c@(Case l exp alts@(Alt _ (PLit _ _ _) _ _ : _)) = Case l (deCaseReorderExp a1 exp) (fmap (deCaseReorderAlt a1) alts)
deCaseReorderExp a1 c@(Case l exp alts) =
  let
    d = buildReorder (dataConToShape a1) l (map altToPartial alts)
    c = Let l (BDecls l
      [ PatBind l (PVar l (Ident l "fallback-")) (UnGuardedRhs l (Var l (UnQual l (Ident l "undefined")))) Nothing
      , PatBind l (PVar l (Ident l "x0-")) (UnGuardedRhs l (deCaseReorderExp a1 exp)) Nothing
      ] ) bodyExp
    bodyExp = orderedCaseToExp d
-- reorder 1 0 c
  in
    error $ "\n\n\n" ++ show (forgetL d) ++ "\n\n\n" ++ show (forgetL c) ++ "\n\n\n" ++ prettyPrint c
    --c
-- Case (id l) (deCaseReorderExp a1 exp) (fmap (deCaseReorderAlt a1) alt)
deCaseReorderExp a1 (Do l stmt) = Do (id l) (fmap (deCaseReorderStmt a1) stmt)
deCaseReorderExp a1 (MDo l stmt) = MDo (id l) (fmap (deCaseReorderStmt a1) stmt)
deCaseReorderExp a1 (Tuple l boxed exp) = Tuple (id l) (deCaseReorderBoxed a1 boxed) (fmap (deCaseReorderExp a1) exp)
deCaseReorderExp a1 (TupleSection l boxed exp) = TupleSection (id l) (deCaseReorderBoxed a1 boxed) (fmap (fmap (deCaseReorderExp a1)) exp)
deCaseReorderExp a1 (List l exp) = List (id l) (fmap (deCaseReorderExp a1) exp)
deCaseReorderExp a1 (ParArray l exp) = ParArray (id l) (fmap (deCaseReorderExp a1) exp)
deCaseReorderExp a1 (Paren l exp) = Paren (id l) (deCaseReorderExp a1 exp)
deCaseReorderExp a1 (LeftSection l exp qOp) = LeftSection (id l) (deCaseReorderExp a1 exp) (deCaseReorderQOp a1 qOp)
deCaseReorderExp a1 (RightSection l qOp exp) = RightSection (id l) (deCaseReorderQOp a1 qOp) (deCaseReorderExp a1 exp)
deCaseReorderExp a1 (RecConstr l qName fieldUpdate) = RecConstr (id l) (deCaseReorderQName a1 qName) (fmap (deCaseReorderFieldUpdate a1) fieldUpdate)
deCaseReorderExp a1 (RecUpdate l exp fieldUpdate) = RecUpdate (id l) (deCaseReorderExp a1 exp) (fmap (deCaseReorderFieldUpdate a1) fieldUpdate)
deCaseReorderExp a1 (EnumFrom l exp) = EnumFrom (id l) (deCaseReorderExp a1 exp)
deCaseReorderExp a1 (EnumFromTo l exp1 exp2) = EnumFromTo (id l) (deCaseReorderExp a1 exp1) (deCaseReorderExp a1 exp2)
deCaseReorderExp a1 (EnumFromThen l exp1 exp2) = EnumFromThen (id l) (deCaseReorderExp a1 exp1) (deCaseReorderExp a1 exp2)
deCaseReorderExp a1 (EnumFromThenTo l exp1 exp2 exp3) = EnumFromThenTo (id l) (deCaseReorderExp a1 exp1) (deCaseReorderExp a1 exp2) (deCaseReorderExp a1 exp3)
deCaseReorderExp a1 (ParArrayFromTo l exp1 exp2) = ParArrayFromTo (id l) (deCaseReorderExp a1 exp1) (deCaseReorderExp a1 exp2)
deCaseReorderExp a1 (ParArrayFromThenTo l exp1 exp2 exp3) = ParArrayFromThenTo (id l) (deCaseReorderExp a1 exp1) (deCaseReorderExp a1 exp2) (deCaseReorderExp a1 exp3)
deCaseReorderExp a1 (ListComp l exp qualStmt) = ListComp (id l) (deCaseReorderExp a1 exp) (fmap (deCaseReorderQualStmt a1) qualStmt)
deCaseReorderExp a1 (ParComp l exp qualStmt) = ParComp (id l) (deCaseReorderExp a1 exp) (fmap (fmap (deCaseReorderQualStmt a1)) qualStmt)
deCaseReorderExp a1 (ParArrayComp l exp qualStmt) = ParArrayComp (id l) (deCaseReorderExp a1 exp) (fmap (fmap (deCaseReorderQualStmt a1)) qualStmt)
deCaseReorderExp a1 (ExpTypeSig l exp type0) = ExpTypeSig (id l) (deCaseReorderExp a1 exp) (deCaseReorderType a1 type0)
deCaseReorderExp a1 (VarQuote l qName) = VarQuote (id l) (deCaseReorderQName a1 qName)
deCaseReorderExp a1 (TypQuote l qName) = TypQuote (id l) (deCaseReorderQName a1 qName)
deCaseReorderExp a1 (BracketExp l bracket) = BracketExp (id l) (deCaseReorderBracket a1 bracket)
deCaseReorderExp a1 (SpliceExp l splice) = SpliceExp (id l) (deCaseReorderSplice a1 splice)
deCaseReorderExp a1 (QuasiQuote l string1 string2) = QuasiQuote (id l) (id string1) (id string2)
deCaseReorderExp a1 (XTag l xName xAttr exp1 exp2) = XTag (id l) (deCaseReorderXName a1 xName) (fmap (deCaseReorderXAttr a1) xAttr) (fmap (deCaseReorderExp a1) exp1) (fmap (deCaseReorderExp a1) exp2)
deCaseReorderExp a1 (XETag l xName xAttr exp) = XETag (id l) (deCaseReorderXName a1 xName) (fmap (deCaseReorderXAttr a1) xAttr) (fmap (deCaseReorderExp a1) exp)
deCaseReorderExp a1 (XPcdata l string) = XPcdata (id l) (id string)
deCaseReorderExp a1 (XExpTag l exp) = XExpTag (id l) (deCaseReorderExp a1 exp)
deCaseReorderExp a1 (XChildTag l exp) = XChildTag (id l) (fmap (deCaseReorderExp a1) exp)
deCaseReorderExp a1 (CorePragma l string exp) = CorePragma (id l) (id string) (deCaseReorderExp a1 exp)
deCaseReorderExp a1 (SCCPragma l string exp) = SCCPragma (id l) (id string) (deCaseReorderExp a1 exp)
deCaseReorderExp a1 (GenPragma l string int1 int2 exp) = GenPragma (id l) (id string) (((id) *** (id)) int1) (((id) *** (id)) int2) (deCaseReorderExp a1 exp)
deCaseReorderExp a1 (Proc l pat exp) = Proc (id l) (deCaseReorderPat a1 pat) (deCaseReorderExp a1 exp)
deCaseReorderExp a1 (LeftArrApp l exp1 exp2) = LeftArrApp (id l) (deCaseReorderExp a1 exp1) (deCaseReorderExp a1 exp2)
deCaseReorderExp a1 (RightArrApp l exp1 exp2) = RightArrApp (id l) (deCaseReorderExp a1 exp1) (deCaseReorderExp a1 exp2)
deCaseReorderExp a1 (LeftArrHighApp l exp1 exp2) = LeftArrHighApp (id l) (deCaseReorderExp a1 exp1) (deCaseReorderExp a1 exp2)
deCaseReorderExp a1 (RightArrHighApp l exp1 exp2) = RightArrHighApp (id l) (deCaseReorderExp a1 exp1) (deCaseReorderExp a1 exp2)
deCaseReorderExp a1 (LCase l alt) = LCase (id l) (fmap (deCaseReorderAlt a1) alt)
deCaseReorderExp a1 (ExprHole l) = ExprHole (id l)
deCaseReorderExportSpec :: SrcInfo l => CollectDataResult -> ExportSpec l -> ExportSpec l
deCaseReorderExportSpec a1 (EVar l qName) = EVar (id l) (deCaseReorderQName a1 qName)
deCaseReorderExportSpec a1 (EAbs l namespace qName) = EAbs (id l) (deCaseReorderNamespace a1 namespace) (deCaseReorderQName a1 qName)
deCaseReorderExportSpec a1 (EThingAll l qName) = EThingAll (id l) (deCaseReorderQName a1 qName)
deCaseReorderExportSpec a1 (EThingWith l qName cName) = EThingWith (id l) (deCaseReorderQName a1 qName) (fmap (deCaseReorderCName a1) cName)
deCaseReorderExportSpec a1 (EModuleContents l moduleName) = EModuleContents (id l) (deCaseReorderModuleName a1 moduleName)
deCaseReorderExportSpecList :: SrcInfo l => CollectDataResult -> ExportSpecList l -> ExportSpecList l
deCaseReorderExportSpecList a1 (ExportSpecList l exportSpec) = ExportSpecList (id l) (fmap (deCaseReorderExportSpec a1) exportSpec)
deCaseReorderFieldDecl :: SrcInfo l => CollectDataResult -> FieldDecl l -> FieldDecl l
deCaseReorderFieldDecl a1 (FieldDecl l name type0) = FieldDecl (id l) (fmap (deCaseReorderName a1) name) (deCaseReorderType a1 type0)
deCaseReorderFieldUpdate :: SrcInfo l => CollectDataResult -> FieldUpdate l -> FieldUpdate l
deCaseReorderFieldUpdate a1 (FieldUpdate l qName exp) = FieldUpdate (id l) (deCaseReorderQName a1 qName) (deCaseReorderExp a1 exp)
deCaseReorderFieldUpdate a1 (FieldPun l qName) = FieldPun (id l) (deCaseReorderQName a1 qName)
deCaseReorderFieldUpdate a1 (FieldWildcard l) = FieldWildcard (id l)
deCaseReorderFunDep :: SrcInfo l => CollectDataResult -> FunDep l -> FunDep l
deCaseReorderFunDep a1 (FunDep l name1 name2) = FunDep (id l) (fmap (deCaseReorderName a1) name1) (fmap (deCaseReorderName a1) name2)
deCaseReorderGadtDecl :: SrcInfo l => CollectDataResult -> GadtDecl l -> GadtDecl l
deCaseReorderGadtDecl a1 (GadtDecl l name fieldDecl type0) = GadtDecl (id l) (deCaseReorderName a1 name) (fmap (fmap (deCaseReorderFieldDecl a1)) fieldDecl) (deCaseReorderType a1 type0)
deCaseReorderGuardedRhs :: SrcInfo l => CollectDataResult -> GuardedRhs l -> GuardedRhs l
deCaseReorderGuardedRhs a1 (GuardedRhs l stmt exp) = GuardedRhs (id l) (fmap (deCaseReorderStmt a1) stmt) (deCaseReorderExp a1 exp)
deCaseReorderIPBind :: SrcInfo l => CollectDataResult -> IPBind l -> IPBind l
deCaseReorderIPBind a1 (IPBind l iPName exp) = IPBind (id l) (deCaseReorderIPName a1 iPName) (deCaseReorderExp a1 exp)
deCaseReorderIPName :: SrcInfo l => CollectDataResult -> IPName l -> IPName l
deCaseReorderIPName a1 (IPDup l string) = IPDup (id l) (id string)
deCaseReorderIPName a1 (IPLin l string) = IPLin (id l) (id string)
deCaseReorderImportDecl :: SrcInfo l => CollectDataResult -> ImportDecl l -> ImportDecl l
deCaseReorderImportDecl a1 (ImportDecl importAnn importModule importQualified importSrc importSafe importPkg importAs importSpecs) = ImportDecl (id importAnn) (deCaseReorderModuleName a1 importModule) (id importQualified) (id importSrc) (id importSafe) (fmap (id) importPkg) (fmap (deCaseReorderModuleName a1) importAs) (fmap (deCaseReorderImportSpecList a1) importSpecs)
deCaseReorderImportSpec :: SrcInfo l => CollectDataResult -> ImportSpec l -> ImportSpec l
deCaseReorderImportSpec a1 (IVar l name) = IVar (id l) (deCaseReorderName a1 name)
deCaseReorderImportSpec a1 (IAbs l namespace name) = IAbs (id l) (deCaseReorderNamespace a1 namespace) (deCaseReorderName a1 name)
deCaseReorderImportSpec a1 (IThingAll l name) = IThingAll (id l) (deCaseReorderName a1 name)
deCaseReorderImportSpec a1 (IThingWith l name cName) = IThingWith (id l) (deCaseReorderName a1 name) (fmap (deCaseReorderCName a1) cName)
deCaseReorderImportSpecList :: SrcInfo l => CollectDataResult -> ImportSpecList l -> ImportSpecList l
deCaseReorderImportSpecList a1 (ImportSpecList l bool importSpec) = ImportSpecList (id l) (id bool) (fmap (deCaseReorderImportSpec a1) importSpec)
deCaseReorderInstDecl :: SrcInfo l => CollectDataResult -> InstDecl l -> InstDecl l
deCaseReorderInstDecl a1 (InsDecl l decl) = InsDecl (id l) (deCaseReorderDecl a1 decl)
deCaseReorderInstDecl a1 (InsType l type1 type2) = InsType (id l) (deCaseReorderType a1 type1) (deCaseReorderType a1 type2)
deCaseReorderInstDecl a1 (InsData l dataOrNew type0 qualConDecl deriving0) = InsData (id l) (deCaseReorderDataOrNew a1 dataOrNew) (deCaseReorderType a1 type0) (fmap (deCaseReorderQualConDecl a1) qualConDecl) (fmap (deCaseReorderDeriving a1) deriving0)
deCaseReorderInstDecl a1 (InsGData l dataOrNew type0 kind gadtDecl deriving0) = InsGData (id l) (deCaseReorderDataOrNew a1 dataOrNew) (deCaseReorderType a1 type0) (fmap (deCaseReorderKind a1) kind) (fmap (deCaseReorderGadtDecl a1) gadtDecl) (fmap (deCaseReorderDeriving a1) deriving0)
deCaseReorderInstHead :: SrcInfo l => CollectDataResult -> InstHead l -> InstHead l
deCaseReorderInstHead a1 (IHCon l qName) = IHCon (id l) (deCaseReorderQName a1 qName)
deCaseReorderInstHead a1 (IHInfix l type0 qName) = IHInfix (id l) (deCaseReorderType a1 type0) (deCaseReorderQName a1 qName)
deCaseReorderInstHead a1 (IHParen l instHead) = IHParen (id l) (deCaseReorderInstHead a1 instHead)
deCaseReorderInstHead a1 (IHApp l instHead type0) = IHApp (id l) (deCaseReorderInstHead a1 instHead) (deCaseReorderType a1 type0)
deCaseReorderInstRule :: SrcInfo l => CollectDataResult -> InstRule l -> InstRule l
deCaseReorderInstRule a1 (IRule l tyVarBind context instHead) = IRule (id l) (fmap (fmap (deCaseReorderTyVarBind a1)) tyVarBind) (fmap (deCaseReorderContext a1) context) (deCaseReorderInstHead a1 instHead)
deCaseReorderInstRule a1 (IParen l instRule) = IParen (id l) (deCaseReorderInstRule a1 instRule)
deCaseReorderKind :: SrcInfo l => CollectDataResult -> Kind l -> Kind l
deCaseReorderKind a1 (KindStar l) = KindStar (id l)
deCaseReorderKind a1 (KindFn l kind1 kind2) = KindFn (id l) (deCaseReorderKind a1 kind1) (deCaseReorderKind a1 kind2)
deCaseReorderKind a1 (KindParen l kind) = KindParen (id l) (deCaseReorderKind a1 kind)
deCaseReorderKind a1 (KindVar l qName) = KindVar (id l) (deCaseReorderQName a1 qName)
deCaseReorderKind a1 (KindApp l kind1 kind2) = KindApp (id l) (deCaseReorderKind a1 kind1) (deCaseReorderKind a1 kind2)
deCaseReorderKind a1 (KindTuple l kind) = KindTuple (id l) (fmap (deCaseReorderKind a1) kind)
deCaseReorderKind a1 (KindList l kind) = KindList (id l) (deCaseReorderKind a1 kind)
deCaseReorderLiteral :: SrcInfo l => CollectDataResult -> Literal l -> Literal l
deCaseReorderLiteral a1 (Char l char string) = Char (id l) (id char) (id string)
deCaseReorderLiteral a1 (String l string1 string2) = String (id l) (id string1) (id string2)
deCaseReorderLiteral a1 (Int l integer string) = Int (id l) (id integer) (id string)
deCaseReorderLiteral a1 (Frac l rational string) = Frac (id l) (id rational) (id string)
deCaseReorderLiteral a1 (PrimInt l integer string) = PrimInt (id l) (id integer) (id string)
deCaseReorderLiteral a1 (PrimWord l integer string) = PrimWord (id l) (id integer) (id string)
deCaseReorderLiteral a1 (PrimFloat l rational string) = PrimFloat (id l) (id rational) (id string)
deCaseReorderLiteral a1 (PrimDouble l rational string) = PrimDouble (id l) (id rational) (id string)
deCaseReorderLiteral a1 (PrimChar l char string) = PrimChar (id l) (id char) (id string)
deCaseReorderLiteral a1 (PrimString l string1 string2) = PrimString (id l) (id string1) (id string2)
deCaseReorderMatch :: SrcInfo l => CollectDataResult -> Match l -> Match l
deCaseReorderMatch a1 (Match l name pat rhs binds) = Match (id l) (deCaseReorderName a1 name) (fmap (deCaseReorderPat a1) pat) (deCaseReorderRhs a1 rhs) (fmap (deCaseReorderBinds a1) binds)
deCaseReorderMatch a1 (InfixMatch l pat1 name pat2 rhs binds) = InfixMatch (id l) (deCaseReorderPat a1 pat1) (deCaseReorderName a1 name) (fmap (deCaseReorderPat a1) pat2) (deCaseReorderRhs a1 rhs) (fmap (deCaseReorderBinds a1) binds)
deCaseReorderModule :: SrcInfo l => CollectDataResult -> Module l -> Module l
deCaseReorderModule a1 (Module l moduleHead modulePragma importDecl decl) = Module (id l) (fmap (deCaseReorderModuleHead a1) moduleHead) (fmap (deCaseReorderModulePragma a1) modulePragma) (fmap (deCaseReorderImportDecl a1) importDecl) (fmap (deCaseReorderDecl a1) decl)
deCaseReorderModule a1 (XmlPage l moduleName modulePragma xName xAttr exp1 exp2) = XmlPage (id l) (deCaseReorderModuleName a1 moduleName) (fmap (deCaseReorderModulePragma a1) modulePragma) (deCaseReorderXName a1 xName) (fmap (deCaseReorderXAttr a1) xAttr) (fmap (deCaseReorderExp a1) exp1) (fmap (deCaseReorderExp a1) exp2)
deCaseReorderModule a1 (XmlHybrid l moduleHead modulePragma importDecl decl xName xAttr exp1 exp2) = XmlHybrid (id l) (fmap (deCaseReorderModuleHead a1) moduleHead) (fmap (deCaseReorderModulePragma a1) modulePragma) (fmap (deCaseReorderImportDecl a1) importDecl) (fmap (deCaseReorderDecl a1) decl) (deCaseReorderXName a1 xName) (fmap (deCaseReorderXAttr a1) xAttr) (fmap (deCaseReorderExp a1) exp1) (fmap (deCaseReorderExp a1) exp2)
deCaseReorderModuleHead :: SrcInfo l => CollectDataResult -> ModuleHead l -> ModuleHead l
deCaseReorderModuleHead a1 (ModuleHead l moduleName warningText exportSpecList) = ModuleHead (id l) (deCaseReorderModuleName a1 moduleName) (fmap (deCaseReorderWarningText a1) warningText) (fmap (deCaseReorderExportSpecList a1) exportSpecList)
deCaseReorderModuleName :: SrcInfo l => CollectDataResult -> ModuleName l -> ModuleName l
deCaseReorderModuleName a1 (ModuleName l string) = ModuleName (id l) (id string)
deCaseReorderModulePragma :: SrcInfo l => CollectDataResult -> ModulePragma l -> ModulePragma l
deCaseReorderModulePragma a1 (LanguagePragma l name) = LanguagePragma (id l) (fmap (deCaseReorderName a1) name)
deCaseReorderModulePragma a1 (OptionsPragma l tool string) = OptionsPragma (id l) (fmap (deCaseReorderTool a1) tool) (id string)
deCaseReorderModulePragma a1 (AnnModulePragma l annotation) = AnnModulePragma (id l) (deCaseReorderAnnotation a1 annotation)
deCaseReorderName :: SrcInfo l => CollectDataResult -> Name l -> Name l
deCaseReorderName a1 (Ident l string) = Ident (id l) (id string)
deCaseReorderName a1 (Symbol l string) = Symbol (id l) (id string)
deCaseReorderNamespace :: SrcInfo l => CollectDataResult -> Namespace l -> Namespace l
deCaseReorderNamespace a1 (NoNamespace l) = NoNamespace (id l)
deCaseReorderNamespace a1 (TypeNamespace l) = TypeNamespace (id l)
deCaseReorderNamespace a1 (PatternNamespace l) = PatternNamespace (id l)
deCaseReorderOp :: SrcInfo l => CollectDataResult -> Op l -> Op l
deCaseReorderOp a1 (VarOp l name) = VarOp (id l) (deCaseReorderName a1 name)
deCaseReorderOp a1 (ConOp l name) = ConOp (id l) (deCaseReorderName a1 name)
deCaseReorderOverlap :: SrcInfo l => CollectDataResult -> Overlap l -> Overlap l
deCaseReorderOverlap a1 (NoOverlap l) = NoOverlap (id l)
deCaseReorderOverlap a1 (Overlap l) = Overlap (id l)
deCaseReorderOverlap a1 (Incoherent l) = Incoherent (id l)
deCaseReorderPXAttr :: SrcInfo l => CollectDataResult -> PXAttr l -> PXAttr l
deCaseReorderPXAttr a1 (PXAttr l xName pat) = PXAttr (id l) (deCaseReorderXName a1 xName) (deCaseReorderPat a1 pat)
deCaseReorderPat :: SrcInfo l => CollectDataResult -> Pat l -> Pat l
deCaseReorderPat a1 (PVar l name) = PVar (id l) (deCaseReorderName a1 name)
deCaseReorderPat a1 (PLit l sign literal) = PLit (id l) (deCaseReorderSign a1 sign) (deCaseReorderLiteral a1 literal)
deCaseReorderPat a1 (PNPlusK l name integer) = PNPlusK (id l) (deCaseReorderName a1 name) (id integer)
deCaseReorderPat a1 (PInfixApp l pat1 qName pat2) = PInfixApp (id l) (deCaseReorderPat a1 pat1) (deCaseReorderQName a1 qName) (deCaseReorderPat a1 pat2)
deCaseReorderPat a1 (PApp l qName pat) = PApp (id l) (deCaseReorderQName a1 qName) (fmap (deCaseReorderPat a1) pat)
deCaseReorderPat a1 (PTuple l boxed pat) = PTuple (id l) (deCaseReorderBoxed a1 boxed) (fmap (deCaseReorderPat a1) pat)
deCaseReorderPat a1 (PList l pat) = PList (id l) (fmap (deCaseReorderPat a1) pat)
deCaseReorderPat a1 (PParen l pat) = PParen (id l) (deCaseReorderPat a1 pat)
deCaseReorderPat a1 (PRec l qName patField) = PRec (id l) (deCaseReorderQName a1 qName) (fmap (deCaseReorderPatField a1) patField)
deCaseReorderPat a1 (PAsPat l name pat) = PAsPat (id l) (deCaseReorderName a1 name) (deCaseReorderPat a1 pat)
deCaseReorderPat a1 (PWildCard l) = PWildCard (id l)
deCaseReorderPat a1 (PIrrPat l pat) = PIrrPat (id l) (deCaseReorderPat a1 pat)
deCaseReorderPat a1 (PatTypeSig l pat type0) = PatTypeSig (id l) (deCaseReorderPat a1 pat) (deCaseReorderType a1 type0)
deCaseReorderPat a1 (PViewPat l exp pat) = PViewPat (id l) (deCaseReorderExp a1 exp) (deCaseReorderPat a1 pat)
deCaseReorderPat a1 (PRPat l rPat) = PRPat (id l) (fmap (deCaseReorderRPat a1) rPat)
deCaseReorderPat a1 (PXTag l xName pXAttr pat1 pat2) = PXTag (id l) (deCaseReorderXName a1 xName) (fmap (deCaseReorderPXAttr a1) pXAttr) (fmap (deCaseReorderPat a1) pat1) (fmap (deCaseReorderPat a1) pat2)
deCaseReorderPat a1 (PXETag l xName pXAttr pat) = PXETag (id l) (deCaseReorderXName a1 xName) (fmap (deCaseReorderPXAttr a1) pXAttr) (fmap (deCaseReorderPat a1) pat)
deCaseReorderPat a1 (PXPcdata l string) = PXPcdata (id l) (id string)
deCaseReorderPat a1 (PXPatTag l pat) = PXPatTag (id l) (deCaseReorderPat a1 pat)
deCaseReorderPat a1 (PXRPats l rPat) = PXRPats (id l) (fmap (deCaseReorderRPat a1) rPat)
deCaseReorderPat a1 (PQuasiQuote l string1 string2) = PQuasiQuote (id l) (id string1) (id string2)
deCaseReorderPat a1 (PBangPat l pat) = PBangPat (id l) (deCaseReorderPat a1 pat)
deCaseReorderPatField :: SrcInfo l => CollectDataResult -> PatField l -> PatField l
deCaseReorderPatField a1 (PFieldPat l qName pat) = PFieldPat (id l) (deCaseReorderQName a1 qName) (deCaseReorderPat a1 pat)
deCaseReorderPatField a1 (PFieldPun l qName) = PFieldPun (id l) (deCaseReorderQName a1 qName)
deCaseReorderPatField a1 (PFieldWildcard l) = PFieldWildcard (id l)
deCaseReorderPatternSynDirection :: SrcInfo l => CollectDataResult -> PatternSynDirection l -> PatternSynDirection l
deCaseReorderPatternSynDirection a1 (Unidirectional) = Unidirectional
deCaseReorderPatternSynDirection a1 (ImplicitBidirectional) = ImplicitBidirectional
deCaseReorderPatternSynDirection a1 (ExplicitBidirectional l decl) = ExplicitBidirectional (id l) (fmap (deCaseReorderDecl a1) decl)
deCaseReorderPromoted :: SrcInfo l => CollectDataResult -> Promoted l -> Promoted l
deCaseReorderPromoted a1 (PromotedInteger l integer string) = PromotedInteger (id l) (id integer) (id string)
deCaseReorderPromoted a1 (PromotedString l string1 string2) = PromotedString (id l) (id string1) (id string2)
deCaseReorderPromoted a1 (PromotedCon l bool qName) = PromotedCon (id l) (id bool) (deCaseReorderQName a1 qName)
deCaseReorderPromoted a1 (PromotedList l bool type0) = PromotedList (id l) (id bool) (fmap (deCaseReorderType a1) type0)
deCaseReorderPromoted a1 (PromotedTuple l type0) = PromotedTuple (id l) (fmap (deCaseReorderType a1) type0)
deCaseReorderPromoted a1 (PromotedUnit l) = PromotedUnit (id l)
deCaseReorderQName :: SrcInfo l => CollectDataResult -> QName l -> QName l
deCaseReorderQName a1 (Qual l moduleName name) = Qual (id l) (deCaseReorderModuleName a1 moduleName) (deCaseReorderName a1 name)
deCaseReorderQName a1 (UnQual l name) = UnQual (id l) (deCaseReorderName a1 name)
deCaseReorderQName a1 (Special l specialCon) = Special (id l) (deCaseReorderSpecialCon a1 specialCon)
deCaseReorderQOp :: SrcInfo l => CollectDataResult -> QOp l -> QOp l
deCaseReorderQOp a1 (QVarOp l qName) = QVarOp (id l) (deCaseReorderQName a1 qName)
deCaseReorderQOp a1 (QConOp l qName) = QConOp (id l) (deCaseReorderQName a1 qName)
deCaseReorderQualConDecl :: SrcInfo l => CollectDataResult -> QualConDecl l -> QualConDecl l
deCaseReorderQualConDecl a1 (QualConDecl l tyVarBind context conDecl) = QualConDecl (id l) (fmap (fmap (deCaseReorderTyVarBind a1)) tyVarBind) (fmap (deCaseReorderContext a1) context) (deCaseReorderConDecl a1 conDecl)
deCaseReorderQualStmt :: SrcInfo l => CollectDataResult -> QualStmt l -> QualStmt l
deCaseReorderQualStmt a1 (QualStmt l stmt) = QualStmt (id l) (deCaseReorderStmt a1 stmt)
deCaseReorderQualStmt a1 (ThenTrans l exp) = ThenTrans (id l) (deCaseReorderExp a1 exp)
deCaseReorderQualStmt a1 (ThenBy l exp1 exp2) = ThenBy (id l) (deCaseReorderExp a1 exp1) (deCaseReorderExp a1 exp2)
deCaseReorderQualStmt a1 (GroupBy l exp) = GroupBy (id l) (deCaseReorderExp a1 exp)
deCaseReorderQualStmt a1 (GroupUsing l exp) = GroupUsing (id l) (deCaseReorderExp a1 exp)
deCaseReorderQualStmt a1 (GroupByUsing l exp1 exp2) = GroupByUsing (id l) (deCaseReorderExp a1 exp1) (deCaseReorderExp a1 exp2)
deCaseReorderRPat :: SrcInfo l => CollectDataResult -> RPat l -> RPat l
deCaseReorderRPat a1 (RPOp l rPat rPatOp) = RPOp (id l) (deCaseReorderRPat a1 rPat) (deCaseReorderRPatOp a1 rPatOp)
deCaseReorderRPat a1 (RPEither l rPat1 rPat2) = RPEither (id l) (deCaseReorderRPat a1 rPat1) (deCaseReorderRPat a1 rPat2)
deCaseReorderRPat a1 (RPSeq l rPat) = RPSeq (id l) (fmap (deCaseReorderRPat a1) rPat)
deCaseReorderRPat a1 (RPGuard l pat stmt) = RPGuard (id l) (deCaseReorderPat a1 pat) (fmap (deCaseReorderStmt a1) stmt)
deCaseReorderRPat a1 (RPCAs l name rPat) = RPCAs (id l) (deCaseReorderName a1 name) (deCaseReorderRPat a1 rPat)
deCaseReorderRPat a1 (RPAs l name rPat) = RPAs (id l) (deCaseReorderName a1 name) (deCaseReorderRPat a1 rPat)
deCaseReorderRPat a1 (RPParen l rPat) = RPParen (id l) (deCaseReorderRPat a1 rPat)
deCaseReorderRPat a1 (RPPat l pat) = RPPat (id l) (deCaseReorderPat a1 pat)
deCaseReorderRPatOp :: SrcInfo l => CollectDataResult -> RPatOp l -> RPatOp l
deCaseReorderRPatOp a1 (RPStar l) = RPStar (id l)
deCaseReorderRPatOp a1 (RPStarG l) = RPStarG (id l)
deCaseReorderRPatOp a1 (RPPlus l) = RPPlus (id l)
deCaseReorderRPatOp a1 (RPPlusG l) = RPPlusG (id l)
deCaseReorderRPatOp a1 (RPOpt l) = RPOpt (id l)
deCaseReorderRPatOp a1 (RPOptG l) = RPOptG (id l)
deCaseReorderRhs :: SrcInfo l => CollectDataResult -> Rhs l -> Rhs l
deCaseReorderRhs a1 (UnGuardedRhs l exp) = UnGuardedRhs (id l) (deCaseReorderExp a1 exp)
deCaseReorderRhs a1 (GuardedRhss l guardedRhs) = GuardedRhss (id l) (fmap (deCaseReorderGuardedRhs a1) guardedRhs)
deCaseReorderRole :: SrcInfo l => CollectDataResult -> Role l -> Role l
deCaseReorderRole a1 (Nominal l) = Nominal (id l)
deCaseReorderRole a1 (Representational l) = Representational (id l)
deCaseReorderRole a1 (Phantom l) = Phantom (id l)
deCaseReorderRole a1 (RoleWildcard l) = RoleWildcard (id l)
deCaseReorderRule :: SrcInfo l => CollectDataResult -> Rule l -> Rule l
deCaseReorderRule a1 (Rule l string activation ruleVar exp1 exp2) = Rule (id l) (id string) (fmap (deCaseReorderActivation a1) activation) (fmap (fmap (deCaseReorderRuleVar a1)) ruleVar) (deCaseReorderExp a1 exp1) (deCaseReorderExp a1 exp2)
deCaseReorderRuleVar :: SrcInfo l => CollectDataResult -> RuleVar l -> RuleVar l
deCaseReorderRuleVar a1 (RuleVar l name) = RuleVar (id l) (deCaseReorderName a1 name)
deCaseReorderRuleVar a1 (TypedRuleVar l name type0) = TypedRuleVar (id l) (deCaseReorderName a1 name) (deCaseReorderType a1 type0)
deCaseReorderSafety :: SrcInfo l => CollectDataResult -> Safety l -> Safety l
deCaseReorderSafety a1 (PlayRisky l) = PlayRisky (id l)
deCaseReorderSafety a1 (PlaySafe l bool) = PlaySafe (id l) (id bool)
deCaseReorderSafety a1 (PlayInterruptible l) = PlayInterruptible (id l)
deCaseReorderSign :: SrcInfo l => CollectDataResult -> Sign l -> Sign l
deCaseReorderSign a1 (Signless l) = Signless (id l)
deCaseReorderSign a1 (Negative l) = Negative (id l)
deCaseReorderSpecialCon :: SrcInfo l => CollectDataResult -> SpecialCon l -> SpecialCon l
deCaseReorderSpecialCon a1 (UnitCon l) = UnitCon (id l)
deCaseReorderSpecialCon a1 (ListCon l) = ListCon (id l)
deCaseReorderSpecialCon a1 (FunCon l) = FunCon (id l)
deCaseReorderSpecialCon a1 (TupleCon l boxed int) = TupleCon (id l) (deCaseReorderBoxed a1 boxed) (id int)
deCaseReorderSpecialCon a1 (Cons l) = Cons (id l)
deCaseReorderSpecialCon a1 (UnboxedSingleCon l) = UnboxedSingleCon (id l)
deCaseReorderSplice :: SrcInfo l => CollectDataResult -> Splice l -> Splice l
deCaseReorderSplice a1 (IdSplice l string) = IdSplice (id l) (id string)
deCaseReorderSplice a1 (ParenSplice l exp) = ParenSplice (id l) (deCaseReorderExp a1 exp)
deCaseReorderStmt :: SrcInfo l => CollectDataResult -> Stmt l -> Stmt l
deCaseReorderStmt a1 (Generator l pat exp) = Generator (id l) (deCaseReorderPat a1 pat) (deCaseReorderExp a1 exp)
deCaseReorderStmt a1 (Qualifier l exp) = Qualifier (id l) (deCaseReorderExp a1 exp)
deCaseReorderStmt a1 (LetStmt l binds) = LetStmt (id l) (deCaseReorderBinds a1 binds)
deCaseReorderStmt a1 (RecStmt l stmt) = RecStmt (id l) (fmap (deCaseReorderStmt a1) stmt)
deCaseReorderTool :: CollectDataResult -> Tool -> Tool
deCaseReorderTool a1 (GHC) = GHC
deCaseReorderTool a1 (HUGS) = HUGS
deCaseReorderTool a1 (NHC98) = NHC98
deCaseReorderTool a1 (YHC) = YHC
deCaseReorderTool a1 (HADDOCK) = HADDOCK
deCaseReorderTool a1 (UnknownTool string) = UnknownTool (id string)
deCaseReorderTyVarBind :: SrcInfo l => CollectDataResult -> TyVarBind l -> TyVarBind l
deCaseReorderTyVarBind a1 (KindedVar l name kind) = KindedVar (id l) (deCaseReorderName a1 name) (deCaseReorderKind a1 kind)
deCaseReorderTyVarBind a1 (UnkindedVar l name) = UnkindedVar (id l) (deCaseReorderName a1 name)
deCaseReorderType :: SrcInfo l => CollectDataResult -> Type l -> Type l
deCaseReorderType a1 (TyForall l tyVarBind context type0) = TyForall (id l) (fmap (fmap (deCaseReorderTyVarBind a1)) tyVarBind) (fmap (deCaseReorderContext a1) context) (deCaseReorderType a1 type0)
deCaseReorderType a1 (TyFun l type1 type2) = TyFun (id l) (deCaseReorderType a1 type1) (deCaseReorderType a1 type2)
deCaseReorderType a1 (TyTuple l boxed type0) = TyTuple (id l) (deCaseReorderBoxed a1 boxed) (fmap (deCaseReorderType a1) type0)
deCaseReorderType a1 (TyList l type0) = TyList (id l) (deCaseReorderType a1 type0)
deCaseReorderType a1 (TyParArray l type0) = TyParArray (id l) (deCaseReorderType a1 type0)
deCaseReorderType a1 (TyApp l type1 type2) = TyApp (id l) (deCaseReorderType a1 type1) (deCaseReorderType a1 type2)
deCaseReorderType a1 (TyVar l name) = TyVar (id l) (deCaseReorderName a1 name)
deCaseReorderType a1 (TyCon l qName) = TyCon (id l) (deCaseReorderQName a1 qName)
deCaseReorderType a1 (TyParen l type0) = TyParen (id l) (deCaseReorderType a1 type0)
deCaseReorderType a1 (TyInfix l type1 qName type2) = TyInfix (id l) (deCaseReorderType a1 type1) (deCaseReorderQName a1 qName) (deCaseReorderType a1 type2)
deCaseReorderType a1 (TyKind l type0 kind) = TyKind (id l) (deCaseReorderType a1 type0) (deCaseReorderKind a1 kind)
deCaseReorderType a1 (TyPromoted l promoted) = TyPromoted (id l) (deCaseReorderPromoted a1 promoted)
deCaseReorderType a1 (TyEquals l type1 type2) = TyEquals (id l) (deCaseReorderType a1 type1) (deCaseReorderType a1 type2)
deCaseReorderType a1 (TySplice l splice) = TySplice (id l) (deCaseReorderSplice a1 splice)
deCaseReorderType a1 (TyBang l bangType type0) = TyBang (id l) (deCaseReorderBangType a1 bangType) (deCaseReorderType a1 type0)
deCaseReorderType a1 (TyWildCard l name) = TyWildCard (id l) (fmap (deCaseReorderName a1) name)
deCaseReorderTypeEqn :: SrcInfo l => CollectDataResult -> TypeEqn l -> TypeEqn l
deCaseReorderTypeEqn a1 (TypeEqn l type1 type2) = TypeEqn (id l) (deCaseReorderType a1 type1) (deCaseReorderType a1 type2)
deCaseReorderWarningText :: SrcInfo l => CollectDataResult -> WarningText l -> WarningText l
deCaseReorderWarningText a1 (DeprText l string) = DeprText (id l) (id string)
deCaseReorderWarningText a1 (WarnText l string) = WarnText (id l) (id string)
deCaseReorderXAttr :: SrcInfo l => CollectDataResult -> XAttr l -> XAttr l
deCaseReorderXAttr a1 (XAttr l xName exp) = XAttr (id l) (deCaseReorderXName a1 xName) (deCaseReorderExp a1 exp)
deCaseReorderXName :: SrcInfo l => CollectDataResult -> XName l -> XName l
deCaseReorderXName a1 (XName l string) = XName (id l) (id string)
deCaseReorderXName a1 (XDomName l string1 string2) = XDomName (id l) (id string1) (id string2)

