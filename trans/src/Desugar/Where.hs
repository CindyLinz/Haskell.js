module Desugar.Where where
import Language.Haskell.Exts.Annotated.Syntax
import Control.Arrow ((***))
deWhereActivation :: Activation l -> Activation l
deWhereActivation (ActiveFrom l int) = ActiveFrom (id l) (id int)
deWhereActivation (ActiveUntil l int) = ActiveUntil (id l) (id int)
deWhereAlt :: Alt l -> Alt l
deWhereAlt (Alt l1 pat (UnGuardedRhs l2 exp) (Just binds)) =
  deWhereAlt (Alt l1 pat (UnGuardedRhs l2 (Let l2 binds exp)) Nothing)
deWhereAlt (Alt l pat rhs binds) = Alt l (deWherePat pat) (deWhereRhs rhs) (fmap (deWhereBinds) binds)
deWhereAnnotation :: Annotation l -> Annotation l
deWhereAnnotation (Ann l name exp) = Ann (id l) (deWhereName name) (deWhereExp exp)
deWhereAnnotation (TypeAnn l name exp) = TypeAnn (id l) (deWhereName name) (deWhereExp exp)
deWhereAnnotation (ModuleAnn l exp) = ModuleAnn (id l) (deWhereExp exp)
deWhereAssoc :: Assoc l -> Assoc l
deWhereAssoc (AssocNone l) = AssocNone (id l)
deWhereAssoc (AssocLeft l) = AssocLeft (id l)
deWhereAssoc (AssocRight l) = AssocRight (id l)
deWhereAsst :: Asst l -> Asst l
deWhereAsst (ClassA l qName type0) = ClassA (id l) (deWhereQName qName) (fmap (deWhereType) type0)
deWhereAsst (AppA l name type0) = AppA (id l) (deWhereName name) (fmap (deWhereType) type0)
deWhereAsst (InfixA l type1 qName type2) = InfixA (id l) (deWhereType type1) (deWhereQName qName) (deWhereType type2)
deWhereAsst (IParam l iPName type0) = IParam (id l) (deWhereIPName iPName) (deWhereType type0)
deWhereAsst (EqualP l type1 type2) = EqualP (id l) (deWhereType type1) (deWhereType type2)
deWhereAsst (ParenA l asst) = ParenA (id l) (deWhereAsst asst)
deWhereAsst (WildCardA l name) = WildCardA (id l) (fmap (deWhereName) name)
deWhereBangType :: BangType l -> BangType l
deWhereBangType (BangedTy l) = BangedTy (id l)
deWhereBangType (UnpackedTy l) = UnpackedTy (id l)
deWhereBinds :: Binds l -> Binds l
deWhereBinds (BDecls l decl) = BDecls (id l) (fmap (deWhereDecl) decl)
deWhereBinds (IPBinds l iPBind) = IPBinds (id l) (fmap (deWhereIPBind) iPBind)
deWhereBooleanFormula :: BooleanFormula l -> BooleanFormula l
deWhereBooleanFormula (VarFormula l name) = VarFormula (id l) (deWhereName name)
deWhereBooleanFormula (AndFormula l booleanFormula) = AndFormula (id l) (fmap (deWhereBooleanFormula) booleanFormula)
deWhereBooleanFormula (OrFormula l booleanFormula) = OrFormula (id l) (fmap (deWhereBooleanFormula) booleanFormula)
deWhereBooleanFormula (ParenFormula l booleanFormula) = ParenFormula (id l) (deWhereBooleanFormula booleanFormula)
deWhereBoxed :: Boxed -> Boxed
deWhereBoxed (Boxed) = Boxed
deWhereBoxed (Unboxed) = Unboxed
deWhereBracket :: Bracket l -> Bracket l
deWhereBracket (ExpBracket l exp) = ExpBracket (id l) (deWhereExp exp)
deWhereBracket (PatBracket l pat) = PatBracket (id l) (deWherePat pat)
deWhereBracket (TypeBracket l type0) = TypeBracket (id l) (deWhereType type0)
deWhereBracket (DeclBracket l decl) = DeclBracket (id l) (fmap (deWhereDecl) decl)
deWhereCName :: CName l -> CName l
deWhereCName (VarName l name) = VarName (id l) (deWhereName name)
deWhereCName (ConName l name) = ConName (id l) (deWhereName name)
deWhereCallConv :: CallConv l -> CallConv l
deWhereCallConv (StdCall l) = StdCall (id l)
deWhereCallConv (CCall l) = CCall (id l)
deWhereCallConv (CPlusPlus l) = CPlusPlus (id l)
deWhereCallConv (DotNet l) = DotNet (id l)
deWhereCallConv (Jvm l) = Jvm (id l)
deWhereCallConv (Js l) = Js (id l)
deWhereCallConv (JavaScript l) = JavaScript (id l)
deWhereCallConv (CApi l) = CApi (id l)
deWhereClassDecl :: ClassDecl l -> ClassDecl l
deWhereClassDecl (ClsDecl l decl) = ClsDecl (id l) (deWhereDecl decl)
deWhereClassDecl (ClsDataFam l context declHead kind) = ClsDataFam (id l) (fmap (deWhereContext) context) (deWhereDeclHead declHead) (fmap (deWhereKind) kind)
deWhereClassDecl (ClsTyFam l declHead kind) = ClsTyFam (id l) (deWhereDeclHead declHead) (fmap (deWhereKind) kind)
deWhereClassDecl (ClsTyDef l type1 type2) = ClsTyDef (id l) (deWhereType type1) (deWhereType type2)
deWhereClassDecl (ClsDefSig l name type0) = ClsDefSig (id l) (deWhereName name) (deWhereType type0)
deWhereConDecl :: ConDecl l -> ConDecl l
deWhereConDecl (ConDecl l name type0) = ConDecl (id l) (deWhereName name) (fmap (deWhereType) type0)
deWhereConDecl (InfixConDecl l type1 name type2) = InfixConDecl (id l) (deWhereType type1) (deWhereName name) (deWhereType type2)
deWhereConDecl (RecDecl l name fieldDecl) = RecDecl (id l) (deWhereName name) (fmap (deWhereFieldDecl) fieldDecl)
deWhereContext :: Context l -> Context l
deWhereContext (CxSingle l asst) = CxSingle (id l) (deWhereAsst asst)
deWhereContext (CxTuple l asst) = CxTuple (id l) (fmap (deWhereAsst) asst)
deWhereContext (CxEmpty l) = CxEmpty (id l)
deWhereDataOrNew :: DataOrNew l -> DataOrNew l
deWhereDataOrNew (DataType l) = DataType (id l)
deWhereDataOrNew (NewType l) = NewType (id l)
deWhereDecl :: Decl l -> Decl l
deWhereDecl (TypeDecl l declHead type0) = TypeDecl (id l) (deWhereDeclHead declHead) (deWhereType type0)
deWhereDecl (TypeFamDecl l declHead kind) = TypeFamDecl (id l) (deWhereDeclHead declHead) (fmap (deWhereKind) kind)
deWhereDecl (ClosedTypeFamDecl l declHead kind typeEqn) = ClosedTypeFamDecl (id l) (deWhereDeclHead declHead) (fmap (deWhereKind) kind) (fmap (deWhereTypeEqn) typeEqn)
deWhereDecl (DataDecl l dataOrNew context declHead qualConDecl deriving0) = DataDecl (id l) (deWhereDataOrNew dataOrNew) (fmap (deWhereContext) context) (deWhereDeclHead declHead) (fmap (deWhereQualConDecl) qualConDecl) (fmap (deWhereDeriving) deriving0)
deWhereDecl (GDataDecl l dataOrNew context declHead kind gadtDecl deriving0) = GDataDecl (id l) (deWhereDataOrNew dataOrNew) (fmap (deWhereContext) context) (deWhereDeclHead declHead) (fmap (deWhereKind) kind) (fmap (deWhereGadtDecl) gadtDecl) (fmap (deWhereDeriving) deriving0)
deWhereDecl (DataFamDecl l context declHead kind) = DataFamDecl (id l) (fmap (deWhereContext) context) (deWhereDeclHead declHead) (fmap (deWhereKind) kind)
deWhereDecl (TypeInsDecl l type1 type2) = TypeInsDecl (id l) (deWhereType type1) (deWhereType type2)
deWhereDecl (DataInsDecl l dataOrNew type0 qualConDecl deriving0) = DataInsDecl (id l) (deWhereDataOrNew dataOrNew) (deWhereType type0) (fmap (deWhereQualConDecl) qualConDecl) (fmap (deWhereDeriving) deriving0)
deWhereDecl (GDataInsDecl l dataOrNew type0 kind gadtDecl deriving0) = GDataInsDecl (id l) (deWhereDataOrNew dataOrNew) (deWhereType type0) (fmap (deWhereKind) kind) (fmap (deWhereGadtDecl) gadtDecl) (fmap (deWhereDeriving) deriving0)
deWhereDecl (ClassDecl l context declHead funDep classDecl) = ClassDecl (id l) (fmap (deWhereContext) context) (deWhereDeclHead declHead) (fmap (deWhereFunDep) funDep) (fmap (fmap (deWhereClassDecl)) classDecl)
deWhereDecl (InstDecl l overlap instRule instDecl) = InstDecl (id l) (fmap (deWhereOverlap) overlap) (deWhereInstRule instRule) (fmap (fmap (deWhereInstDecl)) instDecl)
deWhereDecl (DerivDecl l overlap instRule) = DerivDecl (id l) (fmap (deWhereOverlap) overlap) (deWhereInstRule instRule)
deWhereDecl (InfixDecl l assoc int op) = InfixDecl (id l) (deWhereAssoc assoc) (fmap (id) int) (fmap (deWhereOp) op)
deWhereDecl (DefaultDecl l type0) = DefaultDecl (id l) (fmap (deWhereType) type0)
deWhereDecl (SpliceDecl l exp) = SpliceDecl (id l) (deWhereExp exp)
deWhereDecl (TypeSig l name type0) = TypeSig (id l) (fmap (deWhereName) name) (deWhereType type0)
deWhereDecl (PatSynSig l name tyVarBind context1 context2 type0) = PatSynSig (id l) (deWhereName name) (fmap (fmap (deWhereTyVarBind)) tyVarBind) (fmap (deWhereContext) context1) (fmap (deWhereContext) context2) (deWhereType type0)
deWhereDecl (FunBind l match) = FunBind (id l) (fmap (deWhereMatch) match)
deWhereDecl (PatBind l1 pat (UnGuardedRhs l2 exp) (Just binds)) =
  deWhereDecl (PatBind l1 pat (UnGuardedRhs l2 (Let l2 binds exp)) Nothing)
deWhereDecl (PatBind l pat rhs binds) = PatBind l (deWherePat pat) (deWhereRhs rhs) (fmap (deWhereBinds) binds)
deWhereDecl (PatSyn l pat1 pat2 patternSynDirection) = PatSyn (id l) (deWherePat pat1) (deWherePat pat2) (deWherePatternSynDirection patternSynDirection)
deWhereDecl (ForImp l callConv safety string name type0) = ForImp (id l) (deWhereCallConv callConv) (fmap (deWhereSafety) safety) (fmap (id) string) (deWhereName name) (deWhereType type0)
deWhereDecl (ForExp l callConv string name type0) = ForExp (id l) (deWhereCallConv callConv) (fmap (id) string) (deWhereName name) (deWhereType type0)
deWhereDecl (RulePragmaDecl l rule) = RulePragmaDecl (id l) (fmap (deWhereRule) rule)
deWhereDecl (DeprPragmaDecl l name) = DeprPragmaDecl (id l) (fmap (((fmap (deWhereName)) *** (id))) name)
deWhereDecl (WarnPragmaDecl l name) = WarnPragmaDecl (id l) (fmap (((fmap (deWhereName)) *** (id))) name)
deWhereDecl (InlineSig l bool activation qName) = InlineSig (id l) (id bool) (fmap (deWhereActivation) activation) (deWhereQName qName)
deWhereDecl (InlineConlikeSig l activation qName) = InlineConlikeSig (id l) (fmap (deWhereActivation) activation) (deWhereQName qName)
deWhereDecl (SpecSig l activation qName type0) = SpecSig (id l) (fmap (deWhereActivation) activation) (deWhereQName qName) (fmap (deWhereType) type0)
deWhereDecl (SpecInlineSig l bool activation qName type0) = SpecInlineSig (id l) (id bool) (fmap (deWhereActivation) activation) (deWhereQName qName) (fmap (deWhereType) type0)
deWhereDecl (InstSig l instRule) = InstSig (id l) (deWhereInstRule instRule)
deWhereDecl (AnnPragma l annotation) = AnnPragma (id l) (deWhereAnnotation annotation)
deWhereDecl (MinimalPragma l booleanFormula) = MinimalPragma (id l) (fmap (deWhereBooleanFormula) booleanFormula)
deWhereDecl (RoleAnnotDecl l qName role) = RoleAnnotDecl (id l) (deWhereQName qName) (fmap (deWhereRole) role)
deWhereDeclHead :: DeclHead l -> DeclHead l
deWhereDeclHead (DHead l name) = DHead (id l) (deWhereName name)
deWhereDeclHead (DHInfix l tyVarBind name) = DHInfix (id l) (deWhereTyVarBind tyVarBind) (deWhereName name)
deWhereDeclHead (DHParen l declHead) = DHParen (id l) (deWhereDeclHead declHead)
deWhereDeclHead (DHApp l declHead tyVarBind) = DHApp (id l) (deWhereDeclHead declHead) (deWhereTyVarBind tyVarBind)
deWhereDeriving :: Deriving l -> Deriving l
deWhereDeriving (Deriving l instRule) = Deriving (id l) (fmap (deWhereInstRule) instRule)
deWhereExp :: Exp l -> Exp l
deWhereExp (Var l qName) = Var (id l) (deWhereQName qName)
deWhereExp (IPVar l iPName) = IPVar (id l) (deWhereIPName iPName)
deWhereExp (Con l qName) = Con (id l) (deWhereQName qName)
deWhereExp (Lit l literal) = Lit (id l) (deWhereLiteral literal)
deWhereExp (InfixApp l exp1 qOp exp2) = InfixApp (id l) (deWhereExp exp1) (deWhereQOp qOp) (deWhereExp exp2)
deWhereExp (App l exp1 exp2) = App (id l) (deWhereExp exp1) (deWhereExp exp2)
deWhereExp (NegApp l exp) = NegApp (id l) (deWhereExp exp)
deWhereExp (Lambda l pat exp) = Lambda (id l) (fmap (deWherePat) pat) (deWhereExp exp)
deWhereExp (Let l binds exp) = Let (id l) (deWhereBinds binds) (deWhereExp exp)
deWhereExp (If l exp1 exp2 exp3) = If (id l) (deWhereExp exp1) (deWhereExp exp2) (deWhereExp exp3)
deWhereExp (MultiIf l guardedRhs) = MultiIf (id l) (fmap (deWhereGuardedRhs) guardedRhs)
deWhereExp (Case l exp alt) = Case (id l) (deWhereExp exp) (fmap (deWhereAlt) alt)
deWhereExp (Do l stmt) = Do (id l) (fmap (deWhereStmt) stmt)
deWhereExp (MDo l stmt) = MDo (id l) (fmap (deWhereStmt) stmt)
deWhereExp (Tuple l boxed exp) = Tuple (id l) (deWhereBoxed boxed) (fmap (deWhereExp) exp)
deWhereExp (TupleSection l boxed exp) = TupleSection (id l) (deWhereBoxed boxed) (fmap (fmap (deWhereExp)) exp)
deWhereExp (List l exp) = List (id l) (fmap (deWhereExp) exp)
deWhereExp (ParArray l exp) = ParArray (id l) (fmap (deWhereExp) exp)
deWhereExp (Paren l exp) = Paren (id l) (deWhereExp exp)
deWhereExp (LeftSection l exp qOp) = LeftSection (id l) (deWhereExp exp) (deWhereQOp qOp)
deWhereExp (RightSection l qOp exp) = RightSection (id l) (deWhereQOp qOp) (deWhereExp exp)
deWhereExp (RecConstr l qName fieldUpdate) = RecConstr (id l) (deWhereQName qName) (fmap (deWhereFieldUpdate) fieldUpdate)
deWhereExp (RecUpdate l exp fieldUpdate) = RecUpdate (id l) (deWhereExp exp) (fmap (deWhereFieldUpdate) fieldUpdate)
deWhereExp (EnumFrom l exp) = EnumFrom (id l) (deWhereExp exp)
deWhereExp (EnumFromTo l exp1 exp2) = EnumFromTo (id l) (deWhereExp exp1) (deWhereExp exp2)
deWhereExp (EnumFromThen l exp1 exp2) = EnumFromThen (id l) (deWhereExp exp1) (deWhereExp exp2)
deWhereExp (EnumFromThenTo l exp1 exp2 exp3) = EnumFromThenTo (id l) (deWhereExp exp1) (deWhereExp exp2) (deWhereExp exp3)
deWhereExp (ParArrayFromTo l exp1 exp2) = ParArrayFromTo (id l) (deWhereExp exp1) (deWhereExp exp2)
deWhereExp (ParArrayFromThenTo l exp1 exp2 exp3) = ParArrayFromThenTo (id l) (deWhereExp exp1) (deWhereExp exp2) (deWhereExp exp3)
deWhereExp (ListComp l exp qualStmt) = ListComp (id l) (deWhereExp exp) (fmap (deWhereQualStmt) qualStmt)
deWhereExp (ParComp l exp qualStmt) = ParComp (id l) (deWhereExp exp) (fmap (fmap (deWhereQualStmt)) qualStmt)
deWhereExp (ParArrayComp l exp qualStmt) = ParArrayComp (id l) (deWhereExp exp) (fmap (fmap (deWhereQualStmt)) qualStmt)
deWhereExp (ExpTypeSig l exp type0) = ExpTypeSig (id l) (deWhereExp exp) (deWhereType type0)
deWhereExp (VarQuote l qName) = VarQuote (id l) (deWhereQName qName)
deWhereExp (TypQuote l qName) = TypQuote (id l) (deWhereQName qName)
deWhereExp (BracketExp l bracket) = BracketExp (id l) (deWhereBracket bracket)
deWhereExp (SpliceExp l splice) = SpliceExp (id l) (deWhereSplice splice)
deWhereExp (QuasiQuote l string1 string2) = QuasiQuote (id l) (id string1) (id string2)
deWhereExp (XTag l xName xAttr exp1 exp2) = XTag (id l) (deWhereXName xName) (fmap (deWhereXAttr) xAttr) (fmap (deWhereExp) exp1) (fmap (deWhereExp) exp2)
deWhereExp (XETag l xName xAttr exp) = XETag (id l) (deWhereXName xName) (fmap (deWhereXAttr) xAttr) (fmap (deWhereExp) exp)
deWhereExp (XPcdata l string) = XPcdata (id l) (id string)
deWhereExp (XExpTag l exp) = XExpTag (id l) (deWhereExp exp)
deWhereExp (XChildTag l exp) = XChildTag (id l) (fmap (deWhereExp) exp)
deWhereExp (CorePragma l string exp) = CorePragma (id l) (id string) (deWhereExp exp)
deWhereExp (SCCPragma l string exp) = SCCPragma (id l) (id string) (deWhereExp exp)
deWhereExp (GenPragma l string int1 int2 exp) = GenPragma (id l) (id string) (((id) *** (id)) int1) (((id) *** (id)) int2) (deWhereExp exp)
deWhereExp (Proc l pat exp) = Proc (id l) (deWherePat pat) (deWhereExp exp)
deWhereExp (LeftArrApp l exp1 exp2) = LeftArrApp (id l) (deWhereExp exp1) (deWhereExp exp2)
deWhereExp (RightArrApp l exp1 exp2) = RightArrApp (id l) (deWhereExp exp1) (deWhereExp exp2)
deWhereExp (LeftArrHighApp l exp1 exp2) = LeftArrHighApp (id l) (deWhereExp exp1) (deWhereExp exp2)
deWhereExp (RightArrHighApp l exp1 exp2) = RightArrHighApp (id l) (deWhereExp exp1) (deWhereExp exp2)
deWhereExp (LCase l alt) = LCase (id l) (fmap (deWhereAlt) alt)
deWhereExp (ExprHole l) = ExprHole (id l)
deWhereExportSpec :: ExportSpec l -> ExportSpec l
deWhereExportSpec (EVar l qName) = EVar (id l) (deWhereQName qName)
deWhereExportSpec (EAbs l namespace qName) = EAbs (id l) (deWhereNamespace namespace) (deWhereQName qName)
deWhereExportSpec (EThingAll l qName) = EThingAll (id l) (deWhereQName qName)
deWhereExportSpec (EThingWith l qName cName) = EThingWith (id l) (deWhereQName qName) (fmap (deWhereCName) cName)
deWhereExportSpec (EModuleContents l moduleName) = EModuleContents (id l) (deWhereModuleName moduleName)
deWhereExportSpecList :: ExportSpecList l -> ExportSpecList l
deWhereExportSpecList (ExportSpecList l exportSpec) = ExportSpecList (id l) (fmap (deWhereExportSpec) exportSpec)
deWhereFieldDecl :: FieldDecl l -> FieldDecl l
deWhereFieldDecl (FieldDecl l name type0) = FieldDecl (id l) (fmap (deWhereName) name) (deWhereType type0)
deWhereFieldUpdate :: FieldUpdate l -> FieldUpdate l
deWhereFieldUpdate (FieldUpdate l qName exp) = FieldUpdate (id l) (deWhereQName qName) (deWhereExp exp)
deWhereFieldUpdate (FieldPun l qName) = FieldPun (id l) (deWhereQName qName)
deWhereFieldUpdate (FieldWildcard l) = FieldWildcard (id l)
deWhereFunDep :: FunDep l -> FunDep l
deWhereFunDep (FunDep l name1 name2) = FunDep (id l) (fmap (deWhereName) name1) (fmap (deWhereName) name2)
deWhereGadtDecl :: GadtDecl l -> GadtDecl l
deWhereGadtDecl (GadtDecl l name fieldDecl type0) = GadtDecl (id l) (deWhereName name) (fmap (fmap (deWhereFieldDecl)) fieldDecl) (deWhereType type0)
deWhereGuardedRhs :: GuardedRhs l -> GuardedRhs l
deWhereGuardedRhs (GuardedRhs l stmt exp) = GuardedRhs (id l) (fmap (deWhereStmt) stmt) (deWhereExp exp)
deWhereIPBind :: IPBind l -> IPBind l
deWhereIPBind (IPBind l iPName exp) = IPBind (id l) (deWhereIPName iPName) (deWhereExp exp)
deWhereIPName :: IPName l -> IPName l
deWhereIPName (IPDup l string) = IPDup (id l) (id string)
deWhereIPName (IPLin l string) = IPLin (id l) (id string)
deWhereImportDecl :: ImportDecl l -> ImportDecl l
deWhereImportDecl (ImportDecl importAnn importModule importQualified importSrc importSafe importPkg importAs importSpecs) = ImportDecl (id importAnn) (deWhereModuleName importModule) (id importQualified) (id importSrc) (id importSafe) (fmap (id) importPkg) (fmap (deWhereModuleName) importAs) (fmap (deWhereImportSpecList) importSpecs)
deWhereImportSpec :: ImportSpec l -> ImportSpec l
deWhereImportSpec (IVar l name) = IVar (id l) (deWhereName name)
deWhereImportSpec (IAbs l namespace name) = IAbs (id l) (deWhereNamespace namespace) (deWhereName name)
deWhereImportSpec (IThingAll l name) = IThingAll (id l) (deWhereName name)
deWhereImportSpec (IThingWith l name cName) = IThingWith (id l) (deWhereName name) (fmap (deWhereCName) cName)
deWhereImportSpecList :: ImportSpecList l -> ImportSpecList l
deWhereImportSpecList (ImportSpecList l bool importSpec) = ImportSpecList (id l) (id bool) (fmap (deWhereImportSpec) importSpec)
deWhereInstDecl :: InstDecl l -> InstDecl l
deWhereInstDecl (InsDecl l decl) = InsDecl (id l) (deWhereDecl decl)
deWhereInstDecl (InsType l type1 type2) = InsType (id l) (deWhereType type1) (deWhereType type2)
deWhereInstDecl (InsData l dataOrNew type0 qualConDecl deriving0) = InsData (id l) (deWhereDataOrNew dataOrNew) (deWhereType type0) (fmap (deWhereQualConDecl) qualConDecl) (fmap (deWhereDeriving) deriving0)
deWhereInstDecl (InsGData l dataOrNew type0 kind gadtDecl deriving0) = InsGData (id l) (deWhereDataOrNew dataOrNew) (deWhereType type0) (fmap (deWhereKind) kind) (fmap (deWhereGadtDecl) gadtDecl) (fmap (deWhereDeriving) deriving0)
deWhereInstHead :: InstHead l -> InstHead l
deWhereInstHead (IHCon l qName) = IHCon (id l) (deWhereQName qName)
deWhereInstHead (IHInfix l type0 qName) = IHInfix (id l) (deWhereType type0) (deWhereQName qName)
deWhereInstHead (IHParen l instHead) = IHParen (id l) (deWhereInstHead instHead)
deWhereInstHead (IHApp l instHead type0) = IHApp (id l) (deWhereInstHead instHead) (deWhereType type0)
deWhereInstRule :: InstRule l -> InstRule l
deWhereInstRule (IRule l tyVarBind context instHead) = IRule (id l) (fmap (fmap (deWhereTyVarBind)) tyVarBind) (fmap (deWhereContext) context) (deWhereInstHead instHead)
deWhereInstRule (IParen l instRule) = IParen (id l) (deWhereInstRule instRule)
deWhereKind :: Kind l -> Kind l
deWhereKind (KindStar l) = KindStar (id l)
deWhereKind (KindFn l kind1 kind2) = KindFn (id l) (deWhereKind kind1) (deWhereKind kind2)
deWhereKind (KindParen l kind) = KindParen (id l) (deWhereKind kind)
deWhereKind (KindVar l qName) = KindVar (id l) (deWhereQName qName)
deWhereKind (KindApp l kind1 kind2) = KindApp (id l) (deWhereKind kind1) (deWhereKind kind2)
deWhereKind (KindTuple l kind) = KindTuple (id l) (fmap (deWhereKind) kind)
deWhereKind (KindList l kind) = KindList (id l) (deWhereKind kind)
deWhereLiteral :: Literal l -> Literal l
deWhereLiteral (Char l char string) = Char (id l) (id char) (id string)
deWhereLiteral (String l string1 string2) = String (id l) (id string1) (id string2)
deWhereLiteral (Int l integer string) = Int (id l) (id integer) (id string)
deWhereLiteral (Frac l rational string) = Frac (id l) (id rational) (id string)
deWhereLiteral (PrimInt l integer string) = PrimInt (id l) (id integer) (id string)
deWhereLiteral (PrimWord l integer string) = PrimWord (id l) (id integer) (id string)
deWhereLiteral (PrimFloat l rational string) = PrimFloat (id l) (id rational) (id string)
deWhereLiteral (PrimDouble l rational string) = PrimDouble (id l) (id rational) (id string)
deWhereLiteral (PrimChar l char string) = PrimChar (id l) (id char) (id string)
deWhereLiteral (PrimString l string1 string2) = PrimString (id l) (id string1) (id string2)
deWhereMatch :: Match l -> Match l
deWhereMatch (Match l1 name pat (UnGuardedRhs l2 exp) (Just binds)) =
  deWhereMatch (Match l1 name pat (UnGuardedRhs l2 (Let l2 binds exp)) Nothing)
deWhereMatch (Match l name pat rhs binds) = Match l (deWhereName name) (fmap (deWherePat) pat) (deWhereRhs rhs) (fmap (deWhereBinds) binds)
deWhereMatch (InfixMatch l pat1 name pat2 rhs binds) = InfixMatch (id l) (deWherePat pat1) (deWhereName name) (fmap (deWherePat) pat2) (deWhereRhs rhs) (fmap (deWhereBinds) binds)
deWhereModule :: Module l -> Module l
deWhereModule (Module l moduleHead modulePragma importDecl decl) = Module (id l) (fmap (deWhereModuleHead) moduleHead) (fmap (deWhereModulePragma) modulePragma) (fmap (deWhereImportDecl) importDecl) (fmap (deWhereDecl) decl)
deWhereModule (XmlPage l moduleName modulePragma xName xAttr exp1 exp2) = XmlPage (id l) (deWhereModuleName moduleName) (fmap (deWhereModulePragma) modulePragma) (deWhereXName xName) (fmap (deWhereXAttr) xAttr) (fmap (deWhereExp) exp1) (fmap (deWhereExp) exp2)
deWhereModule (XmlHybrid l moduleHead modulePragma importDecl decl xName xAttr exp1 exp2) = XmlHybrid (id l) (fmap (deWhereModuleHead) moduleHead) (fmap (deWhereModulePragma) modulePragma) (fmap (deWhereImportDecl) importDecl) (fmap (deWhereDecl) decl) (deWhereXName xName) (fmap (deWhereXAttr) xAttr) (fmap (deWhereExp) exp1) (fmap (deWhereExp) exp2)
deWhereModuleHead :: ModuleHead l -> ModuleHead l
deWhereModuleHead (ModuleHead l moduleName warningText exportSpecList) = ModuleHead (id l) (deWhereModuleName moduleName) (fmap (deWhereWarningText) warningText) (fmap (deWhereExportSpecList) exportSpecList)
deWhereModuleName :: ModuleName l -> ModuleName l
deWhereModuleName (ModuleName l string) = ModuleName (id l) (id string)
deWhereModulePragma :: ModulePragma l -> ModulePragma l
deWhereModulePragma (LanguagePragma l name) = LanguagePragma (id l) (fmap (deWhereName) name)
deWhereModulePragma (OptionsPragma l tool string) = OptionsPragma (id l) (fmap (deWhereTool) tool) (id string)
deWhereModulePragma (AnnModulePragma l annotation) = AnnModulePragma (id l) (deWhereAnnotation annotation)
deWhereName :: Name l -> Name l
deWhereName (Ident l string) = Ident (id l) (id string)
deWhereName (Symbol l string) = Symbol (id l) (id string)
deWhereNamespace :: Namespace l -> Namespace l
deWhereNamespace (NoNamespace l) = NoNamespace (id l)
deWhereNamespace (TypeNamespace l) = TypeNamespace (id l)
deWhereNamespace (PatternNamespace l) = PatternNamespace (id l)
deWhereOp :: Op l -> Op l
deWhereOp (VarOp l name) = VarOp (id l) (deWhereName name)
deWhereOp (ConOp l name) = ConOp (id l) (deWhereName name)
deWhereOverlap :: Overlap l -> Overlap l
deWhereOverlap (NoOverlap l) = NoOverlap (id l)
deWhereOverlap (Overlap l) = Overlap (id l)
deWhereOverlap (Incoherent l) = Incoherent (id l)
deWherePXAttr :: PXAttr l -> PXAttr l
deWherePXAttr (PXAttr l xName pat) = PXAttr (id l) (deWhereXName xName) (deWherePat pat)
deWherePat :: Pat l -> Pat l
deWherePat (PVar l name) = PVar (id l) (deWhereName name)
deWherePat (PLit l sign literal) = PLit (id l) (deWhereSign sign) (deWhereLiteral literal)
deWherePat (PNPlusK l name integer) = PNPlusK (id l) (deWhereName name) (id integer)
deWherePat (PInfixApp l pat1 qName pat2) = PInfixApp (id l) (deWherePat pat1) (deWhereQName qName) (deWherePat pat2)
deWherePat (PApp l qName pat) = PApp (id l) (deWhereQName qName) (fmap (deWherePat) pat)
deWherePat (PTuple l boxed pat) = PTuple (id l) (deWhereBoxed boxed) (fmap (deWherePat) pat)
deWherePat (PList l pat) = PList (id l) (fmap (deWherePat) pat)
deWherePat (PParen l pat) = PParen (id l) (deWherePat pat)
deWherePat (PRec l qName patField) = PRec (id l) (deWhereQName qName) (fmap (deWherePatField) patField)
deWherePat (PAsPat l name pat) = PAsPat (id l) (deWhereName name) (deWherePat pat)
deWherePat (PWildCard l) = PWildCard (id l)
deWherePat (PIrrPat l pat) = PIrrPat (id l) (deWherePat pat)
deWherePat (PatTypeSig l pat type0) = PatTypeSig (id l) (deWherePat pat) (deWhereType type0)
deWherePat (PViewPat l exp pat) = PViewPat (id l) (deWhereExp exp) (deWherePat pat)
deWherePat (PRPat l rPat) = PRPat (id l) (fmap (deWhereRPat) rPat)
deWherePat (PXTag l xName pXAttr pat1 pat2) = PXTag (id l) (deWhereXName xName) (fmap (deWherePXAttr) pXAttr) (fmap (deWherePat) pat1) (fmap (deWherePat) pat2)
deWherePat (PXETag l xName pXAttr pat) = PXETag (id l) (deWhereXName xName) (fmap (deWherePXAttr) pXAttr) (fmap (deWherePat) pat)
deWherePat (PXPcdata l string) = PXPcdata (id l) (id string)
deWherePat (PXPatTag l pat) = PXPatTag (id l) (deWherePat pat)
deWherePat (PXRPats l rPat) = PXRPats (id l) (fmap (deWhereRPat) rPat)
deWherePat (PQuasiQuote l string1 string2) = PQuasiQuote (id l) (id string1) (id string2)
deWherePat (PBangPat l pat) = PBangPat (id l) (deWherePat pat)
deWherePatField :: PatField l -> PatField l
deWherePatField (PFieldPat l qName pat) = PFieldPat (id l) (deWhereQName qName) (deWherePat pat)
deWherePatField (PFieldPun l qName) = PFieldPun (id l) (deWhereQName qName)
deWherePatField (PFieldWildcard l) = PFieldWildcard (id l)
deWherePatternSynDirection :: PatternSynDirection l -> PatternSynDirection l
deWherePatternSynDirection (Unidirectional) = Unidirectional
deWherePatternSynDirection (ImplicitBidirectional) = ImplicitBidirectional
deWherePatternSynDirection (ExplicitBidirectional l decl) = ExplicitBidirectional (id l) (fmap (deWhereDecl) decl)
deWherePromoted :: Promoted l -> Promoted l
deWherePromoted (PromotedInteger l integer string) = PromotedInteger (id l) (id integer) (id string)
deWherePromoted (PromotedString l string1 string2) = PromotedString (id l) (id string1) (id string2)
deWherePromoted (PromotedCon l bool qName) = PromotedCon (id l) (id bool) (deWhereQName qName)
deWherePromoted (PromotedList l bool type0) = PromotedList (id l) (id bool) (fmap (deWhereType) type0)
deWherePromoted (PromotedTuple l type0) = PromotedTuple (id l) (fmap (deWhereType) type0)
deWherePromoted (PromotedUnit l) = PromotedUnit (id l)
deWhereQName :: QName l -> QName l
deWhereQName (Qual l moduleName name) = Qual (id l) (deWhereModuleName moduleName) (deWhereName name)
deWhereQName (UnQual l name) = UnQual (id l) (deWhereName name)
deWhereQName (Special l specialCon) = Special (id l) (deWhereSpecialCon specialCon)
deWhereQOp :: QOp l -> QOp l
deWhereQOp (QVarOp l qName) = QVarOp (id l) (deWhereQName qName)
deWhereQOp (QConOp l qName) = QConOp (id l) (deWhereQName qName)
deWhereQualConDecl :: QualConDecl l -> QualConDecl l
deWhereQualConDecl (QualConDecl l tyVarBind context conDecl) = QualConDecl (id l) (fmap (fmap (deWhereTyVarBind)) tyVarBind) (fmap (deWhereContext) context) (deWhereConDecl conDecl)
deWhereQualStmt :: QualStmt l -> QualStmt l
deWhereQualStmt (QualStmt l stmt) = QualStmt (id l) (deWhereStmt stmt)
deWhereQualStmt (ThenTrans l exp) = ThenTrans (id l) (deWhereExp exp)
deWhereQualStmt (ThenBy l exp1 exp2) = ThenBy (id l) (deWhereExp exp1) (deWhereExp exp2)
deWhereQualStmt (GroupBy l exp) = GroupBy (id l) (deWhereExp exp)
deWhereQualStmt (GroupUsing l exp) = GroupUsing (id l) (deWhereExp exp)
deWhereQualStmt (GroupByUsing l exp1 exp2) = GroupByUsing (id l) (deWhereExp exp1) (deWhereExp exp2)
deWhereRPat :: RPat l -> RPat l
deWhereRPat (RPOp l rPat rPatOp) = RPOp (id l) (deWhereRPat rPat) (deWhereRPatOp rPatOp)
deWhereRPat (RPEither l rPat1 rPat2) = RPEither (id l) (deWhereRPat rPat1) (deWhereRPat rPat2)
deWhereRPat (RPSeq l rPat) = RPSeq (id l) (fmap (deWhereRPat) rPat)
deWhereRPat (RPGuard l pat stmt) = RPGuard (id l) (deWherePat pat) (fmap (deWhereStmt) stmt)
deWhereRPat (RPCAs l name rPat) = RPCAs (id l) (deWhereName name) (deWhereRPat rPat)
deWhereRPat (RPAs l name rPat) = RPAs (id l) (deWhereName name) (deWhereRPat rPat)
deWhereRPat (RPParen l rPat) = RPParen (id l) (deWhereRPat rPat)
deWhereRPat (RPPat l pat) = RPPat (id l) (deWherePat pat)
deWhereRPatOp :: RPatOp l -> RPatOp l
deWhereRPatOp (RPStar l) = RPStar (id l)
deWhereRPatOp (RPStarG l) = RPStarG (id l)
deWhereRPatOp (RPPlus l) = RPPlus (id l)
deWhereRPatOp (RPPlusG l) = RPPlusG (id l)
deWhereRPatOp (RPOpt l) = RPOpt (id l)
deWhereRPatOp (RPOptG l) = RPOptG (id l)
deWhereRhs :: Rhs l -> Rhs l
deWhereRhs (UnGuardedRhs l exp) = UnGuardedRhs (id l) (deWhereExp exp)
deWhereRhs (GuardedRhss l guardedRhs) = GuardedRhss (id l) (fmap (deWhereGuardedRhs) guardedRhs)
deWhereRole :: Role l -> Role l
deWhereRole (Nominal l) = Nominal (id l)
deWhereRole (Representational l) = Representational (id l)
deWhereRole (Phantom l) = Phantom (id l)
deWhereRole (RoleWildcard l) = RoleWildcard (id l)
deWhereRule :: Rule l -> Rule l
deWhereRule (Rule l string activation ruleVar exp1 exp2) = Rule (id l) (id string) (fmap (deWhereActivation) activation) (fmap (fmap (deWhereRuleVar)) ruleVar) (deWhereExp exp1) (deWhereExp exp2)
deWhereRuleVar :: RuleVar l -> RuleVar l
deWhereRuleVar (RuleVar l name) = RuleVar (id l) (deWhereName name)
deWhereRuleVar (TypedRuleVar l name type0) = TypedRuleVar (id l) (deWhereName name) (deWhereType type0)
deWhereSafety :: Safety l -> Safety l
deWhereSafety (PlayRisky l) = PlayRisky (id l)
deWhereSafety (PlaySafe l bool) = PlaySafe (id l) (id bool)
deWhereSafety (PlayInterruptible l) = PlayInterruptible (id l)
deWhereSign :: Sign l -> Sign l
deWhereSign (Signless l) = Signless (id l)
deWhereSign (Negative l) = Negative (id l)
deWhereSpecialCon :: SpecialCon l -> SpecialCon l
deWhereSpecialCon (UnitCon l) = UnitCon (id l)
deWhereSpecialCon (ListCon l) = ListCon (id l)
deWhereSpecialCon (FunCon l) = FunCon (id l)
deWhereSpecialCon (TupleCon l boxed int) = TupleCon (id l) (deWhereBoxed boxed) (id int)
deWhereSpecialCon (Cons l) = Cons (id l)
deWhereSpecialCon (UnboxedSingleCon l) = UnboxedSingleCon (id l)
deWhereSplice :: Splice l -> Splice l
deWhereSplice (IdSplice l string) = IdSplice (id l) (id string)
deWhereSplice (ParenSplice l exp) = ParenSplice (id l) (deWhereExp exp)
deWhereStmt :: Stmt l -> Stmt l
deWhereStmt (Generator l pat exp) = Generator (id l) (deWherePat pat) (deWhereExp exp)
deWhereStmt (Qualifier l exp) = Qualifier (id l) (deWhereExp exp)
deWhereStmt (LetStmt l binds) = LetStmt (id l) (deWhereBinds binds)
deWhereStmt (RecStmt l stmt) = RecStmt (id l) (fmap (deWhereStmt) stmt)
deWhereTool :: Tool -> Tool
deWhereTool (GHC) = GHC
deWhereTool (HUGS) = HUGS
deWhereTool (NHC98) = NHC98
deWhereTool (YHC) = YHC
deWhereTool (HADDOCK) = HADDOCK
deWhereTool (UnknownTool string) = UnknownTool (id string)
deWhereTyVarBind :: TyVarBind l -> TyVarBind l
deWhereTyVarBind (KindedVar l name kind) = KindedVar (id l) (deWhereName name) (deWhereKind kind)
deWhereTyVarBind (UnkindedVar l name) = UnkindedVar (id l) (deWhereName name)
deWhereType :: Type l -> Type l
deWhereType (TyForall l tyVarBind context type0) = TyForall (id l) (fmap (fmap (deWhereTyVarBind)) tyVarBind) (fmap (deWhereContext) context) (deWhereType type0)
deWhereType (TyFun l type1 type2) = TyFun (id l) (deWhereType type1) (deWhereType type2)
deWhereType (TyTuple l boxed type0) = TyTuple (id l) (deWhereBoxed boxed) (fmap (deWhereType) type0)
deWhereType (TyList l type0) = TyList (id l) (deWhereType type0)
deWhereType (TyParArray l type0) = TyParArray (id l) (deWhereType type0)
deWhereType (TyApp l type1 type2) = TyApp (id l) (deWhereType type1) (deWhereType type2)
deWhereType (TyVar l name) = TyVar (id l) (deWhereName name)
deWhereType (TyCon l qName) = TyCon (id l) (deWhereQName qName)
deWhereType (TyParen l type0) = TyParen (id l) (deWhereType type0)
deWhereType (TyInfix l type1 qName type2) = TyInfix (id l) (deWhereType type1) (deWhereQName qName) (deWhereType type2)
deWhereType (TyKind l type0 kind) = TyKind (id l) (deWhereType type0) (deWhereKind kind)
deWhereType (TyPromoted l promoted) = TyPromoted (id l) (deWherePromoted promoted)
deWhereType (TyEquals l type1 type2) = TyEquals (id l) (deWhereType type1) (deWhereType type2)
deWhereType (TySplice l splice) = TySplice (id l) (deWhereSplice splice)
deWhereType (TyBang l bangType type0) = TyBang (id l) (deWhereBangType bangType) (deWhereType type0)
deWhereType (TyWildCard l name) = TyWildCard (id l) (fmap (deWhereName) name)
deWhereTypeEqn :: TypeEqn l -> TypeEqn l
deWhereTypeEqn (TypeEqn l type1 type2) = TypeEqn (id l) (deWhereType type1) (deWhereType type2)
deWhereWarningText :: WarningText l -> WarningText l
deWhereWarningText (DeprText l string) = DeprText (id l) (id string)
deWhereWarningText (WarnText l string) = WarnText (id l) (id string)
deWhereXAttr :: XAttr l -> XAttr l
deWhereXAttr (XAttr l xName exp) = XAttr (id l) (deWhereXName xName) (deWhereExp exp)
deWhereXName :: XName l -> XName l
deWhereXName (XName l string) = XName (id l) (id string)
deWhereXName (XDomName l string1 string2) = XDomName (id l) (id string1) (id string2)

