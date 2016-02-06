module Desugar.Pattern where
import Language.Haskell.Exts.Annotated.Syntax
import Control.Arrow ((***))
dePatternActivation :: Activation l -> Activation l
dePatternActivation (ActiveFrom l int) = ActiveFrom (id l) (id int)
dePatternActivation (ActiveUntil l int) = ActiveUntil (id l) (id int)
dePatternAlt :: Alt l -> Alt l
dePatternAlt (Alt l pat rhs binds) = Alt (id l) (dePatternPat pat) (dePatternRhs rhs) (fmap (dePatternBinds) binds)
dePatternAnnotation :: Annotation l -> Annotation l
dePatternAnnotation (Ann l name exp) = Ann (id l) (dePatternName name) (dePatternExp exp)
dePatternAnnotation (TypeAnn l name exp) = TypeAnn (id l) (dePatternName name) (dePatternExp exp)
dePatternAnnotation (ModuleAnn l exp) = ModuleAnn (id l) (dePatternExp exp)
dePatternAssoc :: Assoc l -> Assoc l
dePatternAssoc (AssocNone l) = AssocNone (id l)
dePatternAssoc (AssocLeft l) = AssocLeft (id l)
dePatternAssoc (AssocRight l) = AssocRight (id l)
dePatternAsst :: Asst l -> Asst l
dePatternAsst (ClassA l qName type0) = ClassA (id l) (dePatternQName qName) (fmap (dePatternType) type0)
dePatternAsst (AppA l name type0) = AppA (id l) (dePatternName name) (fmap (dePatternType) type0)
dePatternAsst (InfixA l type1 qName type2) = InfixA (id l) (dePatternType type1) (dePatternQName qName) (dePatternType type2)
dePatternAsst (IParam l iPName type0) = IParam (id l) (dePatternIPName iPName) (dePatternType type0)
dePatternAsst (EqualP l type1 type2) = EqualP (id l) (dePatternType type1) (dePatternType type2)
dePatternAsst (ParenA l asst) = ParenA (id l) (dePatternAsst asst)
dePatternAsst (WildCardA l name) = WildCardA (id l) (fmap (dePatternName) name)
dePatternBangType :: BangType l -> BangType l
dePatternBangType (BangedTy l) = BangedTy (id l)
dePatternBangType (UnpackedTy l) = UnpackedTy (id l)
dePatternBinds :: Binds l -> Binds l
dePatternBinds (BDecls l decl) = BDecls (id l) (fmap (dePatternDecl) decl)
dePatternBinds (IPBinds l iPBind) = IPBinds (id l) (fmap (dePatternIPBind) iPBind)
dePatternBooleanFormula :: BooleanFormula l -> BooleanFormula l
dePatternBooleanFormula (VarFormula l name) = VarFormula (id l) (dePatternName name)
dePatternBooleanFormula (AndFormula l booleanFormula) = AndFormula (id l) (fmap (dePatternBooleanFormula) booleanFormula)
dePatternBooleanFormula (OrFormula l booleanFormula) = OrFormula (id l) (fmap (dePatternBooleanFormula) booleanFormula)
dePatternBooleanFormula (ParenFormula l booleanFormula) = ParenFormula (id l) (dePatternBooleanFormula booleanFormula)
dePatternBoxed :: Boxed -> Boxed
dePatternBoxed (Boxed) = Boxed
dePatternBoxed (Unboxed) = Unboxed
dePatternBracket :: Bracket l -> Bracket l
dePatternBracket (ExpBracket l exp) = ExpBracket (id l) (dePatternExp exp)
dePatternBracket (PatBracket l pat) = PatBracket (id l) (dePatternPat pat)
dePatternBracket (TypeBracket l type0) = TypeBracket (id l) (dePatternType type0)
dePatternBracket (DeclBracket l decl) = DeclBracket (id l) (fmap (dePatternDecl) decl)
dePatternCName :: CName l -> CName l
dePatternCName (VarName l name) = VarName (id l) (dePatternName name)
dePatternCName (ConName l name) = ConName (id l) (dePatternName name)
dePatternCallConv :: CallConv l -> CallConv l
dePatternCallConv (StdCall l) = StdCall (id l)
dePatternCallConv (CCall l) = CCall (id l)
dePatternCallConv (CPlusPlus l) = CPlusPlus (id l)
dePatternCallConv (DotNet l) = DotNet (id l)
dePatternCallConv (Jvm l) = Jvm (id l)
dePatternCallConv (Js l) = Js (id l)
dePatternCallConv (JavaScript l) = JavaScript (id l)
dePatternCallConv (CApi l) = CApi (id l)
dePatternClassDecl :: ClassDecl l -> ClassDecl l
dePatternClassDecl (ClsDecl l decl) = ClsDecl (id l) (dePatternDecl decl)
dePatternClassDecl (ClsDataFam l context declHead kind) = ClsDataFam (id l) (fmap (dePatternContext) context) (dePatternDeclHead declHead) (fmap (dePatternKind) kind)
dePatternClassDecl (ClsTyFam l declHead kind) = ClsTyFam (id l) (dePatternDeclHead declHead) (fmap (dePatternKind) kind)
dePatternClassDecl (ClsTyDef l type1 type2) = ClsTyDef (id l) (dePatternType type1) (dePatternType type2)
dePatternClassDecl (ClsDefSig l name type0) = ClsDefSig (id l) (dePatternName name) (dePatternType type0)
dePatternConDecl :: ConDecl l -> ConDecl l
dePatternConDecl (ConDecl l name type0) = ConDecl (id l) (dePatternName name) (fmap (dePatternType) type0)
dePatternConDecl (InfixConDecl l type1 name type2) = InfixConDecl (id l) (dePatternType type1) (dePatternName name) (dePatternType type2)
dePatternConDecl (RecDecl l name fieldDecl) = RecDecl (id l) (dePatternName name) (fmap (dePatternFieldDecl) fieldDecl)
dePatternContext :: Context l -> Context l
dePatternContext (CxSingle l asst) = CxSingle (id l) (dePatternAsst asst)
dePatternContext (CxTuple l asst) = CxTuple (id l) (fmap (dePatternAsst) asst)
dePatternContext (CxEmpty l) = CxEmpty (id l)
dePatternDataOrNew :: DataOrNew l -> DataOrNew l
dePatternDataOrNew (DataType l) = DataType (id l)
dePatternDataOrNew (NewType l) = NewType (id l)
dePatternDecl :: Decl l -> Decl l
dePatternDecl (TypeDecl l declHead type0) = TypeDecl (id l) (dePatternDeclHead declHead) (dePatternType type0)
dePatternDecl (TypeFamDecl l declHead kind) = TypeFamDecl (id l) (dePatternDeclHead declHead) (fmap (dePatternKind) kind)
dePatternDecl (ClosedTypeFamDecl l declHead kind typeEqn) = ClosedTypeFamDecl (id l) (dePatternDeclHead declHead) (fmap (dePatternKind) kind) (fmap (dePatternTypeEqn) typeEqn)
dePatternDecl (DataDecl l dataOrNew context declHead qualConDecl deriving0) = DataDecl (id l) (dePatternDataOrNew dataOrNew) (fmap (dePatternContext) context) (dePatternDeclHead declHead) (fmap (dePatternQualConDecl) qualConDecl) (fmap (dePatternDeriving) deriving0)
dePatternDecl (GDataDecl l dataOrNew context declHead kind gadtDecl deriving0) = GDataDecl (id l) (dePatternDataOrNew dataOrNew) (fmap (dePatternContext) context) (dePatternDeclHead declHead) (fmap (dePatternKind) kind) (fmap (dePatternGadtDecl) gadtDecl) (fmap (dePatternDeriving) deriving0)
dePatternDecl (DataFamDecl l context declHead kind) = DataFamDecl (id l) (fmap (dePatternContext) context) (dePatternDeclHead declHead) (fmap (dePatternKind) kind)
dePatternDecl (TypeInsDecl l type1 type2) = TypeInsDecl (id l) (dePatternType type1) (dePatternType type2)
dePatternDecl (DataInsDecl l dataOrNew type0 qualConDecl deriving0) = DataInsDecl (id l) (dePatternDataOrNew dataOrNew) (dePatternType type0) (fmap (dePatternQualConDecl) qualConDecl) (fmap (dePatternDeriving) deriving0)
dePatternDecl (GDataInsDecl l dataOrNew type0 kind gadtDecl deriving0) = GDataInsDecl (id l) (dePatternDataOrNew dataOrNew) (dePatternType type0) (fmap (dePatternKind) kind) (fmap (dePatternGadtDecl) gadtDecl) (fmap (dePatternDeriving) deriving0)
dePatternDecl (ClassDecl l context declHead funDep classDecl) = ClassDecl (id l) (fmap (dePatternContext) context) (dePatternDeclHead declHead) (fmap (dePatternFunDep) funDep) (fmap (fmap (dePatternClassDecl)) classDecl)
dePatternDecl (InstDecl l overlap instRule instDecl) = InstDecl (id l) (fmap (dePatternOverlap) overlap) (dePatternInstRule instRule) (fmap (fmap (dePatternInstDecl)) instDecl)
dePatternDecl (DerivDecl l overlap instRule) = DerivDecl (id l) (fmap (dePatternOverlap) overlap) (dePatternInstRule instRule)
dePatternDecl (InfixDecl l assoc int op) = InfixDecl (id l) (dePatternAssoc assoc) (fmap (id) int) (fmap (dePatternOp) op)
dePatternDecl (DefaultDecl l type0) = DefaultDecl (id l) (fmap (dePatternType) type0)
dePatternDecl (SpliceDecl l exp) = SpliceDecl (id l) (dePatternExp exp)
dePatternDecl (TypeSig l name type0) = TypeSig (id l) (fmap (dePatternName) name) (dePatternType type0)
dePatternDecl (PatSynSig l name tyVarBind context1 context2 type0) = PatSynSig (id l) (dePatternName name) (fmap (fmap (dePatternTyVarBind)) tyVarBind) (fmap (dePatternContext) context1) (fmap (dePatternContext) context2) (dePatternType type0)
dePatternDecl (FunBind l match) = FunBind (id l) (fmap (dePatternMatch) match)
dePatternDecl (PatBind l pat rhs binds) = PatBind (id l) (dePatternPat pat) (dePatternRhs rhs) (fmap (dePatternBinds) binds)
dePatternDecl (PatSyn l pat1 pat2 patternSynDirection) = PatSyn (id l) (dePatternPat pat1) (dePatternPat pat2) (dePatternPatternSynDirection patternSynDirection)
dePatternDecl (ForImp l callConv safety string name type0) = ForImp (id l) (dePatternCallConv callConv) (fmap (dePatternSafety) safety) (fmap (id) string) (dePatternName name) (dePatternType type0)
dePatternDecl (ForExp l callConv string name type0) = ForExp (id l) (dePatternCallConv callConv) (fmap (id) string) (dePatternName name) (dePatternType type0)
dePatternDecl (RulePragmaDecl l rule) = RulePragmaDecl (id l) (fmap (dePatternRule) rule)
dePatternDecl (DeprPragmaDecl l name) = DeprPragmaDecl (id l) (fmap (((fmap (dePatternName)) *** (id))) name)
dePatternDecl (WarnPragmaDecl l name) = WarnPragmaDecl (id l) (fmap (((fmap (dePatternName)) *** (id))) name)
dePatternDecl (InlineSig l bool activation qName) = InlineSig (id l) (id bool) (fmap (dePatternActivation) activation) (dePatternQName qName)
dePatternDecl (InlineConlikeSig l activation qName) = InlineConlikeSig (id l) (fmap (dePatternActivation) activation) (dePatternQName qName)
dePatternDecl (SpecSig l activation qName type0) = SpecSig (id l) (fmap (dePatternActivation) activation) (dePatternQName qName) (fmap (dePatternType) type0)
dePatternDecl (SpecInlineSig l bool activation qName type0) = SpecInlineSig (id l) (id bool) (fmap (dePatternActivation) activation) (dePatternQName qName) (fmap (dePatternType) type0)
dePatternDecl (InstSig l instRule) = InstSig (id l) (dePatternInstRule instRule)
dePatternDecl (AnnPragma l annotation) = AnnPragma (id l) (dePatternAnnotation annotation)
dePatternDecl (MinimalPragma l booleanFormula) = MinimalPragma (id l) (fmap (dePatternBooleanFormula) booleanFormula)
dePatternDecl (RoleAnnotDecl l qName role) = RoleAnnotDecl (id l) (dePatternQName qName) (fmap (dePatternRole) role)
dePatternDeclHead :: DeclHead l -> DeclHead l
dePatternDeclHead (DHead l name) = DHead (id l) (dePatternName name)
dePatternDeclHead (DHInfix l tyVarBind name) = DHInfix (id l) (dePatternTyVarBind tyVarBind) (dePatternName name)
dePatternDeclHead (DHParen l declHead) = DHParen (id l) (dePatternDeclHead declHead)
dePatternDeclHead (DHApp l declHead tyVarBind) = DHApp (id l) (dePatternDeclHead declHead) (dePatternTyVarBind tyVarBind)
dePatternDeriving :: Deriving l -> Deriving l
dePatternDeriving (Deriving l instRule) = Deriving (id l) (fmap (dePatternInstRule) instRule)
dePatternExp :: Exp l -> Exp l
dePatternExp (Var l qName) = Var (id l) (dePatternQName qName)
dePatternExp (IPVar l iPName) = IPVar (id l) (dePatternIPName iPName)
dePatternExp (Con l qName) = Con (id l) (dePatternQName qName)
dePatternExp (Lit l literal) = Lit (id l) (dePatternLiteral literal)
dePatternExp (InfixApp l exp1 qOp exp2) = InfixApp (id l) (dePatternExp exp1) (dePatternQOp qOp) (dePatternExp exp2)
dePatternExp (App l exp1 exp2) = App (id l) (dePatternExp exp1) (dePatternExp exp2)
dePatternExp (NegApp l exp) = NegApp (id l) (dePatternExp exp)
dePatternExp (Lambda l pat exp) = Lambda (id l) (fmap (dePatternPat) pat) (dePatternExp exp)
dePatternExp (Let l binds exp) = Let (id l) (dePatternBinds binds) (dePatternExp exp)
dePatternExp (If l exp1 exp2 exp3) = If (id l) (dePatternExp exp1) (dePatternExp exp2) (dePatternExp exp3)
dePatternExp (MultiIf l guardedRhs) = MultiIf (id l) (fmap (dePatternGuardedRhs) guardedRhs)
dePatternExp (Case l exp alt) = Case (id l) (dePatternExp exp) (fmap (dePatternAlt) alt)
dePatternExp (Do l stmt) = Do (id l) (fmap (dePatternStmt) stmt)
dePatternExp (MDo l stmt) = MDo (id l) (fmap (dePatternStmt) stmt)
dePatternExp (Tuple l boxed exp) = Tuple (id l) (dePatternBoxed boxed) (fmap (dePatternExp) exp)
dePatternExp (TupleSection l boxed exp) = TupleSection (id l) (dePatternBoxed boxed) (fmap (fmap (dePatternExp)) exp)
dePatternExp (List l exp) = List (id l) (fmap (dePatternExp) exp)
dePatternExp (ParArray l exp) = ParArray (id l) (fmap (dePatternExp) exp)
dePatternExp (Paren l exp) = Paren (id l) (dePatternExp exp)
dePatternExp (LeftSection l exp qOp) = LeftSection (id l) (dePatternExp exp) (dePatternQOp qOp)
dePatternExp (RightSection l qOp exp) = RightSection (id l) (dePatternQOp qOp) (dePatternExp exp)
dePatternExp (RecConstr l qName fieldUpdate) = RecConstr (id l) (dePatternQName qName) (fmap (dePatternFieldUpdate) fieldUpdate)
dePatternExp (RecUpdate l exp fieldUpdate) = RecUpdate (id l) (dePatternExp exp) (fmap (dePatternFieldUpdate) fieldUpdate)
dePatternExp (EnumFrom l exp) = EnumFrom (id l) (dePatternExp exp)
dePatternExp (EnumFromTo l exp1 exp2) = EnumFromTo (id l) (dePatternExp exp1) (dePatternExp exp2)
dePatternExp (EnumFromThen l exp1 exp2) = EnumFromThen (id l) (dePatternExp exp1) (dePatternExp exp2)
dePatternExp (EnumFromThenTo l exp1 exp2 exp3) = EnumFromThenTo (id l) (dePatternExp exp1) (dePatternExp exp2) (dePatternExp exp3)
dePatternExp (ParArrayFromTo l exp1 exp2) = ParArrayFromTo (id l) (dePatternExp exp1) (dePatternExp exp2)
dePatternExp (ParArrayFromThenTo l exp1 exp2 exp3) = ParArrayFromThenTo (id l) (dePatternExp exp1) (dePatternExp exp2) (dePatternExp exp3)
dePatternExp (ListComp l exp qualStmt) = ListComp (id l) (dePatternExp exp) (fmap (dePatternQualStmt) qualStmt)
dePatternExp (ParComp l exp qualStmt) = ParComp (id l) (dePatternExp exp) (fmap (fmap (dePatternQualStmt)) qualStmt)
dePatternExp (ParArrayComp l exp qualStmt) = ParArrayComp (id l) (dePatternExp exp) (fmap (fmap (dePatternQualStmt)) qualStmt)
dePatternExp (ExpTypeSig l exp type0) = ExpTypeSig (id l) (dePatternExp exp) (dePatternType type0)
dePatternExp (VarQuote l qName) = VarQuote (id l) (dePatternQName qName)
dePatternExp (TypQuote l qName) = TypQuote (id l) (dePatternQName qName)
dePatternExp (BracketExp l bracket) = BracketExp (id l) (dePatternBracket bracket)
dePatternExp (SpliceExp l splice) = SpliceExp (id l) (dePatternSplice splice)
dePatternExp (QuasiQuote l string1 string2) = QuasiQuote (id l) (id string1) (id string2)
dePatternExp (XTag l xName xAttr exp1 exp2) = XTag (id l) (dePatternXName xName) (fmap (dePatternXAttr) xAttr) (fmap (dePatternExp) exp1) (fmap (dePatternExp) exp2)
dePatternExp (XETag l xName xAttr exp) = XETag (id l) (dePatternXName xName) (fmap (dePatternXAttr) xAttr) (fmap (dePatternExp) exp)
dePatternExp (XPcdata l string) = XPcdata (id l) (id string)
dePatternExp (XExpTag l exp) = XExpTag (id l) (dePatternExp exp)
dePatternExp (XChildTag l exp) = XChildTag (id l) (fmap (dePatternExp) exp)
dePatternExp (CorePragma l string exp) = CorePragma (id l) (id string) (dePatternExp exp)
dePatternExp (SCCPragma l string exp) = SCCPragma (id l) (id string) (dePatternExp exp)
dePatternExp (GenPragma l string int1 int2 exp) = GenPragma (id l) (id string) (((id) *** (id)) int1) (((id) *** (id)) int2) (dePatternExp exp)
dePatternExp (Proc l pat exp) = Proc (id l) (dePatternPat pat) (dePatternExp exp)
dePatternExp (LeftArrApp l exp1 exp2) = LeftArrApp (id l) (dePatternExp exp1) (dePatternExp exp2)
dePatternExp (RightArrApp l exp1 exp2) = RightArrApp (id l) (dePatternExp exp1) (dePatternExp exp2)
dePatternExp (LeftArrHighApp l exp1 exp2) = LeftArrHighApp (id l) (dePatternExp exp1) (dePatternExp exp2)
dePatternExp (RightArrHighApp l exp1 exp2) = RightArrHighApp (id l) (dePatternExp exp1) (dePatternExp exp2)
dePatternExp (LCase l alt) = LCase (id l) (fmap (dePatternAlt) alt)
dePatternExp (ExprHole l) = ExprHole (id l)
dePatternExportSpec :: ExportSpec l -> ExportSpec l
dePatternExportSpec (EVar l qName) = EVar (id l) (dePatternQName qName)
dePatternExportSpec (EAbs l namespace qName) = EAbs (id l) (dePatternNamespace namespace) (dePatternQName qName)
dePatternExportSpec (EThingAll l qName) = EThingAll (id l) (dePatternQName qName)
dePatternExportSpec (EThingWith l qName cName) = EThingWith (id l) (dePatternQName qName) (fmap (dePatternCName) cName)
dePatternExportSpec (EModuleContents l moduleName) = EModuleContents (id l) (dePatternModuleName moduleName)
dePatternExportSpecList :: ExportSpecList l -> ExportSpecList l
dePatternExportSpecList (ExportSpecList l exportSpec) = ExportSpecList (id l) (fmap (dePatternExportSpec) exportSpec)
dePatternFieldDecl :: FieldDecl l -> FieldDecl l
dePatternFieldDecl (FieldDecl l name type0) = FieldDecl (id l) (fmap (dePatternName) name) (dePatternType type0)
dePatternFieldUpdate :: FieldUpdate l -> FieldUpdate l
dePatternFieldUpdate (FieldUpdate l qName exp) = FieldUpdate (id l) (dePatternQName qName) (dePatternExp exp)
dePatternFieldUpdate (FieldPun l qName) = FieldPun (id l) (dePatternQName qName)
dePatternFieldUpdate (FieldWildcard l) = FieldWildcard (id l)
dePatternFunDep :: FunDep l -> FunDep l
dePatternFunDep (FunDep l name1 name2) = FunDep (id l) (fmap (dePatternName) name1) (fmap (dePatternName) name2)
dePatternGadtDecl :: GadtDecl l -> GadtDecl l
dePatternGadtDecl (GadtDecl l name fieldDecl type0) = GadtDecl (id l) (dePatternName name) (fmap (fmap (dePatternFieldDecl)) fieldDecl) (dePatternType type0)
dePatternGuardedRhs :: GuardedRhs l -> GuardedRhs l
dePatternGuardedRhs (GuardedRhs l stmt exp) = GuardedRhs (id l) (fmap (dePatternStmt) stmt) (dePatternExp exp)
dePatternIPBind :: IPBind l -> IPBind l
dePatternIPBind (IPBind l iPName exp) = IPBind (id l) (dePatternIPName iPName) (dePatternExp exp)
dePatternIPName :: IPName l -> IPName l
dePatternIPName (IPDup l string) = IPDup (id l) (id string)
dePatternIPName (IPLin l string) = IPLin (id l) (id string)
dePatternImportDecl :: ImportDecl l -> ImportDecl l
dePatternImportDecl (ImportDecl importAnn importModule importQualified importSrc importSafe importPkg importAs importSpecs) = ImportDecl (id importAnn) (dePatternModuleName importModule) (id importQualified) (id importSrc) (id importSafe) (fmap (id) importPkg) (fmap (dePatternModuleName) importAs) (fmap (dePatternImportSpecList) importSpecs)
dePatternImportSpec :: ImportSpec l -> ImportSpec l
dePatternImportSpec (IVar l name) = IVar (id l) (dePatternName name)
dePatternImportSpec (IAbs l namespace name) = IAbs (id l) (dePatternNamespace namespace) (dePatternName name)
dePatternImportSpec (IThingAll l name) = IThingAll (id l) (dePatternName name)
dePatternImportSpec (IThingWith l name cName) = IThingWith (id l) (dePatternName name) (fmap (dePatternCName) cName)
dePatternImportSpecList :: ImportSpecList l -> ImportSpecList l
dePatternImportSpecList (ImportSpecList l bool importSpec) = ImportSpecList (id l) (id bool) (fmap (dePatternImportSpec) importSpec)
dePatternInstDecl :: InstDecl l -> InstDecl l
dePatternInstDecl (InsDecl l decl) = InsDecl (id l) (dePatternDecl decl)
dePatternInstDecl (InsType l type1 type2) = InsType (id l) (dePatternType type1) (dePatternType type2)
dePatternInstDecl (InsData l dataOrNew type0 qualConDecl deriving0) = InsData (id l) (dePatternDataOrNew dataOrNew) (dePatternType type0) (fmap (dePatternQualConDecl) qualConDecl) (fmap (dePatternDeriving) deriving0)
dePatternInstDecl (InsGData l dataOrNew type0 kind gadtDecl deriving0) = InsGData (id l) (dePatternDataOrNew dataOrNew) (dePatternType type0) (fmap (dePatternKind) kind) (fmap (dePatternGadtDecl) gadtDecl) (fmap (dePatternDeriving) deriving0)
dePatternInstHead :: InstHead l -> InstHead l
dePatternInstHead (IHCon l qName) = IHCon (id l) (dePatternQName qName)
dePatternInstHead (IHInfix l type0 qName) = IHInfix (id l) (dePatternType type0) (dePatternQName qName)
dePatternInstHead (IHParen l instHead) = IHParen (id l) (dePatternInstHead instHead)
dePatternInstHead (IHApp l instHead type0) = IHApp (id l) (dePatternInstHead instHead) (dePatternType type0)
dePatternInstRule :: InstRule l -> InstRule l
dePatternInstRule (IRule l tyVarBind context instHead) = IRule (id l) (fmap (fmap (dePatternTyVarBind)) tyVarBind) (fmap (dePatternContext) context) (dePatternInstHead instHead)
dePatternInstRule (IParen l instRule) = IParen (id l) (dePatternInstRule instRule)
dePatternKind :: Kind l -> Kind l
dePatternKind (KindStar l) = KindStar (id l)
dePatternKind (KindFn l kind1 kind2) = KindFn (id l) (dePatternKind kind1) (dePatternKind kind2)
dePatternKind (KindParen l kind) = KindParen (id l) (dePatternKind kind)
dePatternKind (KindVar l qName) = KindVar (id l) (dePatternQName qName)
dePatternKind (KindApp l kind1 kind2) = KindApp (id l) (dePatternKind kind1) (dePatternKind kind2)
dePatternKind (KindTuple l kind) = KindTuple (id l) (fmap (dePatternKind) kind)
dePatternKind (KindList l kind) = KindList (id l) (dePatternKind kind)
dePatternLiteral :: Literal l -> Literal l
dePatternLiteral (Char l char string) = Char (id l) (id char) (id string)
dePatternLiteral (String l string1 string2) = String (id l) (id string1) (id string2)
dePatternLiteral (Int l integer string) = Int (id l) (id integer) (id string)
dePatternLiteral (Frac l rational string) = Frac (id l) (id rational) (id string)
dePatternLiteral (PrimInt l integer string) = PrimInt (id l) (id integer) (id string)
dePatternLiteral (PrimWord l integer string) = PrimWord (id l) (id integer) (id string)
dePatternLiteral (PrimFloat l rational string) = PrimFloat (id l) (id rational) (id string)
dePatternLiteral (PrimDouble l rational string) = PrimDouble (id l) (id rational) (id string)
dePatternLiteral (PrimChar l char string) = PrimChar (id l) (id char) (id string)
dePatternLiteral (PrimString l string1 string2) = PrimString (id l) (id string1) (id string2)
dePatternMatch :: Match l -> Match l
dePatternMatch (Match l name pat rhs binds) = Match (id l) (dePatternName name) (fmap (dePatternPat) pat) (dePatternRhs rhs) (fmap (dePatternBinds) binds)
dePatternMatch (InfixMatch l pat1 name pat2 rhs binds) = InfixMatch (id l) (dePatternPat pat1) (dePatternName name) (fmap (dePatternPat) pat2) (dePatternRhs rhs) (fmap (dePatternBinds) binds)
dePatternModule :: Module l -> Module l
dePatternModule (Module l moduleHead modulePragma importDecl decl) = Module (id l) (fmap (dePatternModuleHead) moduleHead) (fmap (dePatternModulePragma) modulePragma) (fmap (dePatternImportDecl) importDecl) (fmap (dePatternDecl) decl)
dePatternModule (XmlPage l moduleName modulePragma xName xAttr exp1 exp2) = XmlPage (id l) (dePatternModuleName moduleName) (fmap (dePatternModulePragma) modulePragma) (dePatternXName xName) (fmap (dePatternXAttr) xAttr) (fmap (dePatternExp) exp1) (fmap (dePatternExp) exp2)
dePatternModule (XmlHybrid l moduleHead modulePragma importDecl decl xName xAttr exp1 exp2) = XmlHybrid (id l) (fmap (dePatternModuleHead) moduleHead) (fmap (dePatternModulePragma) modulePragma) (fmap (dePatternImportDecl) importDecl) (fmap (dePatternDecl) decl) (dePatternXName xName) (fmap (dePatternXAttr) xAttr) (fmap (dePatternExp) exp1) (fmap (dePatternExp) exp2)
dePatternModuleHead :: ModuleHead l -> ModuleHead l
dePatternModuleHead (ModuleHead l moduleName warningText exportSpecList) = ModuleHead (id l) (dePatternModuleName moduleName) (fmap (dePatternWarningText) warningText) (fmap (dePatternExportSpecList) exportSpecList)
dePatternModuleName :: ModuleName l -> ModuleName l
dePatternModuleName (ModuleName l string) = ModuleName (id l) (id string)
dePatternModulePragma :: ModulePragma l -> ModulePragma l
dePatternModulePragma (LanguagePragma l name) = LanguagePragma (id l) (fmap (dePatternName) name)
dePatternModulePragma (OptionsPragma l tool string) = OptionsPragma (id l) (fmap (dePatternTool) tool) (id string)
dePatternModulePragma (AnnModulePragma l annotation) = AnnModulePragma (id l) (dePatternAnnotation annotation)
dePatternName :: Name l -> Name l
dePatternName (Ident l string) = Ident (id l) (id string)
dePatternName (Symbol l string) = Symbol (id l) (id string)
dePatternNamespace :: Namespace l -> Namespace l
dePatternNamespace (NoNamespace l) = NoNamespace (id l)
dePatternNamespace (TypeNamespace l) = TypeNamespace (id l)
dePatternNamespace (PatternNamespace l) = PatternNamespace (id l)
dePatternOp :: Op l -> Op l
dePatternOp (VarOp l name) = VarOp (id l) (dePatternName name)
dePatternOp (ConOp l name) = ConOp (id l) (dePatternName name)
dePatternOverlap :: Overlap l -> Overlap l
dePatternOverlap (NoOverlap l) = NoOverlap (id l)
dePatternOverlap (Overlap l) = Overlap (id l)
dePatternOverlap (Incoherent l) = Incoherent (id l)
dePatternPXAttr :: PXAttr l -> PXAttr l
dePatternPXAttr (PXAttr l xName pat) = PXAttr (id l) (dePatternXName xName) (dePatternPat pat)
dePatternPat :: Pat l -> Pat l
dePatternPat (PVar l name) = PVar (id l) (dePatternName name)
dePatternPat (PLit l sign literal) = PLit (id l) (dePatternSign sign) (dePatternLiteral literal)
dePatternPat (PNPlusK l name integer) = PNPlusK (id l) (dePatternName name) (id integer)
dePatternPat (PInfixApp l pat1 qName pat2) = PInfixApp (id l) (dePatternPat pat1) (dePatternQName qName) (dePatternPat pat2)
dePatternPat (PApp l qName pat) = PApp (id l) (dePatternQName qName) (fmap (dePatternPat) pat)
dePatternPat (PTuple l boxed pat) =
  dePatternPat (PApp l (Special l (TupleCon l boxed (length pat))) pat)
dePatternPat (PList l pat) = dePatternPat (eatList pat) where
  eatList [] = PApp l (Special l (ListCon l)) []
  eatList (p:ps) = PApp l (Special l (Cons l)) [p, eatList ps]
dePatternPat (PParen l pat) = PParen (id l) (dePatternPat pat)
dePatternPat (PRec l qName patField) = PRec (id l) (dePatternQName qName) (fmap (dePatternPatField) patField)
dePatternPat (PAsPat l name pat) = PAsPat (id l) (dePatternName name) (dePatternPat pat)
dePatternPat (PWildCard l) = PWildCard (id l)
dePatternPat (PIrrPat l pat) = PIrrPat (id l) (dePatternPat pat)
dePatternPat (PatTypeSig l pat type0) = PatTypeSig (id l) (dePatternPat pat) (dePatternType type0)
dePatternPat (PViewPat l exp pat) = PViewPat (id l) (dePatternExp exp) (dePatternPat pat)
dePatternPat (PRPat l rPat) = PRPat (id l) (fmap (dePatternRPat) rPat)
dePatternPat (PXTag l xName pXAttr pat1 pat2) = PXTag (id l) (dePatternXName xName) (fmap (dePatternPXAttr) pXAttr) (fmap (dePatternPat) pat1) (fmap (dePatternPat) pat2)
dePatternPat (PXETag l xName pXAttr pat) = PXETag (id l) (dePatternXName xName) (fmap (dePatternPXAttr) pXAttr) (fmap (dePatternPat) pat)
dePatternPat (PXPcdata l string) = PXPcdata (id l) (id string)
dePatternPat (PXPatTag l pat) = PXPatTag (id l) (dePatternPat pat)
dePatternPat (PXRPats l rPat) = PXRPats (id l) (fmap (dePatternRPat) rPat)
dePatternPat (PQuasiQuote l string1 string2) = PQuasiQuote (id l) (id string1) (id string2)
dePatternPat (PBangPat l pat) = PBangPat (id l) (dePatternPat pat)
dePatternPatField :: PatField l -> PatField l
dePatternPatField (PFieldPat l qName pat) = PFieldPat (id l) (dePatternQName qName) (dePatternPat pat)
dePatternPatField (PFieldPun l qName) = PFieldPun (id l) (dePatternQName qName)
dePatternPatField (PFieldWildcard l) = PFieldWildcard (id l)
dePatternPatternSynDirection :: PatternSynDirection l -> PatternSynDirection l
dePatternPatternSynDirection (Unidirectional) = Unidirectional
dePatternPatternSynDirection (ImplicitBidirectional) = ImplicitBidirectional
dePatternPatternSynDirection (ExplicitBidirectional l decl) = ExplicitBidirectional (id l) (fmap (dePatternDecl) decl)
dePatternPromoted :: Promoted l -> Promoted l
dePatternPromoted (PromotedInteger l integer string) = PromotedInteger (id l) (id integer) (id string)
dePatternPromoted (PromotedString l string1 string2) = PromotedString (id l) (id string1) (id string2)
dePatternPromoted (PromotedCon l bool qName) = PromotedCon (id l) (id bool) (dePatternQName qName)
dePatternPromoted (PromotedList l bool type0) = PromotedList (id l) (id bool) (fmap (dePatternType) type0)
dePatternPromoted (PromotedTuple l type0) = PromotedTuple (id l) (fmap (dePatternType) type0)
dePatternPromoted (PromotedUnit l) = PromotedUnit (id l)
dePatternQName :: QName l -> QName l
dePatternQName (Qual l moduleName name) = Qual (id l) (dePatternModuleName moduleName) (dePatternName name)
dePatternQName (UnQual l name) = UnQual (id l) (dePatternName name)
dePatternQName (Special l specialCon) = Special (id l) (dePatternSpecialCon specialCon)
dePatternQOp :: QOp l -> QOp l
dePatternQOp (QVarOp l qName) = QVarOp (id l) (dePatternQName qName)
dePatternQOp (QConOp l qName) = QConOp (id l) (dePatternQName qName)
dePatternQualConDecl :: QualConDecl l -> QualConDecl l
dePatternQualConDecl (QualConDecl l tyVarBind context conDecl) = QualConDecl (id l) (fmap (fmap (dePatternTyVarBind)) tyVarBind) (fmap (dePatternContext) context) (dePatternConDecl conDecl)
dePatternQualStmt :: QualStmt l -> QualStmt l
dePatternQualStmt (QualStmt l stmt) = QualStmt (id l) (dePatternStmt stmt)
dePatternQualStmt (ThenTrans l exp) = ThenTrans (id l) (dePatternExp exp)
dePatternQualStmt (ThenBy l exp1 exp2) = ThenBy (id l) (dePatternExp exp1) (dePatternExp exp2)
dePatternQualStmt (GroupBy l exp) = GroupBy (id l) (dePatternExp exp)
dePatternQualStmt (GroupUsing l exp) = GroupUsing (id l) (dePatternExp exp)
dePatternQualStmt (GroupByUsing l exp1 exp2) = GroupByUsing (id l) (dePatternExp exp1) (dePatternExp exp2)
dePatternRPat :: RPat l -> RPat l
dePatternRPat (RPOp l rPat rPatOp) = RPOp (id l) (dePatternRPat rPat) (dePatternRPatOp rPatOp)
dePatternRPat (RPEither l rPat1 rPat2) = RPEither (id l) (dePatternRPat rPat1) (dePatternRPat rPat2)
dePatternRPat (RPSeq l rPat) = RPSeq (id l) (fmap (dePatternRPat) rPat)
dePatternRPat (RPGuard l pat stmt) = RPGuard (id l) (dePatternPat pat) (fmap (dePatternStmt) stmt)
dePatternRPat (RPCAs l name rPat) = RPCAs (id l) (dePatternName name) (dePatternRPat rPat)
dePatternRPat (RPAs l name rPat) = RPAs (id l) (dePatternName name) (dePatternRPat rPat)
dePatternRPat (RPParen l rPat) = RPParen (id l) (dePatternRPat rPat)
dePatternRPat (RPPat l pat) = RPPat (id l) (dePatternPat pat)
dePatternRPatOp :: RPatOp l -> RPatOp l
dePatternRPatOp (RPStar l) = RPStar (id l)
dePatternRPatOp (RPStarG l) = RPStarG (id l)
dePatternRPatOp (RPPlus l) = RPPlus (id l)
dePatternRPatOp (RPPlusG l) = RPPlusG (id l)
dePatternRPatOp (RPOpt l) = RPOpt (id l)
dePatternRPatOp (RPOptG l) = RPOptG (id l)
dePatternRhs :: Rhs l -> Rhs l
dePatternRhs (UnGuardedRhs l exp) = UnGuardedRhs (id l) (dePatternExp exp)
dePatternRhs (GuardedRhss l guardedRhs) = GuardedRhss (id l) (fmap (dePatternGuardedRhs) guardedRhs)
dePatternRole :: Role l -> Role l
dePatternRole (Nominal l) = Nominal (id l)
dePatternRole (Representational l) = Representational (id l)
dePatternRole (Phantom l) = Phantom (id l)
dePatternRole (RoleWildcard l) = RoleWildcard (id l)
dePatternRule :: Rule l -> Rule l
dePatternRule (Rule l string activation ruleVar exp1 exp2) = Rule (id l) (id string) (fmap (dePatternActivation) activation) (fmap (fmap (dePatternRuleVar)) ruleVar) (dePatternExp exp1) (dePatternExp exp2)
dePatternRuleVar :: RuleVar l -> RuleVar l
dePatternRuleVar (RuleVar l name) = RuleVar (id l) (dePatternName name)
dePatternRuleVar (TypedRuleVar l name type0) = TypedRuleVar (id l) (dePatternName name) (dePatternType type0)
dePatternSafety :: Safety l -> Safety l
dePatternSafety (PlayRisky l) = PlayRisky (id l)
dePatternSafety (PlaySafe l bool) = PlaySafe (id l) (id bool)
dePatternSafety (PlayInterruptible l) = PlayInterruptible (id l)
dePatternSign :: Sign l -> Sign l
dePatternSign (Signless l) = Signless (id l)
dePatternSign (Negative l) = Negative (id l)
dePatternSpecialCon :: SpecialCon l -> SpecialCon l
dePatternSpecialCon (UnitCon l) = UnitCon (id l)
dePatternSpecialCon (ListCon l) = ListCon (id l)
dePatternSpecialCon (FunCon l) = FunCon (id l)
dePatternSpecialCon (TupleCon l boxed int) = TupleCon (id l) (dePatternBoxed boxed) (id int)
dePatternSpecialCon (Cons l) = Cons (id l)
dePatternSpecialCon (UnboxedSingleCon l) = UnboxedSingleCon (id l)
dePatternSplice :: Splice l -> Splice l
dePatternSplice (IdSplice l string) = IdSplice (id l) (id string)
dePatternSplice (ParenSplice l exp) = ParenSplice (id l) (dePatternExp exp)
dePatternStmt :: Stmt l -> Stmt l
dePatternStmt (Generator l pat exp) = Generator (id l) (dePatternPat pat) (dePatternExp exp)
dePatternStmt (Qualifier l exp) = Qualifier (id l) (dePatternExp exp)
dePatternStmt (LetStmt l binds) = LetStmt (id l) (dePatternBinds binds)
dePatternStmt (RecStmt l stmt) = RecStmt (id l) (fmap (dePatternStmt) stmt)
dePatternTool :: Tool -> Tool
dePatternTool (GHC) = GHC
dePatternTool (HUGS) = HUGS
dePatternTool (NHC98) = NHC98
dePatternTool (YHC) = YHC
dePatternTool (HADDOCK) = HADDOCK
dePatternTool (UnknownTool string) = UnknownTool (id string)
dePatternTyVarBind :: TyVarBind l -> TyVarBind l
dePatternTyVarBind (KindedVar l name kind) = KindedVar (id l) (dePatternName name) (dePatternKind kind)
dePatternTyVarBind (UnkindedVar l name) = UnkindedVar (id l) (dePatternName name)
dePatternType :: Type l -> Type l
dePatternType (TyForall l tyVarBind context type0) = TyForall (id l) (fmap (fmap (dePatternTyVarBind)) tyVarBind) (fmap (dePatternContext) context) (dePatternType type0)
dePatternType (TyFun l type1 type2) = TyFun (id l) (dePatternType type1) (dePatternType type2)
dePatternType (TyTuple l boxed type0) = TyTuple (id l) (dePatternBoxed boxed) (fmap (dePatternType) type0)
dePatternType (TyList l type0) = TyList (id l) (dePatternType type0)
dePatternType (TyParArray l type0) = TyParArray (id l) (dePatternType type0)
dePatternType (TyApp l type1 type2) = TyApp (id l) (dePatternType type1) (dePatternType type2)
dePatternType (TyVar l name) = TyVar (id l) (dePatternName name)
dePatternType (TyCon l qName) = TyCon (id l) (dePatternQName qName)
dePatternType (TyParen l type0) = TyParen (id l) (dePatternType type0)
dePatternType (TyInfix l type1 qName type2) = TyInfix (id l) (dePatternType type1) (dePatternQName qName) (dePatternType type2)
dePatternType (TyKind l type0 kind) = TyKind (id l) (dePatternType type0) (dePatternKind kind)
dePatternType (TyPromoted l promoted) = TyPromoted (id l) (dePatternPromoted promoted)
dePatternType (TyEquals l type1 type2) = TyEquals (id l) (dePatternType type1) (dePatternType type2)
dePatternType (TySplice l splice) = TySplice (id l) (dePatternSplice splice)
dePatternType (TyBang l bangType type0) = TyBang (id l) (dePatternBangType bangType) (dePatternType type0)
dePatternType (TyWildCard l name) = TyWildCard (id l) (fmap (dePatternName) name)
dePatternTypeEqn :: TypeEqn l -> TypeEqn l
dePatternTypeEqn (TypeEqn l type1 type2) = TypeEqn (id l) (dePatternType type1) (dePatternType type2)
dePatternWarningText :: WarningText l -> WarningText l
dePatternWarningText (DeprText l string) = DeprText (id l) (id string)
dePatternWarningText (WarnText l string) = WarnText (id l) (id string)
dePatternXAttr :: XAttr l -> XAttr l
dePatternXAttr (XAttr l xName exp) = XAttr (id l) (dePatternXName xName) (dePatternExp exp)
dePatternXName :: XName l -> XName l
dePatternXName (XName l string) = XName (id l) (id string)
dePatternXName (XDomName l string1 string2) = XDomName (id l) (id string1) (id string2)

