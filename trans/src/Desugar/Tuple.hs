module Desugar.Tuple where
import Language.Haskell.Exts.Annotated.Syntax
import Control.Arrow ((***))
deTupleActivation :: Activation l -> Activation l
deTupleActivation (ActiveFrom l int) = ActiveFrom (id l) (id int)
deTupleActivation (ActiveUntil l int) = ActiveUntil (id l) (id int)
deTupleAlt :: Alt l -> Alt l
deTupleAlt (Alt l pat rhs binds) = Alt (id l) (deTuplePat pat) (deTupleRhs rhs) (fmap (deTupleBinds) binds)
deTupleAnnotation :: Annotation l -> Annotation l
deTupleAnnotation (Ann l name exp) = Ann (id l) (deTupleName name) (deTupleExp exp)
deTupleAnnotation (TypeAnn l name exp) = TypeAnn (id l) (deTupleName name) (deTupleExp exp)
deTupleAnnotation (ModuleAnn l exp) = ModuleAnn (id l) (deTupleExp exp)
deTupleAssoc :: Assoc l -> Assoc l
deTupleAssoc (AssocNone l) = AssocNone (id l)
deTupleAssoc (AssocLeft l) = AssocLeft (id l)
deTupleAssoc (AssocRight l) = AssocRight (id l)
deTupleAsst :: Asst l -> Asst l
deTupleAsst (ClassA l qName type0) = ClassA (id l) (deTupleQName qName) (fmap (deTupleType) type0)
deTupleAsst (AppA l name type0) = AppA (id l) (deTupleName name) (fmap (deTupleType) type0)
deTupleAsst (InfixA l type1 qName type2) = InfixA (id l) (deTupleType type1) (deTupleQName qName) (deTupleType type2)
deTupleAsst (IParam l iPName type0) = IParam (id l) (deTupleIPName iPName) (deTupleType type0)
deTupleAsst (EqualP l type1 type2) = EqualP (id l) (deTupleType type1) (deTupleType type2)
deTupleAsst (ParenA l asst) = ParenA (id l) (deTupleAsst asst)
deTupleAsst (WildCardA l name) = WildCardA (id l) (fmap (deTupleName) name)
deTupleBangType :: BangType l -> BangType l
deTupleBangType (BangedTy l) = BangedTy (id l)
deTupleBangType (UnpackedTy l) = UnpackedTy (id l)
deTupleBinds :: Binds l -> Binds l
deTupleBinds (BDecls l decl) = BDecls (id l) (fmap (deTupleDecl) decl)
deTupleBinds (IPBinds l iPBind) = IPBinds (id l) (fmap (deTupleIPBind) iPBind)
deTupleBooleanFormula :: BooleanFormula l -> BooleanFormula l
deTupleBooleanFormula (VarFormula l name) = VarFormula (id l) (deTupleName name)
deTupleBooleanFormula (AndFormula l booleanFormula) = AndFormula (id l) (fmap (deTupleBooleanFormula) booleanFormula)
deTupleBooleanFormula (OrFormula l booleanFormula) = OrFormula (id l) (fmap (deTupleBooleanFormula) booleanFormula)
deTupleBooleanFormula (ParenFormula l booleanFormula) = ParenFormula (id l) (deTupleBooleanFormula booleanFormula)
deTupleBoxed :: Boxed -> Boxed
deTupleBoxed (Boxed) = Boxed
deTupleBoxed (Unboxed) = Unboxed
deTupleBracket :: Bracket l -> Bracket l
deTupleBracket (ExpBracket l exp) = ExpBracket (id l) (deTupleExp exp)
deTupleBracket (PatBracket l pat) = PatBracket (id l) (deTuplePat pat)
deTupleBracket (TypeBracket l type0) = TypeBracket (id l) (deTupleType type0)
deTupleBracket (DeclBracket l decl) = DeclBracket (id l) (fmap (deTupleDecl) decl)
deTupleCName :: CName l -> CName l
deTupleCName (VarName l name) = VarName (id l) (deTupleName name)
deTupleCName (ConName l name) = ConName (id l) (deTupleName name)
deTupleCallConv :: CallConv l -> CallConv l
deTupleCallConv (StdCall l) = StdCall (id l)
deTupleCallConv (CCall l) = CCall (id l)
deTupleCallConv (CPlusPlus l) = CPlusPlus (id l)
deTupleCallConv (DotNet l) = DotNet (id l)
deTupleCallConv (Jvm l) = Jvm (id l)
deTupleCallConv (Js l) = Js (id l)
deTupleCallConv (JavaScript l) = JavaScript (id l)
deTupleCallConv (CApi l) = CApi (id l)
deTupleClassDecl :: ClassDecl l -> ClassDecl l
deTupleClassDecl (ClsDecl l decl) = ClsDecl (id l) (deTupleDecl decl)
deTupleClassDecl (ClsDataFam l context declHead kind) = ClsDataFam (id l) (fmap (deTupleContext) context) (deTupleDeclHead declHead) (fmap (deTupleKind) kind)
deTupleClassDecl (ClsTyFam l declHead kind) = ClsTyFam (id l) (deTupleDeclHead declHead) (fmap (deTupleKind) kind)
deTupleClassDecl (ClsTyDef l type1 type2) = ClsTyDef (id l) (deTupleType type1) (deTupleType type2)
deTupleClassDecl (ClsDefSig l name type0) = ClsDefSig (id l) (deTupleName name) (deTupleType type0)
deTupleConDecl :: ConDecl l -> ConDecl l
deTupleConDecl (ConDecl l name type0) = ConDecl (id l) (deTupleName name) (fmap (deTupleType) type0)
deTupleConDecl (InfixConDecl l type1 name type2) = InfixConDecl (id l) (deTupleType type1) (deTupleName name) (deTupleType type2)
deTupleConDecl (RecDecl l name fieldDecl) = RecDecl (id l) (deTupleName name) (fmap (deTupleFieldDecl) fieldDecl)
deTupleContext :: Context l -> Context l
deTupleContext (CxSingle l asst) = CxSingle (id l) (deTupleAsst asst)
deTupleContext (CxTuple l asst) = CxTuple (id l) (fmap (deTupleAsst) asst)
deTupleContext (CxEmpty l) = CxEmpty (id l)
deTupleDataOrNew :: DataOrNew l -> DataOrNew l
deTupleDataOrNew (DataType l) = DataType (id l)
deTupleDataOrNew (NewType l) = NewType (id l)
deTupleDecl :: Decl l -> Decl l
deTupleDecl (TypeDecl l declHead type0) = TypeDecl (id l) (deTupleDeclHead declHead) (deTupleType type0)
deTupleDecl (TypeFamDecl l declHead kind) = TypeFamDecl (id l) (deTupleDeclHead declHead) (fmap (deTupleKind) kind)
deTupleDecl (ClosedTypeFamDecl l declHead kind typeEqn) = ClosedTypeFamDecl (id l) (deTupleDeclHead declHead) (fmap (deTupleKind) kind) (fmap (deTupleTypeEqn) typeEqn)
deTupleDecl (DataDecl l dataOrNew context declHead qualConDecl deriving0) = DataDecl (id l) (deTupleDataOrNew dataOrNew) (fmap (deTupleContext) context) (deTupleDeclHead declHead) (fmap (deTupleQualConDecl) qualConDecl) (fmap (deTupleDeriving) deriving0)
deTupleDecl (GDataDecl l dataOrNew context declHead kind gadtDecl deriving0) = GDataDecl (id l) (deTupleDataOrNew dataOrNew) (fmap (deTupleContext) context) (deTupleDeclHead declHead) (fmap (deTupleKind) kind) (fmap (deTupleGadtDecl) gadtDecl) (fmap (deTupleDeriving) deriving0)
deTupleDecl (DataFamDecl l context declHead kind) = DataFamDecl (id l) (fmap (deTupleContext) context) (deTupleDeclHead declHead) (fmap (deTupleKind) kind)
deTupleDecl (TypeInsDecl l type1 type2) = TypeInsDecl (id l) (deTupleType type1) (deTupleType type2)
deTupleDecl (DataInsDecl l dataOrNew type0 qualConDecl deriving0) = DataInsDecl (id l) (deTupleDataOrNew dataOrNew) (deTupleType type0) (fmap (deTupleQualConDecl) qualConDecl) (fmap (deTupleDeriving) deriving0)
deTupleDecl (GDataInsDecl l dataOrNew type0 kind gadtDecl deriving0) = GDataInsDecl (id l) (deTupleDataOrNew dataOrNew) (deTupleType type0) (fmap (deTupleKind) kind) (fmap (deTupleGadtDecl) gadtDecl) (fmap (deTupleDeriving) deriving0)
deTupleDecl (ClassDecl l context declHead funDep classDecl) = ClassDecl (id l) (fmap (deTupleContext) context) (deTupleDeclHead declHead) (fmap (deTupleFunDep) funDep) (fmap (fmap (deTupleClassDecl)) classDecl)
deTupleDecl (InstDecl l overlap instRule instDecl) = InstDecl (id l) (fmap (deTupleOverlap) overlap) (deTupleInstRule instRule) (fmap (fmap (deTupleInstDecl)) instDecl)
deTupleDecl (DerivDecl l overlap instRule) = DerivDecl (id l) (fmap (deTupleOverlap) overlap) (deTupleInstRule instRule)
deTupleDecl (InfixDecl l assoc int op) = InfixDecl (id l) (deTupleAssoc assoc) (fmap (id) int) (fmap (deTupleOp) op)
deTupleDecl (DefaultDecl l type0) = DefaultDecl (id l) (fmap (deTupleType) type0)
deTupleDecl (SpliceDecl l exp) = SpliceDecl (id l) (deTupleExp exp)
deTupleDecl (TypeSig l name type0) = TypeSig (id l) (fmap (deTupleName) name) (deTupleType type0)
deTupleDecl (PatSynSig l name tyVarBind context1 context2 type0) = PatSynSig (id l) (deTupleName name) (fmap (fmap (deTupleTyVarBind)) tyVarBind) (fmap (deTupleContext) context1) (fmap (deTupleContext) context2) (deTupleType type0)
deTupleDecl (FunBind l match) = FunBind (id l) (fmap (deTupleMatch) match)
deTupleDecl (PatBind l pat rhs binds) = PatBind (id l) (deTuplePat pat) (deTupleRhs rhs) (fmap (deTupleBinds) binds)
deTupleDecl (PatSyn l pat1 pat2 patternSynDirection) = PatSyn (id l) (deTuplePat pat1) (deTuplePat pat2) (deTuplePatternSynDirection patternSynDirection)
deTupleDecl (ForImp l callConv safety string name type0) = ForImp (id l) (deTupleCallConv callConv) (fmap (deTupleSafety) safety) (fmap (id) string) (deTupleName name) (deTupleType type0)
deTupleDecl (ForExp l callConv string name type0) = ForExp (id l) (deTupleCallConv callConv) (fmap (id) string) (deTupleName name) (deTupleType type0)
deTupleDecl (RulePragmaDecl l rule) = RulePragmaDecl (id l) (fmap (deTupleRule) rule)
deTupleDecl (DeprPragmaDecl l name) = DeprPragmaDecl (id l) (fmap (((fmap (deTupleName)) *** (id))) name)
deTupleDecl (WarnPragmaDecl l name) = WarnPragmaDecl (id l) (fmap (((fmap (deTupleName)) *** (id))) name)
deTupleDecl (InlineSig l bool activation qName) = InlineSig (id l) (id bool) (fmap (deTupleActivation) activation) (deTupleQName qName)
deTupleDecl (InlineConlikeSig l activation qName) = InlineConlikeSig (id l) (fmap (deTupleActivation) activation) (deTupleQName qName)
deTupleDecl (SpecSig l activation qName type0) = SpecSig (id l) (fmap (deTupleActivation) activation) (deTupleQName qName) (fmap (deTupleType) type0)
deTupleDecl (SpecInlineSig l bool activation qName type0) = SpecInlineSig (id l) (id bool) (fmap (deTupleActivation) activation) (deTupleQName qName) (fmap (deTupleType) type0)
deTupleDecl (InstSig l instRule) = InstSig (id l) (deTupleInstRule instRule)
deTupleDecl (AnnPragma l annotation) = AnnPragma (id l) (deTupleAnnotation annotation)
deTupleDecl (MinimalPragma l booleanFormula) = MinimalPragma (id l) (fmap (deTupleBooleanFormula) booleanFormula)
deTupleDecl (RoleAnnotDecl l qName role) = RoleAnnotDecl (id l) (deTupleQName qName) (fmap (deTupleRole) role)
deTupleDeclHead :: DeclHead l -> DeclHead l
deTupleDeclHead (DHead l name) = DHead (id l) (deTupleName name)
deTupleDeclHead (DHInfix l tyVarBind name) = DHInfix (id l) (deTupleTyVarBind tyVarBind) (deTupleName name)
deTupleDeclHead (DHParen l declHead) = DHParen (id l) (deTupleDeclHead declHead)
deTupleDeclHead (DHApp l declHead tyVarBind) = DHApp (id l) (deTupleDeclHead declHead) (deTupleTyVarBind tyVarBind)
deTupleDeriving :: Deriving l -> Deriving l
deTupleDeriving (Deriving l instRule) = Deriving (id l) (fmap (deTupleInstRule) instRule)
deTupleExp :: Exp l -> Exp l
deTupleExp (Var l qName) = Var (id l) (deTupleQName qName)
deTupleExp (IPVar l iPName) = IPVar (id l) (deTupleIPName iPName)
deTupleExp (Con l qName) = Con (id l) (deTupleQName qName)
deTupleExp (Lit l literal) = Lit (id l) (deTupleLiteral literal)
deTupleExp (InfixApp l exp1 qOp exp2) = InfixApp (id l) (deTupleExp exp1) (deTupleQOp qOp) (deTupleExp exp2)
deTupleExp (App l exp1 exp2) = App (id l) (deTupleExp exp1) (deTupleExp exp2)
deTupleExp (NegApp l exp) = NegApp (id l) (deTupleExp exp)
deTupleExp (Lambda l pat exp) = Lambda (id l) (fmap (deTuplePat) pat) (deTupleExp exp)
deTupleExp (Let l binds exp) = Let (id l) (deTupleBinds binds) (deTupleExp exp)
deTupleExp (If l exp1 exp2 exp3) = If (id l) (deTupleExp exp1) (deTupleExp exp2) (deTupleExp exp3)
deTupleExp (MultiIf l guardedRhs) = MultiIf (id l) (fmap (deTupleGuardedRhs) guardedRhs)
deTupleExp (Case l exp alt) = Case (id l) (deTupleExp exp) (fmap (deTupleAlt) alt)
deTupleExp (Do l stmt) = Do (id l) (fmap (deTupleStmt) stmt)
deTupleExp (MDo l stmt) = MDo (id l) (fmap (deTupleStmt) stmt)
deTupleExp (Tuple l boxed exp) =
  deTupleExp (eat (Con l (Special l (TupleCon l boxed (length exp)))) exp)
  where
    eat acc [] = acc
    eat acc (e:es) = eat (App l acc e) es
deTupleExp (TupleSection l boxed exp) = TupleSection (id l) (deTupleBoxed boxed) (fmap (fmap (deTupleExp)) exp)
deTupleExp (List l exp) = List (id l) (fmap (deTupleExp) exp)
deTupleExp (ParArray l exp) = ParArray (id l) (fmap (deTupleExp) exp)
deTupleExp (Paren l exp) = Paren (id l) (deTupleExp exp)
deTupleExp (LeftSection l exp qOp) = LeftSection (id l) (deTupleExp exp) (deTupleQOp qOp)
deTupleExp (RightSection l qOp exp) = RightSection (id l) (deTupleQOp qOp) (deTupleExp exp)
deTupleExp (RecConstr l qName fieldUpdate) = RecConstr (id l) (deTupleQName qName) (fmap (deTupleFieldUpdate) fieldUpdate)
deTupleExp (RecUpdate l exp fieldUpdate) = RecUpdate (id l) (deTupleExp exp) (fmap (deTupleFieldUpdate) fieldUpdate)
deTupleExp (EnumFrom l exp) = EnumFrom (id l) (deTupleExp exp)
deTupleExp (EnumFromTo l exp1 exp2) = EnumFromTo (id l) (deTupleExp exp1) (deTupleExp exp2)
deTupleExp (EnumFromThen l exp1 exp2) = EnumFromThen (id l) (deTupleExp exp1) (deTupleExp exp2)
deTupleExp (EnumFromThenTo l exp1 exp2 exp3) = EnumFromThenTo (id l) (deTupleExp exp1) (deTupleExp exp2) (deTupleExp exp3)
deTupleExp (ParArrayFromTo l exp1 exp2) = ParArrayFromTo (id l) (deTupleExp exp1) (deTupleExp exp2)
deTupleExp (ParArrayFromThenTo l exp1 exp2 exp3) = ParArrayFromThenTo (id l) (deTupleExp exp1) (deTupleExp exp2) (deTupleExp exp3)
deTupleExp (ListComp l exp qualStmt) = ListComp (id l) (deTupleExp exp) (fmap (deTupleQualStmt) qualStmt)
deTupleExp (ParComp l exp qualStmt) = ParComp (id l) (deTupleExp exp) (fmap (fmap (deTupleQualStmt)) qualStmt)
deTupleExp (ParArrayComp l exp qualStmt) = ParArrayComp (id l) (deTupleExp exp) (fmap (fmap (deTupleQualStmt)) qualStmt)
deTupleExp (ExpTypeSig l exp type0) = ExpTypeSig (id l) (deTupleExp exp) (deTupleType type0)
deTupleExp (VarQuote l qName) = VarQuote (id l) (deTupleQName qName)
deTupleExp (TypQuote l qName) = TypQuote (id l) (deTupleQName qName)
deTupleExp (BracketExp l bracket) = BracketExp (id l) (deTupleBracket bracket)
deTupleExp (SpliceExp l splice) = SpliceExp (id l) (deTupleSplice splice)
deTupleExp (QuasiQuote l string1 string2) = QuasiQuote (id l) (id string1) (id string2)
deTupleExp (XTag l xName xAttr exp1 exp2) = XTag (id l) (deTupleXName xName) (fmap (deTupleXAttr) xAttr) (fmap (deTupleExp) exp1) (fmap (deTupleExp) exp2)
deTupleExp (XETag l xName xAttr exp) = XETag (id l) (deTupleXName xName) (fmap (deTupleXAttr) xAttr) (fmap (deTupleExp) exp)
deTupleExp (XPcdata l string) = XPcdata (id l) (id string)
deTupleExp (XExpTag l exp) = XExpTag (id l) (deTupleExp exp)
deTupleExp (XChildTag l exp) = XChildTag (id l) (fmap (deTupleExp) exp)
deTupleExp (CorePragma l string exp) = CorePragma (id l) (id string) (deTupleExp exp)
deTupleExp (SCCPragma l string exp) = SCCPragma (id l) (id string) (deTupleExp exp)
deTupleExp (GenPragma l string int1 int2 exp) = GenPragma (id l) (id string) (((id) *** (id)) int1) (((id) *** (id)) int2) (deTupleExp exp)
deTupleExp (Proc l pat exp) = Proc (id l) (deTuplePat pat) (deTupleExp exp)
deTupleExp (LeftArrApp l exp1 exp2) = LeftArrApp (id l) (deTupleExp exp1) (deTupleExp exp2)
deTupleExp (RightArrApp l exp1 exp2) = RightArrApp (id l) (deTupleExp exp1) (deTupleExp exp2)
deTupleExp (LeftArrHighApp l exp1 exp2) = LeftArrHighApp (id l) (deTupleExp exp1) (deTupleExp exp2)
deTupleExp (RightArrHighApp l exp1 exp2) = RightArrHighApp (id l) (deTupleExp exp1) (deTupleExp exp2)
deTupleExp (LCase l alt) = LCase (id l) (fmap (deTupleAlt) alt)
deTupleExp (ExprHole l) = ExprHole (id l)
deTupleExportSpec :: ExportSpec l -> ExportSpec l
deTupleExportSpec (EVar l qName) = EVar (id l) (deTupleQName qName)
deTupleExportSpec (EAbs l namespace qName) = EAbs (id l) (deTupleNamespace namespace) (deTupleQName qName)
deTupleExportSpec (EThingAll l qName) = EThingAll (id l) (deTupleQName qName)
deTupleExportSpec (EThingWith l qName cName) = EThingWith (id l) (deTupleQName qName) (fmap (deTupleCName) cName)
deTupleExportSpec (EModuleContents l moduleName) = EModuleContents (id l) (deTupleModuleName moduleName)
deTupleExportSpecList :: ExportSpecList l -> ExportSpecList l
deTupleExportSpecList (ExportSpecList l exportSpec) = ExportSpecList (id l) (fmap (deTupleExportSpec) exportSpec)
deTupleFieldDecl :: FieldDecl l -> FieldDecl l
deTupleFieldDecl (FieldDecl l name type0) = FieldDecl (id l) (fmap (deTupleName) name) (deTupleType type0)
deTupleFieldUpdate :: FieldUpdate l -> FieldUpdate l
deTupleFieldUpdate (FieldUpdate l qName exp) = FieldUpdate (id l) (deTupleQName qName) (deTupleExp exp)
deTupleFieldUpdate (FieldPun l qName) = FieldPun (id l) (deTupleQName qName)
deTupleFieldUpdate (FieldWildcard l) = FieldWildcard (id l)
deTupleFunDep :: FunDep l -> FunDep l
deTupleFunDep (FunDep l name1 name2) = FunDep (id l) (fmap (deTupleName) name1) (fmap (deTupleName) name2)
deTupleGadtDecl :: GadtDecl l -> GadtDecl l
deTupleGadtDecl (GadtDecl l name fieldDecl type0) = GadtDecl (id l) (deTupleName name) (fmap (fmap (deTupleFieldDecl)) fieldDecl) (deTupleType type0)
deTupleGuardedRhs :: GuardedRhs l -> GuardedRhs l
deTupleGuardedRhs (GuardedRhs l stmt exp) = GuardedRhs (id l) (fmap (deTupleStmt) stmt) (deTupleExp exp)
deTupleIPBind :: IPBind l -> IPBind l
deTupleIPBind (IPBind l iPName exp) = IPBind (id l) (deTupleIPName iPName) (deTupleExp exp)
deTupleIPName :: IPName l -> IPName l
deTupleIPName (IPDup l string) = IPDup (id l) (id string)
deTupleIPName (IPLin l string) = IPLin (id l) (id string)
deTupleImportDecl :: ImportDecl l -> ImportDecl l
deTupleImportDecl (ImportDecl importAnn importModule importQualified importSrc importSafe importPkg importAs importSpecs) = ImportDecl (id importAnn) (deTupleModuleName importModule) (id importQualified) (id importSrc) (id importSafe) (fmap (id) importPkg) (fmap (deTupleModuleName) importAs) (fmap (deTupleImportSpecList) importSpecs)
deTupleImportSpec :: ImportSpec l -> ImportSpec l
deTupleImportSpec (IVar l name) = IVar (id l) (deTupleName name)
deTupleImportSpec (IAbs l namespace name) = IAbs (id l) (deTupleNamespace namespace) (deTupleName name)
deTupleImportSpec (IThingAll l name) = IThingAll (id l) (deTupleName name)
deTupleImportSpec (IThingWith l name cName) = IThingWith (id l) (deTupleName name) (fmap (deTupleCName) cName)
deTupleImportSpecList :: ImportSpecList l -> ImportSpecList l
deTupleImportSpecList (ImportSpecList l bool importSpec) = ImportSpecList (id l) (id bool) (fmap (deTupleImportSpec) importSpec)
deTupleInstDecl :: InstDecl l -> InstDecl l
deTupleInstDecl (InsDecl l decl) = InsDecl (id l) (deTupleDecl decl)
deTupleInstDecl (InsType l type1 type2) = InsType (id l) (deTupleType type1) (deTupleType type2)
deTupleInstDecl (InsData l dataOrNew type0 qualConDecl deriving0) = InsData (id l) (deTupleDataOrNew dataOrNew) (deTupleType type0) (fmap (deTupleQualConDecl) qualConDecl) (fmap (deTupleDeriving) deriving0)
deTupleInstDecl (InsGData l dataOrNew type0 kind gadtDecl deriving0) = InsGData (id l) (deTupleDataOrNew dataOrNew) (deTupleType type0) (fmap (deTupleKind) kind) (fmap (deTupleGadtDecl) gadtDecl) (fmap (deTupleDeriving) deriving0)
deTupleInstHead :: InstHead l -> InstHead l
deTupleInstHead (IHCon l qName) = IHCon (id l) (deTupleQName qName)
deTupleInstHead (IHInfix l type0 qName) = IHInfix (id l) (deTupleType type0) (deTupleQName qName)
deTupleInstHead (IHParen l instHead) = IHParen (id l) (deTupleInstHead instHead)
deTupleInstHead (IHApp l instHead type0) = IHApp (id l) (deTupleInstHead instHead) (deTupleType type0)
deTupleInstRule :: InstRule l -> InstRule l
deTupleInstRule (IRule l tyVarBind context instHead) = IRule (id l) (fmap (fmap (deTupleTyVarBind)) tyVarBind) (fmap (deTupleContext) context) (deTupleInstHead instHead)
deTupleInstRule (IParen l instRule) = IParen (id l) (deTupleInstRule instRule)
deTupleKind :: Kind l -> Kind l
deTupleKind (KindStar l) = KindStar (id l)
deTupleKind (KindFn l kind1 kind2) = KindFn (id l) (deTupleKind kind1) (deTupleKind kind2)
deTupleKind (KindParen l kind) = KindParen (id l) (deTupleKind kind)
deTupleKind (KindVar l qName) = KindVar (id l) (deTupleQName qName)
deTupleKind (KindApp l kind1 kind2) = KindApp (id l) (deTupleKind kind1) (deTupleKind kind2)
deTupleKind (KindTuple l kind) = KindTuple (id l) (fmap (deTupleKind) kind)
deTupleKind (KindList l kind) = KindList (id l) (deTupleKind kind)
deTupleLiteral :: Literal l -> Literal l
deTupleLiteral (Char l char string) = Char (id l) (id char) (id string)
deTupleLiteral (String l string1 string2) = String (id l) (id string1) (id string2)
deTupleLiteral (Int l integer string) = Int (id l) (id integer) (id string)
deTupleLiteral (Frac l rational string) = Frac (id l) (id rational) (id string)
deTupleLiteral (PrimInt l integer string) = PrimInt (id l) (id integer) (id string)
deTupleLiteral (PrimWord l integer string) = PrimWord (id l) (id integer) (id string)
deTupleLiteral (PrimFloat l rational string) = PrimFloat (id l) (id rational) (id string)
deTupleLiteral (PrimDouble l rational string) = PrimDouble (id l) (id rational) (id string)
deTupleLiteral (PrimChar l char string) = PrimChar (id l) (id char) (id string)
deTupleLiteral (PrimString l string1 string2) = PrimString (id l) (id string1) (id string2)
deTupleMatch :: Match l -> Match l
deTupleMatch (Match l name pat rhs binds) = Match (id l) (deTupleName name) (fmap (deTuplePat) pat) (deTupleRhs rhs) (fmap (deTupleBinds) binds)
deTupleMatch (InfixMatch l pat1 name pat2 rhs binds) = InfixMatch (id l) (deTuplePat pat1) (deTupleName name) (fmap (deTuplePat) pat2) (deTupleRhs rhs) (fmap (deTupleBinds) binds)
deTupleModule :: Module l -> Module l
deTupleModule (Module l moduleHead modulePragma importDecl decl) = Module (id l) (fmap (deTupleModuleHead) moduleHead) (fmap (deTupleModulePragma) modulePragma) (fmap (deTupleImportDecl) importDecl) (fmap (deTupleDecl) decl)
deTupleModule (XmlPage l moduleName modulePragma xName xAttr exp1 exp2) = XmlPage (id l) (deTupleModuleName moduleName) (fmap (deTupleModulePragma) modulePragma) (deTupleXName xName) (fmap (deTupleXAttr) xAttr) (fmap (deTupleExp) exp1) (fmap (deTupleExp) exp2)
deTupleModule (XmlHybrid l moduleHead modulePragma importDecl decl xName xAttr exp1 exp2) = XmlHybrid (id l) (fmap (deTupleModuleHead) moduleHead) (fmap (deTupleModulePragma) modulePragma) (fmap (deTupleImportDecl) importDecl) (fmap (deTupleDecl) decl) (deTupleXName xName) (fmap (deTupleXAttr) xAttr) (fmap (deTupleExp) exp1) (fmap (deTupleExp) exp2)
deTupleModuleHead :: ModuleHead l -> ModuleHead l
deTupleModuleHead (ModuleHead l moduleName warningText exportSpecList) = ModuleHead (id l) (deTupleModuleName moduleName) (fmap (deTupleWarningText) warningText) (fmap (deTupleExportSpecList) exportSpecList)
deTupleModuleName :: ModuleName l -> ModuleName l
deTupleModuleName (ModuleName l string) = ModuleName (id l) (id string)
deTupleModulePragma :: ModulePragma l -> ModulePragma l
deTupleModulePragma (LanguagePragma l name) = LanguagePragma (id l) (fmap (deTupleName) name)
deTupleModulePragma (OptionsPragma l tool string) = OptionsPragma (id l) (fmap (deTupleTool) tool) (id string)
deTupleModulePragma (AnnModulePragma l annotation) = AnnModulePragma (id l) (deTupleAnnotation annotation)
deTupleName :: Name l -> Name l
deTupleName (Ident l string) = Ident (id l) (id string)
deTupleName (Symbol l string) = Symbol (id l) (id string)
deTupleNamespace :: Namespace l -> Namespace l
deTupleNamespace (NoNamespace l) = NoNamespace (id l)
deTupleNamespace (TypeNamespace l) = TypeNamespace (id l)
deTupleNamespace (PatternNamespace l) = PatternNamespace (id l)
deTupleOp :: Op l -> Op l
deTupleOp (VarOp l name) = VarOp (id l) (deTupleName name)
deTupleOp (ConOp l name) = ConOp (id l) (deTupleName name)
deTupleOverlap :: Overlap l -> Overlap l
deTupleOverlap (NoOverlap l) = NoOverlap (id l)
deTupleOverlap (Overlap l) = Overlap (id l)
deTupleOverlap (Incoherent l) = Incoherent (id l)
deTuplePXAttr :: PXAttr l -> PXAttr l
deTuplePXAttr (PXAttr l xName pat) = PXAttr (id l) (deTupleXName xName) (deTuplePat pat)
deTuplePat :: Pat l -> Pat l
deTuplePat (PVar l name) = PVar (id l) (deTupleName name)
deTuplePat (PLit l sign literal) = PLit (id l) (deTupleSign sign) (deTupleLiteral literal)
deTuplePat (PNPlusK l name integer) = PNPlusK (id l) (deTupleName name) (id integer)
deTuplePat (PInfixApp l pat1 qName pat2) = PInfixApp (id l) (deTuplePat pat1) (deTupleQName qName) (deTuplePat pat2)
deTuplePat (PApp l qName pat) = PApp (id l) (deTupleQName qName) (fmap (deTuplePat) pat)
deTuplePat (PTuple l boxed pat) =
  deTuplePat (PApp l (Special l (TupleCon l boxed (length pat))) pat)
deTuplePat (PList l pat) = PList (id l) (fmap (deTuplePat) pat)
deTuplePat (PParen l pat) = PParen (id l) (deTuplePat pat)
deTuplePat (PRec l qName patField) = PRec (id l) (deTupleQName qName) (fmap (deTuplePatField) patField)
deTuplePat (PAsPat l name pat) = PAsPat (id l) (deTupleName name) (deTuplePat pat)
deTuplePat (PWildCard l) = PWildCard (id l)
deTuplePat (PIrrPat l pat) = PIrrPat (id l) (deTuplePat pat)
deTuplePat (PatTypeSig l pat type0) = PatTypeSig (id l) (deTuplePat pat) (deTupleType type0)
deTuplePat (PViewPat l exp pat) = PViewPat (id l) (deTupleExp exp) (deTuplePat pat)
deTuplePat (PRPat l rPat) = PRPat (id l) (fmap (deTupleRPat) rPat)
deTuplePat (PXTag l xName pXAttr pat1 pat2) = PXTag (id l) (deTupleXName xName) (fmap (deTuplePXAttr) pXAttr) (fmap (deTuplePat) pat1) (fmap (deTuplePat) pat2)
deTuplePat (PXETag l xName pXAttr pat) = PXETag (id l) (deTupleXName xName) (fmap (deTuplePXAttr) pXAttr) (fmap (deTuplePat) pat)
deTuplePat (PXPcdata l string) = PXPcdata (id l) (id string)
deTuplePat (PXPatTag l pat) = PXPatTag (id l) (deTuplePat pat)
deTuplePat (PXRPats l rPat) = PXRPats (id l) (fmap (deTupleRPat) rPat)
deTuplePat (PQuasiQuote l string1 string2) = PQuasiQuote (id l) (id string1) (id string2)
deTuplePat (PBangPat l pat) = PBangPat (id l) (deTuplePat pat)
deTuplePatField :: PatField l -> PatField l
deTuplePatField (PFieldPat l qName pat) = PFieldPat (id l) (deTupleQName qName) (deTuplePat pat)
deTuplePatField (PFieldPun l qName) = PFieldPun (id l) (deTupleQName qName)
deTuplePatField (PFieldWildcard l) = PFieldWildcard (id l)
deTuplePatternSynDirection :: PatternSynDirection l -> PatternSynDirection l
deTuplePatternSynDirection (Unidirectional) = Unidirectional
deTuplePatternSynDirection (ImplicitBidirectional) = ImplicitBidirectional
deTuplePatternSynDirection (ExplicitBidirectional l decl) = ExplicitBidirectional (id l) (fmap (deTupleDecl) decl)
deTuplePromoted :: Promoted l -> Promoted l
deTuplePromoted (PromotedInteger l integer string) = PromotedInteger (id l) (id integer) (id string)
deTuplePromoted (PromotedString l string1 string2) = PromotedString (id l) (id string1) (id string2)
deTuplePromoted (PromotedCon l bool qName) = PromotedCon (id l) (id bool) (deTupleQName qName)
deTuplePromoted (PromotedList l bool type0) = PromotedList (id l) (id bool) (fmap (deTupleType) type0)
deTuplePromoted (PromotedTuple l type0) = PromotedTuple (id l) (fmap (deTupleType) type0)
deTuplePromoted (PromotedUnit l) = PromotedUnit (id l)
deTupleQName :: QName l -> QName l
deTupleQName (Qual l moduleName name) = Qual (id l) (deTupleModuleName moduleName) (deTupleName name)
deTupleQName (UnQual l name) = UnQual (id l) (deTupleName name)
deTupleQName (Special l specialCon) = Special (id l) (deTupleSpecialCon specialCon)
deTupleQOp :: QOp l -> QOp l
deTupleQOp (QVarOp l qName) = QVarOp (id l) (deTupleQName qName)
deTupleQOp (QConOp l qName) = QConOp (id l) (deTupleQName qName)
deTupleQualConDecl :: QualConDecl l -> QualConDecl l
deTupleQualConDecl (QualConDecl l tyVarBind context conDecl) = QualConDecl (id l) (fmap (fmap (deTupleTyVarBind)) tyVarBind) (fmap (deTupleContext) context) (deTupleConDecl conDecl)
deTupleQualStmt :: QualStmt l -> QualStmt l
deTupleQualStmt (QualStmt l stmt) = QualStmt (id l) (deTupleStmt stmt)
deTupleQualStmt (ThenTrans l exp) = ThenTrans (id l) (deTupleExp exp)
deTupleQualStmt (ThenBy l exp1 exp2) = ThenBy (id l) (deTupleExp exp1) (deTupleExp exp2)
deTupleQualStmt (GroupBy l exp) = GroupBy (id l) (deTupleExp exp)
deTupleQualStmt (GroupUsing l exp) = GroupUsing (id l) (deTupleExp exp)
deTupleQualStmt (GroupByUsing l exp1 exp2) = GroupByUsing (id l) (deTupleExp exp1) (deTupleExp exp2)
deTupleRPat :: RPat l -> RPat l
deTupleRPat (RPOp l rPat rPatOp) = RPOp (id l) (deTupleRPat rPat) (deTupleRPatOp rPatOp)
deTupleRPat (RPEither l rPat1 rPat2) = RPEither (id l) (deTupleRPat rPat1) (deTupleRPat rPat2)
deTupleRPat (RPSeq l rPat) = RPSeq (id l) (fmap (deTupleRPat) rPat)
deTupleRPat (RPGuard l pat stmt) = RPGuard (id l) (deTuplePat pat) (fmap (deTupleStmt) stmt)
deTupleRPat (RPCAs l name rPat) = RPCAs (id l) (deTupleName name) (deTupleRPat rPat)
deTupleRPat (RPAs l name rPat) = RPAs (id l) (deTupleName name) (deTupleRPat rPat)
deTupleRPat (RPParen l rPat) = RPParen (id l) (deTupleRPat rPat)
deTupleRPat (RPPat l pat) = RPPat (id l) (deTuplePat pat)
deTupleRPatOp :: RPatOp l -> RPatOp l
deTupleRPatOp (RPStar l) = RPStar (id l)
deTupleRPatOp (RPStarG l) = RPStarG (id l)
deTupleRPatOp (RPPlus l) = RPPlus (id l)
deTupleRPatOp (RPPlusG l) = RPPlusG (id l)
deTupleRPatOp (RPOpt l) = RPOpt (id l)
deTupleRPatOp (RPOptG l) = RPOptG (id l)
deTupleRhs :: Rhs l -> Rhs l
deTupleRhs (UnGuardedRhs l exp) = UnGuardedRhs (id l) (deTupleExp exp)
deTupleRhs (GuardedRhss l guardedRhs) = GuardedRhss (id l) (fmap (deTupleGuardedRhs) guardedRhs)
deTupleRole :: Role l -> Role l
deTupleRole (Nominal l) = Nominal (id l)
deTupleRole (Representational l) = Representational (id l)
deTupleRole (Phantom l) = Phantom (id l)
deTupleRole (RoleWildcard l) = RoleWildcard (id l)
deTupleRule :: Rule l -> Rule l
deTupleRule (Rule l string activation ruleVar exp1 exp2) = Rule (id l) (id string) (fmap (deTupleActivation) activation) (fmap (fmap (deTupleRuleVar)) ruleVar) (deTupleExp exp1) (deTupleExp exp2)
deTupleRuleVar :: RuleVar l -> RuleVar l
deTupleRuleVar (RuleVar l name) = RuleVar (id l) (deTupleName name)
deTupleRuleVar (TypedRuleVar l name type0) = TypedRuleVar (id l) (deTupleName name) (deTupleType type0)
deTupleSafety :: Safety l -> Safety l
deTupleSafety (PlayRisky l) = PlayRisky (id l)
deTupleSafety (PlaySafe l bool) = PlaySafe (id l) (id bool)
deTupleSafety (PlayInterruptible l) = PlayInterruptible (id l)
deTupleSign :: Sign l -> Sign l
deTupleSign (Signless l) = Signless (id l)
deTupleSign (Negative l) = Negative (id l)
deTupleSpecialCon :: SpecialCon l -> SpecialCon l
deTupleSpecialCon (UnitCon l) = UnitCon (id l)
deTupleSpecialCon (ListCon l) = ListCon (id l)
deTupleSpecialCon (FunCon l) = FunCon (id l)
deTupleSpecialCon (TupleCon l boxed int) = TupleCon (id l) (deTupleBoxed boxed) (id int)
deTupleSpecialCon (Cons l) = Cons (id l)
deTupleSpecialCon (UnboxedSingleCon l) = UnboxedSingleCon (id l)
deTupleSplice :: Splice l -> Splice l
deTupleSplice (IdSplice l string) = IdSplice (id l) (id string)
deTupleSplice (ParenSplice l exp) = ParenSplice (id l) (deTupleExp exp)
deTupleStmt :: Stmt l -> Stmt l
deTupleStmt (Generator l pat exp) = Generator (id l) (deTuplePat pat) (deTupleExp exp)
deTupleStmt (Qualifier l exp) = Qualifier (id l) (deTupleExp exp)
deTupleStmt (LetStmt l binds) = LetStmt (id l) (deTupleBinds binds)
deTupleStmt (RecStmt l stmt) = RecStmt (id l) (fmap (deTupleStmt) stmt)
deTupleTool :: Tool -> Tool
deTupleTool (GHC) = GHC
deTupleTool (HUGS) = HUGS
deTupleTool (NHC98) = NHC98
deTupleTool (YHC) = YHC
deTupleTool (HADDOCK) = HADDOCK
deTupleTool (UnknownTool string) = UnknownTool (id string)
deTupleTyVarBind :: TyVarBind l -> TyVarBind l
deTupleTyVarBind (KindedVar l name kind) = KindedVar (id l) (deTupleName name) (deTupleKind kind)
deTupleTyVarBind (UnkindedVar l name) = UnkindedVar (id l) (deTupleName name)
deTupleType :: Type l -> Type l
deTupleType (TyForall l tyVarBind context type0) = TyForall (id l) (fmap (fmap (deTupleTyVarBind)) tyVarBind) (fmap (deTupleContext) context) (deTupleType type0)
deTupleType (TyFun l type1 type2) = TyFun (id l) (deTupleType type1) (deTupleType type2)
deTupleType (TyTuple l boxed type0) = TyTuple (id l) (deTupleBoxed boxed) (fmap (deTupleType) type0)
deTupleType (TyList l type0) = TyList (id l) (deTupleType type0)
deTupleType (TyParArray l type0) = TyParArray (id l) (deTupleType type0)
deTupleType (TyApp l type1 type2) = TyApp (id l) (deTupleType type1) (deTupleType type2)
deTupleType (TyVar l name) = TyVar (id l) (deTupleName name)
deTupleType (TyCon l qName) = TyCon (id l) (deTupleQName qName)
deTupleType (TyParen l type0) = TyParen (id l) (deTupleType type0)
deTupleType (TyInfix l type1 qName type2) = TyInfix (id l) (deTupleType type1) (deTupleQName qName) (deTupleType type2)
deTupleType (TyKind l type0 kind) = TyKind (id l) (deTupleType type0) (deTupleKind kind)
deTupleType (TyPromoted l promoted) = TyPromoted (id l) (deTuplePromoted promoted)
deTupleType (TyEquals l type1 type2) = TyEquals (id l) (deTupleType type1) (deTupleType type2)
deTupleType (TySplice l splice) = TySplice (id l) (deTupleSplice splice)
deTupleType (TyBang l bangType type0) = TyBang (id l) (deTupleBangType bangType) (deTupleType type0)
deTupleType (TyWildCard l name) = TyWildCard (id l) (fmap (deTupleName) name)
deTupleTypeEqn :: TypeEqn l -> TypeEqn l
deTupleTypeEqn (TypeEqn l type1 type2) = TypeEqn (id l) (deTupleType type1) (deTupleType type2)
deTupleWarningText :: WarningText l -> WarningText l
deTupleWarningText (DeprText l string) = DeprText (id l) (id string)
deTupleWarningText (WarnText l string) = WarnText (id l) (id string)
deTupleXAttr :: XAttr l -> XAttr l
deTupleXAttr (XAttr l xName exp) = XAttr (id l) (deTupleXName xName) (deTupleExp exp)
deTupleXName :: XName l -> XName l
deTupleXName (XName l string) = XName (id l) (id string)
deTupleXName (XDomName l string1 string2) = XDomName (id l) (id string1) (id string2)

