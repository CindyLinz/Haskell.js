module Desugar.LambdaCase where
import Language.Haskell.Exts.Annotated.Syntax
import Control.Arrow ((***))
deLambdaCaseActivation :: Activation l -> Activation l
deLambdaCaseActivation (ActiveFrom l int) = ActiveFrom (id l) (id int)
deLambdaCaseActivation (ActiveUntil l int) = ActiveUntil (id l) (id int)
deLambdaCaseAlt :: Alt l -> Alt l
deLambdaCaseAlt (Alt l pat rhs binds) = Alt (id l) (deLambdaCasePat pat) (deLambdaCaseRhs rhs) (fmap (deLambdaCaseBinds) binds)
deLambdaCaseAnnotation :: Annotation l -> Annotation l
deLambdaCaseAnnotation (Ann l name exp) = Ann (id l) (deLambdaCaseName name) (deLambdaCaseExp exp)
deLambdaCaseAnnotation (TypeAnn l name exp) = TypeAnn (id l) (deLambdaCaseName name) (deLambdaCaseExp exp)
deLambdaCaseAnnotation (ModuleAnn l exp) = ModuleAnn (id l) (deLambdaCaseExp exp)
deLambdaCaseAssoc :: Assoc l -> Assoc l
deLambdaCaseAssoc (AssocNone l) = AssocNone (id l)
deLambdaCaseAssoc (AssocLeft l) = AssocLeft (id l)
deLambdaCaseAssoc (AssocRight l) = AssocRight (id l)
deLambdaCaseAsst :: Asst l -> Asst l
deLambdaCaseAsst (ClassA l qName type0) = ClassA (id l) (deLambdaCaseQName qName) (fmap (deLambdaCaseType) type0)
deLambdaCaseAsst (AppA l name type0) = AppA (id l) (deLambdaCaseName name) (fmap (deLambdaCaseType) type0)
deLambdaCaseAsst (InfixA l type1 qName type2) = InfixA (id l) (deLambdaCaseType type1) (deLambdaCaseQName qName) (deLambdaCaseType type2)
deLambdaCaseAsst (IParam l iPName type0) = IParam (id l) (deLambdaCaseIPName iPName) (deLambdaCaseType type0)
deLambdaCaseAsst (EqualP l type1 type2) = EqualP (id l) (deLambdaCaseType type1) (deLambdaCaseType type2)
deLambdaCaseAsst (ParenA l asst) = ParenA (id l) (deLambdaCaseAsst asst)
deLambdaCaseAsst (WildCardA l name) = WildCardA (id l) (fmap (deLambdaCaseName) name)
deLambdaCaseBangType :: BangType l -> BangType l
deLambdaCaseBangType (BangedTy l) = BangedTy (id l)
deLambdaCaseBangType (UnpackedTy l) = UnpackedTy (id l)
deLambdaCaseBinds :: Binds l -> Binds l
deLambdaCaseBinds (BDecls l decl) = BDecls (id l) (fmap (deLambdaCaseDecl) decl)
deLambdaCaseBinds (IPBinds l iPBind) = IPBinds (id l) (fmap (deLambdaCaseIPBind) iPBind)
deLambdaCaseBooleanFormula :: BooleanFormula l -> BooleanFormula l
deLambdaCaseBooleanFormula (VarFormula l name) = VarFormula (id l) (deLambdaCaseName name)
deLambdaCaseBooleanFormula (AndFormula l booleanFormula) = AndFormula (id l) (fmap (deLambdaCaseBooleanFormula) booleanFormula)
deLambdaCaseBooleanFormula (OrFormula l booleanFormula) = OrFormula (id l) (fmap (deLambdaCaseBooleanFormula) booleanFormula)
deLambdaCaseBooleanFormula (ParenFormula l booleanFormula) = ParenFormula (id l) (deLambdaCaseBooleanFormula booleanFormula)
deLambdaCaseBoxed :: Boxed -> Boxed
deLambdaCaseBoxed (Boxed) = Boxed
deLambdaCaseBoxed (Unboxed) = Unboxed
deLambdaCaseBracket :: Bracket l -> Bracket l
deLambdaCaseBracket (ExpBracket l exp) = ExpBracket (id l) (deLambdaCaseExp exp)
deLambdaCaseBracket (PatBracket l pat) = PatBracket (id l) (deLambdaCasePat pat)
deLambdaCaseBracket (TypeBracket l type0) = TypeBracket (id l) (deLambdaCaseType type0)
deLambdaCaseBracket (DeclBracket l decl) = DeclBracket (id l) (fmap (deLambdaCaseDecl) decl)
deLambdaCaseCName :: CName l -> CName l
deLambdaCaseCName (VarName l name) = VarName (id l) (deLambdaCaseName name)
deLambdaCaseCName (ConName l name) = ConName (id l) (deLambdaCaseName name)
deLambdaCaseCallConv :: CallConv l -> CallConv l
deLambdaCaseCallConv (StdCall l) = StdCall (id l)
deLambdaCaseCallConv (CCall l) = CCall (id l)
deLambdaCaseCallConv (CPlusPlus l) = CPlusPlus (id l)
deLambdaCaseCallConv (DotNet l) = DotNet (id l)
deLambdaCaseCallConv (Jvm l) = Jvm (id l)
deLambdaCaseCallConv (Js l) = Js (id l)
deLambdaCaseCallConv (JavaScript l) = JavaScript (id l)
deLambdaCaseCallConv (CApi l) = CApi (id l)
deLambdaCaseClassDecl :: ClassDecl l -> ClassDecl l
deLambdaCaseClassDecl (ClsDecl l decl) = ClsDecl (id l) (deLambdaCaseDecl decl)
deLambdaCaseClassDecl (ClsDataFam l context declHead kind) = ClsDataFam (id l) (fmap (deLambdaCaseContext) context) (deLambdaCaseDeclHead declHead) (fmap (deLambdaCaseKind) kind)
deLambdaCaseClassDecl (ClsTyFam l declHead kind) = ClsTyFam (id l) (deLambdaCaseDeclHead declHead) (fmap (deLambdaCaseKind) kind)
deLambdaCaseClassDecl (ClsTyDef l type1 type2) = ClsTyDef (id l) (deLambdaCaseType type1) (deLambdaCaseType type2)
deLambdaCaseClassDecl (ClsDefSig l name type0) = ClsDefSig (id l) (deLambdaCaseName name) (deLambdaCaseType type0)
deLambdaCaseConDecl :: ConDecl l -> ConDecl l
deLambdaCaseConDecl (ConDecl l name type0) = ConDecl (id l) (deLambdaCaseName name) (fmap (deLambdaCaseType) type0)
deLambdaCaseConDecl (InfixConDecl l type1 name type2) = InfixConDecl (id l) (deLambdaCaseType type1) (deLambdaCaseName name) (deLambdaCaseType type2)
deLambdaCaseConDecl (RecDecl l name fieldDecl) = RecDecl (id l) (deLambdaCaseName name) (fmap (deLambdaCaseFieldDecl) fieldDecl)
deLambdaCaseContext :: Context l -> Context l
deLambdaCaseContext (CxSingle l asst) = CxSingle (id l) (deLambdaCaseAsst asst)
deLambdaCaseContext (CxTuple l asst) = CxTuple (id l) (fmap (deLambdaCaseAsst) asst)
deLambdaCaseContext (CxEmpty l) = CxEmpty (id l)
deLambdaCaseDataOrNew :: DataOrNew l -> DataOrNew l
deLambdaCaseDataOrNew (DataType l) = DataType (id l)
deLambdaCaseDataOrNew (NewType l) = NewType (id l)
deLambdaCaseDecl :: Decl l -> Decl l
deLambdaCaseDecl (TypeDecl l declHead type0) = TypeDecl (id l) (deLambdaCaseDeclHead declHead) (deLambdaCaseType type0)
deLambdaCaseDecl (TypeFamDecl l declHead kind) = TypeFamDecl (id l) (deLambdaCaseDeclHead declHead) (fmap (deLambdaCaseKind) kind)
deLambdaCaseDecl (ClosedTypeFamDecl l declHead kind typeEqn) = ClosedTypeFamDecl (id l) (deLambdaCaseDeclHead declHead) (fmap (deLambdaCaseKind) kind) (fmap (deLambdaCaseTypeEqn) typeEqn)
deLambdaCaseDecl (DataDecl l dataOrNew context declHead qualConDecl deriving0) = DataDecl (id l) (deLambdaCaseDataOrNew dataOrNew) (fmap (deLambdaCaseContext) context) (deLambdaCaseDeclHead declHead) (fmap (deLambdaCaseQualConDecl) qualConDecl) (fmap (deLambdaCaseDeriving) deriving0)
deLambdaCaseDecl (GDataDecl l dataOrNew context declHead kind gadtDecl deriving0) = GDataDecl (id l) (deLambdaCaseDataOrNew dataOrNew) (fmap (deLambdaCaseContext) context) (deLambdaCaseDeclHead declHead) (fmap (deLambdaCaseKind) kind) (fmap (deLambdaCaseGadtDecl) gadtDecl) (fmap (deLambdaCaseDeriving) deriving0)
deLambdaCaseDecl (DataFamDecl l context declHead kind) = DataFamDecl (id l) (fmap (deLambdaCaseContext) context) (deLambdaCaseDeclHead declHead) (fmap (deLambdaCaseKind) kind)
deLambdaCaseDecl (TypeInsDecl l type1 type2) = TypeInsDecl (id l) (deLambdaCaseType type1) (deLambdaCaseType type2)
deLambdaCaseDecl (DataInsDecl l dataOrNew type0 qualConDecl deriving0) = DataInsDecl (id l) (deLambdaCaseDataOrNew dataOrNew) (deLambdaCaseType type0) (fmap (deLambdaCaseQualConDecl) qualConDecl) (fmap (deLambdaCaseDeriving) deriving0)
deLambdaCaseDecl (GDataInsDecl l dataOrNew type0 kind gadtDecl deriving0) = GDataInsDecl (id l) (deLambdaCaseDataOrNew dataOrNew) (deLambdaCaseType type0) (fmap (deLambdaCaseKind) kind) (fmap (deLambdaCaseGadtDecl) gadtDecl) (fmap (deLambdaCaseDeriving) deriving0)
deLambdaCaseDecl (ClassDecl l context declHead funDep classDecl) = ClassDecl (id l) (fmap (deLambdaCaseContext) context) (deLambdaCaseDeclHead declHead) (fmap (deLambdaCaseFunDep) funDep) (fmap (fmap (deLambdaCaseClassDecl)) classDecl)
deLambdaCaseDecl (InstDecl l overlap instRule instDecl) = InstDecl (id l) (fmap (deLambdaCaseOverlap) overlap) (deLambdaCaseInstRule instRule) (fmap (fmap (deLambdaCaseInstDecl)) instDecl)
deLambdaCaseDecl (DerivDecl l overlap instRule) = DerivDecl (id l) (fmap (deLambdaCaseOverlap) overlap) (deLambdaCaseInstRule instRule)
deLambdaCaseDecl (InfixDecl l assoc int op) = InfixDecl (id l) (deLambdaCaseAssoc assoc) (fmap (id) int) (fmap (deLambdaCaseOp) op)
deLambdaCaseDecl (DefaultDecl l type0) = DefaultDecl (id l) (fmap (deLambdaCaseType) type0)
deLambdaCaseDecl (SpliceDecl l exp) = SpliceDecl (id l) (deLambdaCaseExp exp)
deLambdaCaseDecl (TypeSig l name type0) = TypeSig (id l) (fmap (deLambdaCaseName) name) (deLambdaCaseType type0)
deLambdaCaseDecl (PatSynSig l name tyVarBind context1 context2 type0) = PatSynSig (id l) (deLambdaCaseName name) (fmap (fmap (deLambdaCaseTyVarBind)) tyVarBind) (fmap (deLambdaCaseContext) context1) (fmap (deLambdaCaseContext) context2) (deLambdaCaseType type0)
deLambdaCaseDecl (FunBind l match) = FunBind (id l) (fmap (deLambdaCaseMatch) match)
deLambdaCaseDecl (PatBind l pat rhs binds) = PatBind (id l) (deLambdaCasePat pat) (deLambdaCaseRhs rhs) (fmap (deLambdaCaseBinds) binds)
deLambdaCaseDecl (PatSyn l pat1 pat2 patternSynDirection) = PatSyn (id l) (deLambdaCasePat pat1) (deLambdaCasePat pat2) (deLambdaCasePatternSynDirection patternSynDirection)
deLambdaCaseDecl (ForImp l callConv safety string name type0) = ForImp (id l) (deLambdaCaseCallConv callConv) (fmap (deLambdaCaseSafety) safety) (fmap (id) string) (deLambdaCaseName name) (deLambdaCaseType type0)
deLambdaCaseDecl (ForExp l callConv string name type0) = ForExp (id l) (deLambdaCaseCallConv callConv) (fmap (id) string) (deLambdaCaseName name) (deLambdaCaseType type0)
deLambdaCaseDecl (RulePragmaDecl l rule) = RulePragmaDecl (id l) (fmap (deLambdaCaseRule) rule)
deLambdaCaseDecl (DeprPragmaDecl l name) = DeprPragmaDecl (id l) (fmap (((fmap (deLambdaCaseName)) *** (id))) name)
deLambdaCaseDecl (WarnPragmaDecl l name) = WarnPragmaDecl (id l) (fmap (((fmap (deLambdaCaseName)) *** (id))) name)
deLambdaCaseDecl (InlineSig l bool activation qName) = InlineSig (id l) (id bool) (fmap (deLambdaCaseActivation) activation) (deLambdaCaseQName qName)
deLambdaCaseDecl (InlineConlikeSig l activation qName) = InlineConlikeSig (id l) (fmap (deLambdaCaseActivation) activation) (deLambdaCaseQName qName)
deLambdaCaseDecl (SpecSig l activation qName type0) = SpecSig (id l) (fmap (deLambdaCaseActivation) activation) (deLambdaCaseQName qName) (fmap (deLambdaCaseType) type0)
deLambdaCaseDecl (SpecInlineSig l bool activation qName type0) = SpecInlineSig (id l) (id bool) (fmap (deLambdaCaseActivation) activation) (deLambdaCaseQName qName) (fmap (deLambdaCaseType) type0)
deLambdaCaseDecl (InstSig l instRule) = InstSig (id l) (deLambdaCaseInstRule instRule)
deLambdaCaseDecl (AnnPragma l annotation) = AnnPragma (id l) (deLambdaCaseAnnotation annotation)
deLambdaCaseDecl (MinimalPragma l booleanFormula) = MinimalPragma (id l) (fmap (deLambdaCaseBooleanFormula) booleanFormula)
deLambdaCaseDecl (RoleAnnotDecl l qName role) = RoleAnnotDecl (id l) (deLambdaCaseQName qName) (fmap (deLambdaCaseRole) role)
deLambdaCaseDeclHead :: DeclHead l -> DeclHead l
deLambdaCaseDeclHead (DHead l name) = DHead (id l) (deLambdaCaseName name)
deLambdaCaseDeclHead (DHInfix l tyVarBind name) = DHInfix (id l) (deLambdaCaseTyVarBind tyVarBind) (deLambdaCaseName name)
deLambdaCaseDeclHead (DHParen l declHead) = DHParen (id l) (deLambdaCaseDeclHead declHead)
deLambdaCaseDeclHead (DHApp l declHead tyVarBind) = DHApp (id l) (deLambdaCaseDeclHead declHead) (deLambdaCaseTyVarBind tyVarBind)
deLambdaCaseDeriving :: Deriving l -> Deriving l
deLambdaCaseDeriving (Deriving l instRule) = Deriving (id l) (fmap (deLambdaCaseInstRule) instRule)
deLambdaCaseExp :: Exp l -> Exp l
deLambdaCaseExp (Var l qName) = Var (id l) (deLambdaCaseQName qName)
deLambdaCaseExp (IPVar l iPName) = IPVar (id l) (deLambdaCaseIPName iPName)
deLambdaCaseExp (Con l qName) = Con (id l) (deLambdaCaseQName qName)
deLambdaCaseExp (Lit l literal) = Lit (id l) (deLambdaCaseLiteral literal)
deLambdaCaseExp (InfixApp l exp1 qOp exp2) = InfixApp (id l) (deLambdaCaseExp exp1) (deLambdaCaseQOp qOp) (deLambdaCaseExp exp2)
deLambdaCaseExp (App l exp1 exp2) = App (id l) (deLambdaCaseExp exp1) (deLambdaCaseExp exp2)
deLambdaCaseExp (NegApp l exp) = NegApp (id l) (deLambdaCaseExp exp)
deLambdaCaseExp (Lambda l pat exp) = Lambda (id l) (fmap (deLambdaCasePat) pat) (deLambdaCaseExp exp)
deLambdaCaseExp (Let l binds exp) = Let (id l) (deLambdaCaseBinds binds) (deLambdaCaseExp exp)
deLambdaCaseExp (If l exp1 exp2 exp3) = If (id l) (deLambdaCaseExp exp1) (deLambdaCaseExp exp2) (deLambdaCaseExp exp3)
deLambdaCaseExp (MultiIf l guardedRhs) = MultiIf (id l) (fmap (deLambdaCaseGuardedRhs) guardedRhs)
deLambdaCaseExp (Case l exp alt) = Case (id l) (deLambdaCaseExp exp) (fmap (deLambdaCaseAlt) alt)
deLambdaCaseExp (Do l stmt) = Do (id l) (fmap (deLambdaCaseStmt) stmt)
deLambdaCaseExp (MDo l stmt) = MDo (id l) (fmap (deLambdaCaseStmt) stmt)
deLambdaCaseExp (Tuple l boxed exp) = Tuple (id l) (deLambdaCaseBoxed boxed) (fmap (deLambdaCaseExp) exp)
deLambdaCaseExp (TupleSection l boxed exp) = TupleSection (id l) (deLambdaCaseBoxed boxed) (fmap (fmap (deLambdaCaseExp)) exp)
deLambdaCaseExp (List l exp) = List (id l) (fmap (deLambdaCaseExp) exp)
deLambdaCaseExp (ParArray l exp) = ParArray (id l) (fmap (deLambdaCaseExp) exp)
deLambdaCaseExp (Paren l exp) = Paren (id l) (deLambdaCaseExp exp)
deLambdaCaseExp (LeftSection l exp qOp) = LeftSection (id l) (deLambdaCaseExp exp) (deLambdaCaseQOp qOp)
deLambdaCaseExp (RightSection l qOp exp) = RightSection (id l) (deLambdaCaseQOp qOp) (deLambdaCaseExp exp)
deLambdaCaseExp (RecConstr l qName fieldUpdate) = RecConstr (id l) (deLambdaCaseQName qName) (fmap (deLambdaCaseFieldUpdate) fieldUpdate)
deLambdaCaseExp (RecUpdate l exp fieldUpdate) = RecUpdate (id l) (deLambdaCaseExp exp) (fmap (deLambdaCaseFieldUpdate) fieldUpdate)
deLambdaCaseExp (EnumFrom l exp) = EnumFrom (id l) (deLambdaCaseExp exp)
deLambdaCaseExp (EnumFromTo l exp1 exp2) = EnumFromTo (id l) (deLambdaCaseExp exp1) (deLambdaCaseExp exp2)
deLambdaCaseExp (EnumFromThen l exp1 exp2) = EnumFromThen (id l) (deLambdaCaseExp exp1) (deLambdaCaseExp exp2)
deLambdaCaseExp (EnumFromThenTo l exp1 exp2 exp3) = EnumFromThenTo (id l) (deLambdaCaseExp exp1) (deLambdaCaseExp exp2) (deLambdaCaseExp exp3)
deLambdaCaseExp (ParArrayFromTo l exp1 exp2) = ParArrayFromTo (id l) (deLambdaCaseExp exp1) (deLambdaCaseExp exp2)
deLambdaCaseExp (ParArrayFromThenTo l exp1 exp2 exp3) = ParArrayFromThenTo (id l) (deLambdaCaseExp exp1) (deLambdaCaseExp exp2) (deLambdaCaseExp exp3)
deLambdaCaseExp (ListComp l exp qualStmt) = ListComp (id l) (deLambdaCaseExp exp) (fmap (deLambdaCaseQualStmt) qualStmt)
deLambdaCaseExp (ParComp l exp qualStmt) = ParComp (id l) (deLambdaCaseExp exp) (fmap (fmap (deLambdaCaseQualStmt)) qualStmt)
deLambdaCaseExp (ParArrayComp l exp qualStmt) = ParArrayComp (id l) (deLambdaCaseExp exp) (fmap (fmap (deLambdaCaseQualStmt)) qualStmt)
deLambdaCaseExp (ExpTypeSig l exp type0) = ExpTypeSig (id l) (deLambdaCaseExp exp) (deLambdaCaseType type0)
deLambdaCaseExp (VarQuote l qName) = VarQuote (id l) (deLambdaCaseQName qName)
deLambdaCaseExp (TypQuote l qName) = TypQuote (id l) (deLambdaCaseQName qName)
deLambdaCaseExp (BracketExp l bracket) = BracketExp (id l) (deLambdaCaseBracket bracket)
deLambdaCaseExp (SpliceExp l splice) = SpliceExp (id l) (deLambdaCaseSplice splice)
deLambdaCaseExp (QuasiQuote l string1 string2) = QuasiQuote (id l) (id string1) (id string2)
deLambdaCaseExp (XTag l xName xAttr exp1 exp2) = XTag (id l) (deLambdaCaseXName xName) (fmap (deLambdaCaseXAttr) xAttr) (fmap (deLambdaCaseExp) exp1) (fmap (deLambdaCaseExp) exp2)
deLambdaCaseExp (XETag l xName xAttr exp) = XETag (id l) (deLambdaCaseXName xName) (fmap (deLambdaCaseXAttr) xAttr) (fmap (deLambdaCaseExp) exp)
deLambdaCaseExp (XPcdata l string) = XPcdata (id l) (id string)
deLambdaCaseExp (XExpTag l exp) = XExpTag (id l) (deLambdaCaseExp exp)
deLambdaCaseExp (XChildTag l exp) = XChildTag (id l) (fmap (deLambdaCaseExp) exp)
deLambdaCaseExp (CorePragma l string exp) = CorePragma (id l) (id string) (deLambdaCaseExp exp)
deLambdaCaseExp (SCCPragma l string exp) = SCCPragma (id l) (id string) (deLambdaCaseExp exp)
deLambdaCaseExp (GenPragma l string int1 int2 exp) = GenPragma (id l) (id string) (((id) *** (id)) int1) (((id) *** (id)) int2) (deLambdaCaseExp exp)
deLambdaCaseExp (Proc l pat exp) = Proc (id l) (deLambdaCasePat pat) (deLambdaCaseExp exp)
deLambdaCaseExp (LeftArrApp l exp1 exp2) = LeftArrApp (id l) (deLambdaCaseExp exp1) (deLambdaCaseExp exp2)
deLambdaCaseExp (RightArrApp l exp1 exp2) = RightArrApp (id l) (deLambdaCaseExp exp1) (deLambdaCaseExp exp2)
deLambdaCaseExp (LeftArrHighApp l exp1 exp2) = LeftArrHighApp (id l) (deLambdaCaseExp exp1) (deLambdaCaseExp exp2)
deLambdaCaseExp (RightArrHighApp l exp1 exp2) = RightArrHighApp (id l) (deLambdaCaseExp exp1) (deLambdaCaseExp exp2)
deLambdaCaseExp (LCase l alt) =
  Lambda l [PVar l varName] (Case l (Var l (UnQual l varName)) alt)
  where
    varName = Ident l "lambdaCase-"
deLambdaCaseExp (ExprHole l) = ExprHole (id l)
deLambdaCaseExportSpec :: ExportSpec l -> ExportSpec l
deLambdaCaseExportSpec (EVar l qName) = EVar (id l) (deLambdaCaseQName qName)
deLambdaCaseExportSpec (EAbs l namespace qName) = EAbs (id l) (deLambdaCaseNamespace namespace) (deLambdaCaseQName qName)
deLambdaCaseExportSpec (EThingAll l qName) = EThingAll (id l) (deLambdaCaseQName qName)
deLambdaCaseExportSpec (EThingWith l qName cName) = EThingWith (id l) (deLambdaCaseQName qName) (fmap (deLambdaCaseCName) cName)
deLambdaCaseExportSpec (EModuleContents l moduleName) = EModuleContents (id l) (deLambdaCaseModuleName moduleName)
deLambdaCaseExportSpecList :: ExportSpecList l -> ExportSpecList l
deLambdaCaseExportSpecList (ExportSpecList l exportSpec) = ExportSpecList (id l) (fmap (deLambdaCaseExportSpec) exportSpec)
deLambdaCaseFieldDecl :: FieldDecl l -> FieldDecl l
deLambdaCaseFieldDecl (FieldDecl l name type0) = FieldDecl (id l) (fmap (deLambdaCaseName) name) (deLambdaCaseType type0)
deLambdaCaseFieldUpdate :: FieldUpdate l -> FieldUpdate l
deLambdaCaseFieldUpdate (FieldUpdate l qName exp) = FieldUpdate (id l) (deLambdaCaseQName qName) (deLambdaCaseExp exp)
deLambdaCaseFieldUpdate (FieldPun l qName) = FieldPun (id l) (deLambdaCaseQName qName)
deLambdaCaseFieldUpdate (FieldWildcard l) = FieldWildcard (id l)
deLambdaCaseFunDep :: FunDep l -> FunDep l
deLambdaCaseFunDep (FunDep l name1 name2) = FunDep (id l) (fmap (deLambdaCaseName) name1) (fmap (deLambdaCaseName) name2)
deLambdaCaseGadtDecl :: GadtDecl l -> GadtDecl l
deLambdaCaseGadtDecl (GadtDecl l name fieldDecl type0) = GadtDecl (id l) (deLambdaCaseName name) (fmap (fmap (deLambdaCaseFieldDecl)) fieldDecl) (deLambdaCaseType type0)
deLambdaCaseGuardedRhs :: GuardedRhs l -> GuardedRhs l
deLambdaCaseGuardedRhs (GuardedRhs l stmt exp) = GuardedRhs (id l) (fmap (deLambdaCaseStmt) stmt) (deLambdaCaseExp exp)
deLambdaCaseIPBind :: IPBind l -> IPBind l
deLambdaCaseIPBind (IPBind l iPName exp) = IPBind (id l) (deLambdaCaseIPName iPName) (deLambdaCaseExp exp)
deLambdaCaseIPName :: IPName l -> IPName l
deLambdaCaseIPName (IPDup l string) = IPDup (id l) (id string)
deLambdaCaseIPName (IPLin l string) = IPLin (id l) (id string)
deLambdaCaseImportDecl :: ImportDecl l -> ImportDecl l
deLambdaCaseImportDecl (ImportDecl importAnn importModule importQualified importSrc importSafe importPkg importAs importSpecs) = ImportDecl (id importAnn) (deLambdaCaseModuleName importModule) (id importQualified) (id importSrc) (id importSafe) (fmap (id) importPkg) (fmap (deLambdaCaseModuleName) importAs) (fmap (deLambdaCaseImportSpecList) importSpecs)
deLambdaCaseImportSpec :: ImportSpec l -> ImportSpec l
deLambdaCaseImportSpec (IVar l name) = IVar (id l) (deLambdaCaseName name)
deLambdaCaseImportSpec (IAbs l namespace name) = IAbs (id l) (deLambdaCaseNamespace namespace) (deLambdaCaseName name)
deLambdaCaseImportSpec (IThingAll l name) = IThingAll (id l) (deLambdaCaseName name)
deLambdaCaseImportSpec (IThingWith l name cName) = IThingWith (id l) (deLambdaCaseName name) (fmap (deLambdaCaseCName) cName)
deLambdaCaseImportSpecList :: ImportSpecList l -> ImportSpecList l
deLambdaCaseImportSpecList (ImportSpecList l bool importSpec) = ImportSpecList (id l) (id bool) (fmap (deLambdaCaseImportSpec) importSpec)
deLambdaCaseInstDecl :: InstDecl l -> InstDecl l
deLambdaCaseInstDecl (InsDecl l decl) = InsDecl (id l) (deLambdaCaseDecl decl)
deLambdaCaseInstDecl (InsType l type1 type2) = InsType (id l) (deLambdaCaseType type1) (deLambdaCaseType type2)
deLambdaCaseInstDecl (InsData l dataOrNew type0 qualConDecl deriving0) = InsData (id l) (deLambdaCaseDataOrNew dataOrNew) (deLambdaCaseType type0) (fmap (deLambdaCaseQualConDecl) qualConDecl) (fmap (deLambdaCaseDeriving) deriving0)
deLambdaCaseInstDecl (InsGData l dataOrNew type0 kind gadtDecl deriving0) = InsGData (id l) (deLambdaCaseDataOrNew dataOrNew) (deLambdaCaseType type0) (fmap (deLambdaCaseKind) kind) (fmap (deLambdaCaseGadtDecl) gadtDecl) (fmap (deLambdaCaseDeriving) deriving0)
deLambdaCaseInstHead :: InstHead l -> InstHead l
deLambdaCaseInstHead (IHCon l qName) = IHCon (id l) (deLambdaCaseQName qName)
deLambdaCaseInstHead (IHInfix l type0 qName) = IHInfix (id l) (deLambdaCaseType type0) (deLambdaCaseQName qName)
deLambdaCaseInstHead (IHParen l instHead) = IHParen (id l) (deLambdaCaseInstHead instHead)
deLambdaCaseInstHead (IHApp l instHead type0) = IHApp (id l) (deLambdaCaseInstHead instHead) (deLambdaCaseType type0)
deLambdaCaseInstRule :: InstRule l -> InstRule l
deLambdaCaseInstRule (IRule l tyVarBind context instHead) = IRule (id l) (fmap (fmap (deLambdaCaseTyVarBind)) tyVarBind) (fmap (deLambdaCaseContext) context) (deLambdaCaseInstHead instHead)
deLambdaCaseInstRule (IParen l instRule) = IParen (id l) (deLambdaCaseInstRule instRule)
deLambdaCaseKind :: Kind l -> Kind l
deLambdaCaseKind (KindStar l) = KindStar (id l)
deLambdaCaseKind (KindFn l kind1 kind2) = KindFn (id l) (deLambdaCaseKind kind1) (deLambdaCaseKind kind2)
deLambdaCaseKind (KindParen l kind) = KindParen (id l) (deLambdaCaseKind kind)
deLambdaCaseKind (KindVar l qName) = KindVar (id l) (deLambdaCaseQName qName)
deLambdaCaseKind (KindApp l kind1 kind2) = KindApp (id l) (deLambdaCaseKind kind1) (deLambdaCaseKind kind2)
deLambdaCaseKind (KindTuple l kind) = KindTuple (id l) (fmap (deLambdaCaseKind) kind)
deLambdaCaseKind (KindList l kind) = KindList (id l) (deLambdaCaseKind kind)
deLambdaCaseLiteral :: Literal l -> Literal l
deLambdaCaseLiteral (Char l char string) = Char (id l) (id char) (id string)
deLambdaCaseLiteral (String l string1 string2) = String (id l) (id string1) (id string2)
deLambdaCaseLiteral (Int l integer string) = Int (id l) (id integer) (id string)
deLambdaCaseLiteral (Frac l rational string) = Frac (id l) (id rational) (id string)
deLambdaCaseLiteral (PrimInt l integer string) = PrimInt (id l) (id integer) (id string)
deLambdaCaseLiteral (PrimWord l integer string) = PrimWord (id l) (id integer) (id string)
deLambdaCaseLiteral (PrimFloat l rational string) = PrimFloat (id l) (id rational) (id string)
deLambdaCaseLiteral (PrimDouble l rational string) = PrimDouble (id l) (id rational) (id string)
deLambdaCaseLiteral (PrimChar l char string) = PrimChar (id l) (id char) (id string)
deLambdaCaseLiteral (PrimString l string1 string2) = PrimString (id l) (id string1) (id string2)
deLambdaCaseMatch :: Match l -> Match l
deLambdaCaseMatch (Match l name pat rhs binds) = Match (id l) (deLambdaCaseName name) (fmap (deLambdaCasePat) pat) (deLambdaCaseRhs rhs) (fmap (deLambdaCaseBinds) binds)
deLambdaCaseMatch (InfixMatch l pat1 name pat2 rhs binds) = InfixMatch (id l) (deLambdaCasePat pat1) (deLambdaCaseName name) (fmap (deLambdaCasePat) pat2) (deLambdaCaseRhs rhs) (fmap (deLambdaCaseBinds) binds)
deLambdaCaseModule :: Module l -> Module l
deLambdaCaseModule (Module l moduleHead modulePragma importDecl decl) = Module (id l) (fmap (deLambdaCaseModuleHead) moduleHead) (fmap (deLambdaCaseModulePragma) modulePragma) (fmap (deLambdaCaseImportDecl) importDecl) (fmap (deLambdaCaseDecl) decl)
deLambdaCaseModule (XmlPage l moduleName modulePragma xName xAttr exp1 exp2) = XmlPage (id l) (deLambdaCaseModuleName moduleName) (fmap (deLambdaCaseModulePragma) modulePragma) (deLambdaCaseXName xName) (fmap (deLambdaCaseXAttr) xAttr) (fmap (deLambdaCaseExp) exp1) (fmap (deLambdaCaseExp) exp2)
deLambdaCaseModule (XmlHybrid l moduleHead modulePragma importDecl decl xName xAttr exp1 exp2) = XmlHybrid (id l) (fmap (deLambdaCaseModuleHead) moduleHead) (fmap (deLambdaCaseModulePragma) modulePragma) (fmap (deLambdaCaseImportDecl) importDecl) (fmap (deLambdaCaseDecl) decl) (deLambdaCaseXName xName) (fmap (deLambdaCaseXAttr) xAttr) (fmap (deLambdaCaseExp) exp1) (fmap (deLambdaCaseExp) exp2)
deLambdaCaseModuleHead :: ModuleHead l -> ModuleHead l
deLambdaCaseModuleHead (ModuleHead l moduleName warningText exportSpecList) = ModuleHead (id l) (deLambdaCaseModuleName moduleName) (fmap (deLambdaCaseWarningText) warningText) (fmap (deLambdaCaseExportSpecList) exportSpecList)
deLambdaCaseModuleName :: ModuleName l -> ModuleName l
deLambdaCaseModuleName (ModuleName l string) = ModuleName (id l) (id string)
deLambdaCaseModulePragma :: ModulePragma l -> ModulePragma l
deLambdaCaseModulePragma (LanguagePragma l name) = LanguagePragma (id l) (fmap (deLambdaCaseName) name)
deLambdaCaseModulePragma (OptionsPragma l tool string) = OptionsPragma (id l) (fmap (deLambdaCaseTool) tool) (id string)
deLambdaCaseModulePragma (AnnModulePragma l annotation) = AnnModulePragma (id l) (deLambdaCaseAnnotation annotation)
deLambdaCaseName :: Name l -> Name l
deLambdaCaseName (Ident l string) = Ident (id l) (id string)
deLambdaCaseName (Symbol l string) = Symbol (id l) (id string)
deLambdaCaseNamespace :: Namespace l -> Namespace l
deLambdaCaseNamespace (NoNamespace l) = NoNamespace (id l)
deLambdaCaseNamespace (TypeNamespace l) = TypeNamespace (id l)
deLambdaCaseNamespace (PatternNamespace l) = PatternNamespace (id l)
deLambdaCaseOp :: Op l -> Op l
deLambdaCaseOp (VarOp l name) = VarOp (id l) (deLambdaCaseName name)
deLambdaCaseOp (ConOp l name) = ConOp (id l) (deLambdaCaseName name)
deLambdaCaseOverlap :: Overlap l -> Overlap l
deLambdaCaseOverlap (NoOverlap l) = NoOverlap (id l)
deLambdaCaseOverlap (Overlap l) = Overlap (id l)
deLambdaCaseOverlap (Incoherent l) = Incoherent (id l)
deLambdaCasePXAttr :: PXAttr l -> PXAttr l
deLambdaCasePXAttr (PXAttr l xName pat) = PXAttr (id l) (deLambdaCaseXName xName) (deLambdaCasePat pat)
deLambdaCasePat :: Pat l -> Pat l
deLambdaCasePat (PVar l name) = PVar (id l) (deLambdaCaseName name)
deLambdaCasePat (PLit l sign literal) = PLit (id l) (deLambdaCaseSign sign) (deLambdaCaseLiteral literal)
deLambdaCasePat (PNPlusK l name integer) = PNPlusK (id l) (deLambdaCaseName name) (id integer)
deLambdaCasePat (PInfixApp l pat1 qName pat2) = PInfixApp (id l) (deLambdaCasePat pat1) (deLambdaCaseQName qName) (deLambdaCasePat pat2)
deLambdaCasePat (PApp l qName pat) = PApp (id l) (deLambdaCaseQName qName) (fmap (deLambdaCasePat) pat)
deLambdaCasePat (PTuple l boxed pat) = PTuple (id l) (deLambdaCaseBoxed boxed) (fmap (deLambdaCasePat) pat)
deLambdaCasePat (PList l pat) = PList (id l) (fmap (deLambdaCasePat) pat)
deLambdaCasePat (PParen l pat) = PParen (id l) (deLambdaCasePat pat)
deLambdaCasePat (PRec l qName patField) = PRec (id l) (deLambdaCaseQName qName) (fmap (deLambdaCasePatField) patField)
deLambdaCasePat (PAsPat l name pat) = PAsPat (id l) (deLambdaCaseName name) (deLambdaCasePat pat)
deLambdaCasePat (PWildCard l) = PWildCard (id l)
deLambdaCasePat (PIrrPat l pat) = PIrrPat (id l) (deLambdaCasePat pat)
deLambdaCasePat (PatTypeSig l pat type0) = PatTypeSig (id l) (deLambdaCasePat pat) (deLambdaCaseType type0)
deLambdaCasePat (PViewPat l exp pat) = PViewPat (id l) (deLambdaCaseExp exp) (deLambdaCasePat pat)
deLambdaCasePat (PRPat l rPat) = PRPat (id l) (fmap (deLambdaCaseRPat) rPat)
deLambdaCasePat (PXTag l xName pXAttr pat1 pat2) = PXTag (id l) (deLambdaCaseXName xName) (fmap (deLambdaCasePXAttr) pXAttr) (fmap (deLambdaCasePat) pat1) (fmap (deLambdaCasePat) pat2)
deLambdaCasePat (PXETag l xName pXAttr pat) = PXETag (id l) (deLambdaCaseXName xName) (fmap (deLambdaCasePXAttr) pXAttr) (fmap (deLambdaCasePat) pat)
deLambdaCasePat (PXPcdata l string) = PXPcdata (id l) (id string)
deLambdaCasePat (PXPatTag l pat) = PXPatTag (id l) (deLambdaCasePat pat)
deLambdaCasePat (PXRPats l rPat) = PXRPats (id l) (fmap (deLambdaCaseRPat) rPat)
deLambdaCasePat (PQuasiQuote l string1 string2) = PQuasiQuote (id l) (id string1) (id string2)
deLambdaCasePat (PBangPat l pat) = PBangPat (id l) (deLambdaCasePat pat)
deLambdaCasePatField :: PatField l -> PatField l
deLambdaCasePatField (PFieldPat l qName pat) = PFieldPat (id l) (deLambdaCaseQName qName) (deLambdaCasePat pat)
deLambdaCasePatField (PFieldPun l qName) = PFieldPun (id l) (deLambdaCaseQName qName)
deLambdaCasePatField (PFieldWildcard l) = PFieldWildcard (id l)
deLambdaCasePatternSynDirection :: PatternSynDirection l -> PatternSynDirection l
deLambdaCasePatternSynDirection (Unidirectional) = Unidirectional
deLambdaCasePatternSynDirection (ImplicitBidirectional) = ImplicitBidirectional
deLambdaCasePatternSynDirection (ExplicitBidirectional l decl) = ExplicitBidirectional (id l) (fmap (deLambdaCaseDecl) decl)
deLambdaCasePromoted :: Promoted l -> Promoted l
deLambdaCasePromoted (PromotedInteger l integer string) = PromotedInteger (id l) (id integer) (id string)
deLambdaCasePromoted (PromotedString l string1 string2) = PromotedString (id l) (id string1) (id string2)
deLambdaCasePromoted (PromotedCon l bool qName) = PromotedCon (id l) (id bool) (deLambdaCaseQName qName)
deLambdaCasePromoted (PromotedList l bool type0) = PromotedList (id l) (id bool) (fmap (deLambdaCaseType) type0)
deLambdaCasePromoted (PromotedTuple l type0) = PromotedTuple (id l) (fmap (deLambdaCaseType) type0)
deLambdaCasePromoted (PromotedUnit l) = PromotedUnit (id l)
deLambdaCaseQName :: QName l -> QName l
deLambdaCaseQName (Qual l moduleName name) = Qual (id l) (deLambdaCaseModuleName moduleName) (deLambdaCaseName name)
deLambdaCaseQName (UnQual l name) = UnQual (id l) (deLambdaCaseName name)
deLambdaCaseQName (Special l specialCon) = Special (id l) (deLambdaCaseSpecialCon specialCon)
deLambdaCaseQOp :: QOp l -> QOp l
deLambdaCaseQOp (QVarOp l qName) = QVarOp (id l) (deLambdaCaseQName qName)
deLambdaCaseQOp (QConOp l qName) = QConOp (id l) (deLambdaCaseQName qName)
deLambdaCaseQualConDecl :: QualConDecl l -> QualConDecl l
deLambdaCaseQualConDecl (QualConDecl l tyVarBind context conDecl) = QualConDecl (id l) (fmap (fmap (deLambdaCaseTyVarBind)) tyVarBind) (fmap (deLambdaCaseContext) context) (deLambdaCaseConDecl conDecl)
deLambdaCaseQualStmt :: QualStmt l -> QualStmt l
deLambdaCaseQualStmt (QualStmt l stmt) = QualStmt (id l) (deLambdaCaseStmt stmt)
deLambdaCaseQualStmt (ThenTrans l exp) = ThenTrans (id l) (deLambdaCaseExp exp)
deLambdaCaseQualStmt (ThenBy l exp1 exp2) = ThenBy (id l) (deLambdaCaseExp exp1) (deLambdaCaseExp exp2)
deLambdaCaseQualStmt (GroupBy l exp) = GroupBy (id l) (deLambdaCaseExp exp)
deLambdaCaseQualStmt (GroupUsing l exp) = GroupUsing (id l) (deLambdaCaseExp exp)
deLambdaCaseQualStmt (GroupByUsing l exp1 exp2) = GroupByUsing (id l) (deLambdaCaseExp exp1) (deLambdaCaseExp exp2)
deLambdaCaseRPat :: RPat l -> RPat l
deLambdaCaseRPat (RPOp l rPat rPatOp) = RPOp (id l) (deLambdaCaseRPat rPat) (deLambdaCaseRPatOp rPatOp)
deLambdaCaseRPat (RPEither l rPat1 rPat2) = RPEither (id l) (deLambdaCaseRPat rPat1) (deLambdaCaseRPat rPat2)
deLambdaCaseRPat (RPSeq l rPat) = RPSeq (id l) (fmap (deLambdaCaseRPat) rPat)
deLambdaCaseRPat (RPGuard l pat stmt) = RPGuard (id l) (deLambdaCasePat pat) (fmap (deLambdaCaseStmt) stmt)
deLambdaCaseRPat (RPCAs l name rPat) = RPCAs (id l) (deLambdaCaseName name) (deLambdaCaseRPat rPat)
deLambdaCaseRPat (RPAs l name rPat) = RPAs (id l) (deLambdaCaseName name) (deLambdaCaseRPat rPat)
deLambdaCaseRPat (RPParen l rPat) = RPParen (id l) (deLambdaCaseRPat rPat)
deLambdaCaseRPat (RPPat l pat) = RPPat (id l) (deLambdaCasePat pat)
deLambdaCaseRPatOp :: RPatOp l -> RPatOp l
deLambdaCaseRPatOp (RPStar l) = RPStar (id l)
deLambdaCaseRPatOp (RPStarG l) = RPStarG (id l)
deLambdaCaseRPatOp (RPPlus l) = RPPlus (id l)
deLambdaCaseRPatOp (RPPlusG l) = RPPlusG (id l)
deLambdaCaseRPatOp (RPOpt l) = RPOpt (id l)
deLambdaCaseRPatOp (RPOptG l) = RPOptG (id l)
deLambdaCaseRhs :: Rhs l -> Rhs l
deLambdaCaseRhs (UnGuardedRhs l exp) = UnGuardedRhs (id l) (deLambdaCaseExp exp)
deLambdaCaseRhs (GuardedRhss l guardedRhs) = GuardedRhss (id l) (fmap (deLambdaCaseGuardedRhs) guardedRhs)
deLambdaCaseRole :: Role l -> Role l
deLambdaCaseRole (Nominal l) = Nominal (id l)
deLambdaCaseRole (Representational l) = Representational (id l)
deLambdaCaseRole (Phantom l) = Phantom (id l)
deLambdaCaseRole (RoleWildcard l) = RoleWildcard (id l)
deLambdaCaseRule :: Rule l -> Rule l
deLambdaCaseRule (Rule l string activation ruleVar exp1 exp2) = Rule (id l) (id string) (fmap (deLambdaCaseActivation) activation) (fmap (fmap (deLambdaCaseRuleVar)) ruleVar) (deLambdaCaseExp exp1) (deLambdaCaseExp exp2)
deLambdaCaseRuleVar :: RuleVar l -> RuleVar l
deLambdaCaseRuleVar (RuleVar l name) = RuleVar (id l) (deLambdaCaseName name)
deLambdaCaseRuleVar (TypedRuleVar l name type0) = TypedRuleVar (id l) (deLambdaCaseName name) (deLambdaCaseType type0)
deLambdaCaseSafety :: Safety l -> Safety l
deLambdaCaseSafety (PlayRisky l) = PlayRisky (id l)
deLambdaCaseSafety (PlaySafe l bool) = PlaySafe (id l) (id bool)
deLambdaCaseSafety (PlayInterruptible l) = PlayInterruptible (id l)
deLambdaCaseSign :: Sign l -> Sign l
deLambdaCaseSign (Signless l) = Signless (id l)
deLambdaCaseSign (Negative l) = Negative (id l)
deLambdaCaseSpecialCon :: SpecialCon l -> SpecialCon l
deLambdaCaseSpecialCon (UnitCon l) = UnitCon (id l)
deLambdaCaseSpecialCon (ListCon l) = ListCon (id l)
deLambdaCaseSpecialCon (FunCon l) = FunCon (id l)
deLambdaCaseSpecialCon (TupleCon l boxed int) = TupleCon (id l) (deLambdaCaseBoxed boxed) (id int)
deLambdaCaseSpecialCon (Cons l) = Cons (id l)
deLambdaCaseSpecialCon (UnboxedSingleCon l) = UnboxedSingleCon (id l)
deLambdaCaseSplice :: Splice l -> Splice l
deLambdaCaseSplice (IdSplice l string) = IdSplice (id l) (id string)
deLambdaCaseSplice (ParenSplice l exp) = ParenSplice (id l) (deLambdaCaseExp exp)
deLambdaCaseStmt :: Stmt l -> Stmt l
deLambdaCaseStmt (Generator l pat exp) = Generator (id l) (deLambdaCasePat pat) (deLambdaCaseExp exp)
deLambdaCaseStmt (Qualifier l exp) = Qualifier (id l) (deLambdaCaseExp exp)
deLambdaCaseStmt (LetStmt l binds) = LetStmt (id l) (deLambdaCaseBinds binds)
deLambdaCaseStmt (RecStmt l stmt) = RecStmt (id l) (fmap (deLambdaCaseStmt) stmt)
deLambdaCaseTool :: Tool -> Tool
deLambdaCaseTool (GHC) = GHC
deLambdaCaseTool (HUGS) = HUGS
deLambdaCaseTool (NHC98) = NHC98
deLambdaCaseTool (YHC) = YHC
deLambdaCaseTool (HADDOCK) = HADDOCK
deLambdaCaseTool (UnknownTool string) = UnknownTool (id string)
deLambdaCaseTyVarBind :: TyVarBind l -> TyVarBind l
deLambdaCaseTyVarBind (KindedVar l name kind) = KindedVar (id l) (deLambdaCaseName name) (deLambdaCaseKind kind)
deLambdaCaseTyVarBind (UnkindedVar l name) = UnkindedVar (id l) (deLambdaCaseName name)
deLambdaCaseType :: Type l -> Type l
deLambdaCaseType (TyForall l tyVarBind context type0) = TyForall (id l) (fmap (fmap (deLambdaCaseTyVarBind)) tyVarBind) (fmap (deLambdaCaseContext) context) (deLambdaCaseType type0)
deLambdaCaseType (TyFun l type1 type2) = TyFun (id l) (deLambdaCaseType type1) (deLambdaCaseType type2)
deLambdaCaseType (TyTuple l boxed type0) = TyTuple (id l) (deLambdaCaseBoxed boxed) (fmap (deLambdaCaseType) type0)
deLambdaCaseType (TyList l type0) = TyList (id l) (deLambdaCaseType type0)
deLambdaCaseType (TyParArray l type0) = TyParArray (id l) (deLambdaCaseType type0)
deLambdaCaseType (TyApp l type1 type2) = TyApp (id l) (deLambdaCaseType type1) (deLambdaCaseType type2)
deLambdaCaseType (TyVar l name) = TyVar (id l) (deLambdaCaseName name)
deLambdaCaseType (TyCon l qName) = TyCon (id l) (deLambdaCaseQName qName)
deLambdaCaseType (TyParen l type0) = TyParen (id l) (deLambdaCaseType type0)
deLambdaCaseType (TyInfix l type1 qName type2) = TyInfix (id l) (deLambdaCaseType type1) (deLambdaCaseQName qName) (deLambdaCaseType type2)
deLambdaCaseType (TyKind l type0 kind) = TyKind (id l) (deLambdaCaseType type0) (deLambdaCaseKind kind)
deLambdaCaseType (TyPromoted l promoted) = TyPromoted (id l) (deLambdaCasePromoted promoted)
deLambdaCaseType (TyEquals l type1 type2) = TyEquals (id l) (deLambdaCaseType type1) (deLambdaCaseType type2)
deLambdaCaseType (TySplice l splice) = TySplice (id l) (deLambdaCaseSplice splice)
deLambdaCaseType (TyBang l bangType type0) = TyBang (id l) (deLambdaCaseBangType bangType) (deLambdaCaseType type0)
deLambdaCaseType (TyWildCard l name) = TyWildCard (id l) (fmap (deLambdaCaseName) name)
deLambdaCaseTypeEqn :: TypeEqn l -> TypeEqn l
deLambdaCaseTypeEqn (TypeEqn l type1 type2) = TypeEqn (id l) (deLambdaCaseType type1) (deLambdaCaseType type2)
deLambdaCaseWarningText :: WarningText l -> WarningText l
deLambdaCaseWarningText (DeprText l string) = DeprText (id l) (id string)
deLambdaCaseWarningText (WarnText l string) = WarnText (id l) (id string)
deLambdaCaseXAttr :: XAttr l -> XAttr l
deLambdaCaseXAttr (XAttr l xName exp) = XAttr (id l) (deLambdaCaseXName xName) (deLambdaCaseExp exp)
deLambdaCaseXName :: XName l -> XName l
deLambdaCaseXName (XName l string) = XName (id l) (id string)
deLambdaCaseXName (XDomName l string1 string2) = XDomName (id l) (id string1) (id string2)

