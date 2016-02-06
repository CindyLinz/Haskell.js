module Desugar.String where
import Language.Haskell.Exts.Annotated.Syntax
import Control.Arrow ((***))
deStringActivation :: Activation l -> Activation l
deStringActivation (ActiveFrom l int) = ActiveFrom (id l) (id int)
deStringActivation (ActiveUntil l int) = ActiveUntil (id l) (id int)
deStringAlt :: Alt l -> Alt l
deStringAlt (Alt l pat rhs binds) = Alt (id l) (deStringPat pat) (deStringRhs rhs) (fmap (deStringBinds) binds)
deStringAnnotation :: Annotation l -> Annotation l
deStringAnnotation (Ann l name exp) = Ann (id l) (deStringName name) (deStringExp exp)
deStringAnnotation (TypeAnn l name exp) = TypeAnn (id l) (deStringName name) (deStringExp exp)
deStringAnnotation (ModuleAnn l exp) = ModuleAnn (id l) (deStringExp exp)
deStringAssoc :: Assoc l -> Assoc l
deStringAssoc (AssocNone l) = AssocNone (id l)
deStringAssoc (AssocLeft l) = AssocLeft (id l)
deStringAssoc (AssocRight l) = AssocRight (id l)
deStringAsst :: Asst l -> Asst l
deStringAsst (ClassA l qName type0) = ClassA (id l) (deStringQName qName) (fmap (deStringType) type0)
deStringAsst (AppA l name type0) = AppA (id l) (deStringName name) (fmap (deStringType) type0)
deStringAsst (InfixA l type1 qName type2) = InfixA (id l) (deStringType type1) (deStringQName qName) (deStringType type2)
deStringAsst (IParam l iPName type0) = IParam (id l) (deStringIPName iPName) (deStringType type0)
deStringAsst (EqualP l type1 type2) = EqualP (id l) (deStringType type1) (deStringType type2)
deStringAsst (ParenA l asst) = ParenA (id l) (deStringAsst asst)
deStringAsst (WildCardA l name) = WildCardA (id l) (fmap (deStringName) name)
deStringBangType :: BangType l -> BangType l
deStringBangType (BangedTy l) = BangedTy (id l)
deStringBangType (UnpackedTy l) = UnpackedTy (id l)
deStringBinds :: Binds l -> Binds l
deStringBinds (BDecls l decl) = BDecls (id l) (fmap (deStringDecl) decl)
deStringBinds (IPBinds l iPBind) = IPBinds (id l) (fmap (deStringIPBind) iPBind)
deStringBooleanFormula :: BooleanFormula l -> BooleanFormula l
deStringBooleanFormula (VarFormula l name) = VarFormula (id l) (deStringName name)
deStringBooleanFormula (AndFormula l booleanFormula) = AndFormula (id l) (fmap (deStringBooleanFormula) booleanFormula)
deStringBooleanFormula (OrFormula l booleanFormula) = OrFormula (id l) (fmap (deStringBooleanFormula) booleanFormula)
deStringBooleanFormula (ParenFormula l booleanFormula) = ParenFormula (id l) (deStringBooleanFormula booleanFormula)
deStringBoxed :: Boxed -> Boxed
deStringBoxed (Boxed) = Boxed
deStringBoxed (Unboxed) = Unboxed
deStringBracket :: Bracket l -> Bracket l
deStringBracket (ExpBracket l exp) = ExpBracket (id l) (deStringExp exp)
deStringBracket (PatBracket l pat) = PatBracket (id l) (deStringPat pat)
deStringBracket (TypeBracket l type0) = TypeBracket (id l) (deStringType type0)
deStringBracket (DeclBracket l decl) = DeclBracket (id l) (fmap (deStringDecl) decl)
deStringCName :: CName l -> CName l
deStringCName (VarName l name) = VarName (id l) (deStringName name)
deStringCName (ConName l name) = ConName (id l) (deStringName name)
deStringCallConv :: CallConv l -> CallConv l
deStringCallConv (StdCall l) = StdCall (id l)
deStringCallConv (CCall l) = CCall (id l)
deStringCallConv (CPlusPlus l) = CPlusPlus (id l)
deStringCallConv (DotNet l) = DotNet (id l)
deStringCallConv (Jvm l) = Jvm (id l)
deStringCallConv (Js l) = Js (id l)
deStringCallConv (JavaScript l) = JavaScript (id l)
deStringCallConv (CApi l) = CApi (id l)
deStringClassDecl :: ClassDecl l -> ClassDecl l
deStringClassDecl (ClsDecl l decl) = ClsDecl (id l) (deStringDecl decl)
deStringClassDecl (ClsDataFam l context declHead kind) = ClsDataFam (id l) (fmap (deStringContext) context) (deStringDeclHead declHead) (fmap (deStringKind) kind)
deStringClassDecl (ClsTyFam l declHead kind) = ClsTyFam (id l) (deStringDeclHead declHead) (fmap (deStringKind) kind)
deStringClassDecl (ClsTyDef l type1 type2) = ClsTyDef (id l) (deStringType type1) (deStringType type2)
deStringClassDecl (ClsDefSig l name type0) = ClsDefSig (id l) (deStringName name) (deStringType type0)
deStringConDecl :: ConDecl l -> ConDecl l
deStringConDecl (ConDecl l name type0) = ConDecl (id l) (deStringName name) (fmap (deStringType) type0)
deStringConDecl (InfixConDecl l type1 name type2) = InfixConDecl (id l) (deStringType type1) (deStringName name) (deStringType type2)
deStringConDecl (RecDecl l name fieldDecl) = RecDecl (id l) (deStringName name) (fmap (deStringFieldDecl) fieldDecl)
deStringContext :: Context l -> Context l
deStringContext (CxSingle l asst) = CxSingle (id l) (deStringAsst asst)
deStringContext (CxTuple l asst) = CxTuple (id l) (fmap (deStringAsst) asst)
deStringContext (CxEmpty l) = CxEmpty (id l)
deStringDataOrNew :: DataOrNew l -> DataOrNew l
deStringDataOrNew (DataType l) = DataType (id l)
deStringDataOrNew (NewType l) = NewType (id l)
deStringDecl :: Decl l -> Decl l
deStringDecl (TypeDecl l declHead type0) = TypeDecl (id l) (deStringDeclHead declHead) (deStringType type0)
deStringDecl (TypeFamDecl l declHead kind) = TypeFamDecl (id l) (deStringDeclHead declHead) (fmap (deStringKind) kind)
deStringDecl (ClosedTypeFamDecl l declHead kind typeEqn) = ClosedTypeFamDecl (id l) (deStringDeclHead declHead) (fmap (deStringKind) kind) (fmap (deStringTypeEqn) typeEqn)
deStringDecl (DataDecl l dataOrNew context declHead qualConDecl deriving0) = DataDecl (id l) (deStringDataOrNew dataOrNew) (fmap (deStringContext) context) (deStringDeclHead declHead) (fmap (deStringQualConDecl) qualConDecl) (fmap (deStringDeriving) deriving0)
deStringDecl (GDataDecl l dataOrNew context declHead kind gadtDecl deriving0) = GDataDecl (id l) (deStringDataOrNew dataOrNew) (fmap (deStringContext) context) (deStringDeclHead declHead) (fmap (deStringKind) kind) (fmap (deStringGadtDecl) gadtDecl) (fmap (deStringDeriving) deriving0)
deStringDecl (DataFamDecl l context declHead kind) = DataFamDecl (id l) (fmap (deStringContext) context) (deStringDeclHead declHead) (fmap (deStringKind) kind)
deStringDecl (TypeInsDecl l type1 type2) = TypeInsDecl (id l) (deStringType type1) (deStringType type2)
deStringDecl (DataInsDecl l dataOrNew type0 qualConDecl deriving0) = DataInsDecl (id l) (deStringDataOrNew dataOrNew) (deStringType type0) (fmap (deStringQualConDecl) qualConDecl) (fmap (deStringDeriving) deriving0)
deStringDecl (GDataInsDecl l dataOrNew type0 kind gadtDecl deriving0) = GDataInsDecl (id l) (deStringDataOrNew dataOrNew) (deStringType type0) (fmap (deStringKind) kind) (fmap (deStringGadtDecl) gadtDecl) (fmap (deStringDeriving) deriving0)
deStringDecl (ClassDecl l context declHead funDep classDecl) = ClassDecl (id l) (fmap (deStringContext) context) (deStringDeclHead declHead) (fmap (deStringFunDep) funDep) (fmap (fmap (deStringClassDecl)) classDecl)
deStringDecl (InstDecl l overlap instRule instDecl) = InstDecl (id l) (fmap (deStringOverlap) overlap) (deStringInstRule instRule) (fmap (fmap (deStringInstDecl)) instDecl)
deStringDecl (DerivDecl l overlap instRule) = DerivDecl (id l) (fmap (deStringOverlap) overlap) (deStringInstRule instRule)
deStringDecl (InfixDecl l assoc int op) = InfixDecl (id l) (deStringAssoc assoc) (fmap (id) int) (fmap (deStringOp) op)
deStringDecl (DefaultDecl l type0) = DefaultDecl (id l) (fmap (deStringType) type0)
deStringDecl (SpliceDecl l exp) = SpliceDecl (id l) (deStringExp exp)
deStringDecl (TypeSig l name type0) = TypeSig (id l) (fmap (deStringName) name) (deStringType type0)
deStringDecl (PatSynSig l name tyVarBind context1 context2 type0) = PatSynSig (id l) (deStringName name) (fmap (fmap (deStringTyVarBind)) tyVarBind) (fmap (deStringContext) context1) (fmap (deStringContext) context2) (deStringType type0)
deStringDecl (FunBind l match) = FunBind (id l) (fmap (deStringMatch) match)
deStringDecl (PatBind l pat rhs binds) = PatBind (id l) (deStringPat pat) (deStringRhs rhs) (fmap (deStringBinds) binds)
deStringDecl (PatSyn l pat1 pat2 patternSynDirection) = PatSyn (id l) (deStringPat pat1) (deStringPat pat2) (deStringPatternSynDirection patternSynDirection)
deStringDecl (ForImp l callConv safety string name type0) = ForImp (id l) (deStringCallConv callConv) (fmap (deStringSafety) safety) (fmap (id) string) (deStringName name) (deStringType type0)
deStringDecl (ForExp l callConv string name type0) = ForExp (id l) (deStringCallConv callConv) (fmap (id) string) (deStringName name) (deStringType type0)
deStringDecl (RulePragmaDecl l rule) = RulePragmaDecl (id l) (fmap (deStringRule) rule)
deStringDecl (DeprPragmaDecl l name) = DeprPragmaDecl (id l) (fmap (((fmap (deStringName)) *** (id))) name)
deStringDecl (WarnPragmaDecl l name) = WarnPragmaDecl (id l) (fmap (((fmap (deStringName)) *** (id))) name)
deStringDecl (InlineSig l bool activation qName) = InlineSig (id l) (id bool) (fmap (deStringActivation) activation) (deStringQName qName)
deStringDecl (InlineConlikeSig l activation qName) = InlineConlikeSig (id l) (fmap (deStringActivation) activation) (deStringQName qName)
deStringDecl (SpecSig l activation qName type0) = SpecSig (id l) (fmap (deStringActivation) activation) (deStringQName qName) (fmap (deStringType) type0)
deStringDecl (SpecInlineSig l bool activation qName type0) = SpecInlineSig (id l) (id bool) (fmap (deStringActivation) activation) (deStringQName qName) (fmap (deStringType) type0)
deStringDecl (InstSig l instRule) = InstSig (id l) (deStringInstRule instRule)
deStringDecl (AnnPragma l annotation) = AnnPragma (id l) (deStringAnnotation annotation)
deStringDecl (MinimalPragma l booleanFormula) = MinimalPragma (id l) (fmap (deStringBooleanFormula) booleanFormula)
deStringDecl (RoleAnnotDecl l qName role) = RoleAnnotDecl (id l) (deStringQName qName) (fmap (deStringRole) role)
deStringDeclHead :: DeclHead l -> DeclHead l
deStringDeclHead (DHead l name) = DHead (id l) (deStringName name)
deStringDeclHead (DHInfix l tyVarBind name) = DHInfix (id l) (deStringTyVarBind tyVarBind) (deStringName name)
deStringDeclHead (DHParen l declHead) = DHParen (id l) (deStringDeclHead declHead)
deStringDeclHead (DHApp l declHead tyVarBind) = DHApp (id l) (deStringDeclHead declHead) (deStringTyVarBind tyVarBind)
deStringDeriving :: Deriving l -> Deriving l
deStringDeriving (Deriving l instRule) = Deriving (id l) (fmap (deStringInstRule) instRule)
deStringExp :: Exp l -> Exp l
deStringExp (Var l qName) = Var (id l) (deStringQName qName)
deStringExp (IPVar l iPName) = IPVar (id l) (deStringIPName iPName)
deStringExp (Con l qName) = Con (id l) (deStringQName qName)
deStringExp (Lit l literal) = case literal of
  String l str _ -> deStringExp (List l (map (\ch -> Lit l (Char l ch (pure ch))) str))
  _ -> Lit (id l) (deStringLiteral literal)
deStringExp (InfixApp l exp1 qOp exp2) = InfixApp (id l) (deStringExp exp1) (deStringQOp qOp) (deStringExp exp2)
deStringExp (App l exp1 exp2) = App (id l) (deStringExp exp1) (deStringExp exp2)
deStringExp (NegApp l exp) = NegApp (id l) (deStringExp exp)
deStringExp (Lambda l pat exp) = Lambda (id l) (fmap (deStringPat) pat) (deStringExp exp)
deStringExp (Let l binds exp) = Let (id l) (deStringBinds binds) (deStringExp exp)
deStringExp (If l exp1 exp2 exp3) = If (id l) (deStringExp exp1) (deStringExp exp2) (deStringExp exp3)
deStringExp (MultiIf l guardedRhs) = MultiIf (id l) (fmap (deStringGuardedRhs) guardedRhs)
deStringExp (Case l exp alt) = Case (id l) (deStringExp exp) (fmap (deStringAlt) alt)
deStringExp (Do l stmt) = Do (id l) (fmap (deStringStmt) stmt)
deStringExp (MDo l stmt) = MDo (id l) (fmap (deStringStmt) stmt)
deStringExp (Tuple l boxed exp) = Tuple (id l) (deStringBoxed boxed) (fmap (deStringExp) exp)
deStringExp (TupleSection l boxed exp) = TupleSection (id l) (deStringBoxed boxed) (fmap (fmap (deStringExp)) exp)
deStringExp (List l exp) = List (id l) (fmap (deStringExp) exp)
deStringExp (ParArray l exp) = ParArray (id l) (fmap (deStringExp) exp)
deStringExp (Paren l exp) = Paren (id l) (deStringExp exp)
deStringExp (LeftSection l exp qOp) = LeftSection (id l) (deStringExp exp) (deStringQOp qOp)
deStringExp (RightSection l qOp exp) = RightSection (id l) (deStringQOp qOp) (deStringExp exp)
deStringExp (RecConstr l qName fieldUpdate) = RecConstr (id l) (deStringQName qName) (fmap (deStringFieldUpdate) fieldUpdate)
deStringExp (RecUpdate l exp fieldUpdate) = RecUpdate (id l) (deStringExp exp) (fmap (deStringFieldUpdate) fieldUpdate)
deStringExp (EnumFrom l exp) = EnumFrom (id l) (deStringExp exp)
deStringExp (EnumFromTo l exp1 exp2) = EnumFromTo (id l) (deStringExp exp1) (deStringExp exp2)
deStringExp (EnumFromThen l exp1 exp2) = EnumFromThen (id l) (deStringExp exp1) (deStringExp exp2)
deStringExp (EnumFromThenTo l exp1 exp2 exp3) = EnumFromThenTo (id l) (deStringExp exp1) (deStringExp exp2) (deStringExp exp3)
deStringExp (ParArrayFromTo l exp1 exp2) = ParArrayFromTo (id l) (deStringExp exp1) (deStringExp exp2)
deStringExp (ParArrayFromThenTo l exp1 exp2 exp3) = ParArrayFromThenTo (id l) (deStringExp exp1) (deStringExp exp2) (deStringExp exp3)
deStringExp (ListComp l exp qualStmt) = ListComp (id l) (deStringExp exp) (fmap (deStringQualStmt) qualStmt)
deStringExp (ParComp l exp qualStmt) = ParComp (id l) (deStringExp exp) (fmap (fmap (deStringQualStmt)) qualStmt)
deStringExp (ParArrayComp l exp qualStmt) = ParArrayComp (id l) (deStringExp exp) (fmap (fmap (deStringQualStmt)) qualStmt)
deStringExp (ExpTypeSig l exp type0) = ExpTypeSig (id l) (deStringExp exp) (deStringType type0)
deStringExp (VarQuote l qName) = VarQuote (id l) (deStringQName qName)
deStringExp (TypQuote l qName) = TypQuote (id l) (deStringQName qName)
deStringExp (BracketExp l bracket) = BracketExp (id l) (deStringBracket bracket)
deStringExp (SpliceExp l splice) = SpliceExp (id l) (deStringSplice splice)
deStringExp (QuasiQuote l string1 string2) = QuasiQuote (id l) (id string1) (id string2)
deStringExp (XTag l xName xAttr exp1 exp2) = XTag (id l) (deStringXName xName) (fmap (deStringXAttr) xAttr) (fmap (deStringExp) exp1) (fmap (deStringExp) exp2)
deStringExp (XETag l xName xAttr exp) = XETag (id l) (deStringXName xName) (fmap (deStringXAttr) xAttr) (fmap (deStringExp) exp)
deStringExp (XPcdata l string) = XPcdata (id l) (id string)
deStringExp (XExpTag l exp) = XExpTag (id l) (deStringExp exp)
deStringExp (XChildTag l exp) = XChildTag (id l) (fmap (deStringExp) exp)
deStringExp (CorePragma l string exp) = CorePragma (id l) (id string) (deStringExp exp)
deStringExp (SCCPragma l string exp) = SCCPragma (id l) (id string) (deStringExp exp)
deStringExp (GenPragma l string int1 int2 exp) = GenPragma (id l) (id string) (((id) *** (id)) int1) (((id) *** (id)) int2) (deStringExp exp)
deStringExp (Proc l pat exp) = Proc (id l) (deStringPat pat) (deStringExp exp)
deStringExp (LeftArrApp l exp1 exp2) = LeftArrApp (id l) (deStringExp exp1) (deStringExp exp2)
deStringExp (RightArrApp l exp1 exp2) = RightArrApp (id l) (deStringExp exp1) (deStringExp exp2)
deStringExp (LeftArrHighApp l exp1 exp2) = LeftArrHighApp (id l) (deStringExp exp1) (deStringExp exp2)
deStringExp (RightArrHighApp l exp1 exp2) = RightArrHighApp (id l) (deStringExp exp1) (deStringExp exp2)
deStringExp (LCase l alt) = LCase (id l) (fmap (deStringAlt) alt)
deStringExp (ExprHole l) = ExprHole (id l)
deStringExportSpec :: ExportSpec l -> ExportSpec l
deStringExportSpec (EVar l qName) = EVar (id l) (deStringQName qName)
deStringExportSpec (EAbs l namespace qName) = EAbs (id l) (deStringNamespace namespace) (deStringQName qName)
deStringExportSpec (EThingAll l qName) = EThingAll (id l) (deStringQName qName)
deStringExportSpec (EThingWith l qName cName) = EThingWith (id l) (deStringQName qName) (fmap (deStringCName) cName)
deStringExportSpec (EModuleContents l moduleName) = EModuleContents (id l) (deStringModuleName moduleName)
deStringExportSpecList :: ExportSpecList l -> ExportSpecList l
deStringExportSpecList (ExportSpecList l exportSpec) = ExportSpecList (id l) (fmap (deStringExportSpec) exportSpec)
deStringFieldDecl :: FieldDecl l -> FieldDecl l
deStringFieldDecl (FieldDecl l name type0) = FieldDecl (id l) (fmap (deStringName) name) (deStringType type0)
deStringFieldUpdate :: FieldUpdate l -> FieldUpdate l
deStringFieldUpdate (FieldUpdate l qName exp) = FieldUpdate (id l) (deStringQName qName) (deStringExp exp)
deStringFieldUpdate (FieldPun l qName) = FieldPun (id l) (deStringQName qName)
deStringFieldUpdate (FieldWildcard l) = FieldWildcard (id l)
deStringFunDep :: FunDep l -> FunDep l
deStringFunDep (FunDep l name1 name2) = FunDep (id l) (fmap (deStringName) name1) (fmap (deStringName) name2)
deStringGadtDecl :: GadtDecl l -> GadtDecl l
deStringGadtDecl (GadtDecl l name fieldDecl type0) = GadtDecl (id l) (deStringName name) (fmap (fmap (deStringFieldDecl)) fieldDecl) (deStringType type0)
deStringGuardedRhs :: GuardedRhs l -> GuardedRhs l
deStringGuardedRhs (GuardedRhs l stmt exp) = GuardedRhs (id l) (fmap (deStringStmt) stmt) (deStringExp exp)
deStringIPBind :: IPBind l -> IPBind l
deStringIPBind (IPBind l iPName exp) = IPBind (id l) (deStringIPName iPName) (deStringExp exp)
deStringIPName :: IPName l -> IPName l
deStringIPName (IPDup l string) = IPDup (id l) (id string)
deStringIPName (IPLin l string) = IPLin (id l) (id string)
deStringImportDecl :: ImportDecl l -> ImportDecl l
deStringImportDecl (ImportDecl importAnn importModule importQualified importSrc importSafe importPkg importAs importSpecs) = ImportDecl (id importAnn) (deStringModuleName importModule) (id importQualified) (id importSrc) (id importSafe) (fmap (id) importPkg) (fmap (deStringModuleName) importAs) (fmap (deStringImportSpecList) importSpecs)
deStringImportSpec :: ImportSpec l -> ImportSpec l
deStringImportSpec (IVar l name) = IVar (id l) (deStringName name)
deStringImportSpec (IAbs l namespace name) = IAbs (id l) (deStringNamespace namespace) (deStringName name)
deStringImportSpec (IThingAll l name) = IThingAll (id l) (deStringName name)
deStringImportSpec (IThingWith l name cName) = IThingWith (id l) (deStringName name) (fmap (deStringCName) cName)
deStringImportSpecList :: ImportSpecList l -> ImportSpecList l
deStringImportSpecList (ImportSpecList l bool importSpec) = ImportSpecList (id l) (id bool) (fmap (deStringImportSpec) importSpec)
deStringInstDecl :: InstDecl l -> InstDecl l
deStringInstDecl (InsDecl l decl) = InsDecl (id l) (deStringDecl decl)
deStringInstDecl (InsType l type1 type2) = InsType (id l) (deStringType type1) (deStringType type2)
deStringInstDecl (InsData l dataOrNew type0 qualConDecl deriving0) = InsData (id l) (deStringDataOrNew dataOrNew) (deStringType type0) (fmap (deStringQualConDecl) qualConDecl) (fmap (deStringDeriving) deriving0)
deStringInstDecl (InsGData l dataOrNew type0 kind gadtDecl deriving0) = InsGData (id l) (deStringDataOrNew dataOrNew) (deStringType type0) (fmap (deStringKind) kind) (fmap (deStringGadtDecl) gadtDecl) (fmap (deStringDeriving) deriving0)
deStringInstHead :: InstHead l -> InstHead l
deStringInstHead (IHCon l qName) = IHCon (id l) (deStringQName qName)
deStringInstHead (IHInfix l type0 qName) = IHInfix (id l) (deStringType type0) (deStringQName qName)
deStringInstHead (IHParen l instHead) = IHParen (id l) (deStringInstHead instHead)
deStringInstHead (IHApp l instHead type0) = IHApp (id l) (deStringInstHead instHead) (deStringType type0)
deStringInstRule :: InstRule l -> InstRule l
deStringInstRule (IRule l tyVarBind context instHead) = IRule (id l) (fmap (fmap (deStringTyVarBind)) tyVarBind) (fmap (deStringContext) context) (deStringInstHead instHead)
deStringInstRule (IParen l instRule) = IParen (id l) (deStringInstRule instRule)
deStringKind :: Kind l -> Kind l
deStringKind (KindStar l) = KindStar (id l)
deStringKind (KindFn l kind1 kind2) = KindFn (id l) (deStringKind kind1) (deStringKind kind2)
deStringKind (KindParen l kind) = KindParen (id l) (deStringKind kind)
deStringKind (KindVar l qName) = KindVar (id l) (deStringQName qName)
deStringKind (KindApp l kind1 kind2) = KindApp (id l) (deStringKind kind1) (deStringKind kind2)
deStringKind (KindTuple l kind) = KindTuple (id l) (fmap (deStringKind) kind)
deStringKind (KindList l kind) = KindList (id l) (deStringKind kind)
deStringLiteral :: Literal l -> Literal l
deStringLiteral (Char l char string) = Char (id l) (id char) (id string)
deStringLiteral (String l string1 string2) = String (id l) (id string1) (id string2)
deStringLiteral (Int l integer string) = Int (id l) (id integer) (id string)
deStringLiteral (Frac l rational string) = Frac (id l) (id rational) (id string)
deStringLiteral (PrimInt l integer string) = PrimInt (id l) (id integer) (id string)
deStringLiteral (PrimWord l integer string) = PrimWord (id l) (id integer) (id string)
deStringLiteral (PrimFloat l rational string) = PrimFloat (id l) (id rational) (id string)
deStringLiteral (PrimDouble l rational string) = PrimDouble (id l) (id rational) (id string)
deStringLiteral (PrimChar l char string) = PrimChar (id l) (id char) (id string)
deStringLiteral (PrimString l string1 string2) = PrimString (id l) (id string1) (id string2)
deStringMatch :: Match l -> Match l
deStringMatch (Match l name pat rhs binds) = Match (id l) (deStringName name) (fmap (deStringPat) pat) (deStringRhs rhs) (fmap (deStringBinds) binds)
deStringMatch (InfixMatch l pat1 name pat2 rhs binds) = InfixMatch (id l) (deStringPat pat1) (deStringName name) (fmap (deStringPat) pat2) (deStringRhs rhs) (fmap (deStringBinds) binds)
deStringModule :: Module l -> Module l
deStringModule (Module l moduleHead modulePragma importDecl decl) = Module (id l) (fmap (deStringModuleHead) moduleHead) (fmap (deStringModulePragma) modulePragma) (fmap (deStringImportDecl) importDecl) (fmap (deStringDecl) decl)
deStringModule (XmlPage l moduleName modulePragma xName xAttr exp1 exp2) = XmlPage (id l) (deStringModuleName moduleName) (fmap (deStringModulePragma) modulePragma) (deStringXName xName) (fmap (deStringXAttr) xAttr) (fmap (deStringExp) exp1) (fmap (deStringExp) exp2)
deStringModule (XmlHybrid l moduleHead modulePragma importDecl decl xName xAttr exp1 exp2) = XmlHybrid (id l) (fmap (deStringModuleHead) moduleHead) (fmap (deStringModulePragma) modulePragma) (fmap (deStringImportDecl) importDecl) (fmap (deStringDecl) decl) (deStringXName xName) (fmap (deStringXAttr) xAttr) (fmap (deStringExp) exp1) (fmap (deStringExp) exp2)
deStringModuleHead :: ModuleHead l -> ModuleHead l
deStringModuleHead (ModuleHead l moduleName warningText exportSpecList) = ModuleHead (id l) (deStringModuleName moduleName) (fmap (deStringWarningText) warningText) (fmap (deStringExportSpecList) exportSpecList)
deStringModuleName :: ModuleName l -> ModuleName l
deStringModuleName (ModuleName l string) = ModuleName (id l) (id string)
deStringModulePragma :: ModulePragma l -> ModulePragma l
deStringModulePragma (LanguagePragma l name) = LanguagePragma (id l) (fmap (deStringName) name)
deStringModulePragma (OptionsPragma l tool string) = OptionsPragma (id l) (fmap (deStringTool) tool) (id string)
deStringModulePragma (AnnModulePragma l annotation) = AnnModulePragma (id l) (deStringAnnotation annotation)
deStringName :: Name l -> Name l
deStringName (Ident l string) = Ident (id l) (id string)
deStringName (Symbol l string) = Symbol (id l) (id string)
deStringNamespace :: Namespace l -> Namespace l
deStringNamespace (NoNamespace l) = NoNamespace (id l)
deStringNamespace (TypeNamespace l) = TypeNamespace (id l)
deStringNamespace (PatternNamespace l) = PatternNamespace (id l)
deStringOp :: Op l -> Op l
deStringOp (VarOp l name) = VarOp (id l) (deStringName name)
deStringOp (ConOp l name) = ConOp (id l) (deStringName name)
deStringOverlap :: Overlap l -> Overlap l
deStringOverlap (NoOverlap l) = NoOverlap (id l)
deStringOverlap (Overlap l) = Overlap (id l)
deStringOverlap (Incoherent l) = Incoherent (id l)
deStringPXAttr :: PXAttr l -> PXAttr l
deStringPXAttr (PXAttr l xName pat) = PXAttr (id l) (deStringXName xName) (deStringPat pat)
deStringPat :: Pat l -> Pat l
deStringPat (PVar l name) = PVar (id l) (deStringName name)
deStringPat (PLit l sign literal) =
  case literal of
    String l str _ -> deStringPat (PList l (map (\ch -> PLit l sign (Char l ch (pure ch))) str))
    _ -> PLit (id l) (deStringSign sign) (deStringLiteral literal)
deStringPat (PNPlusK l name integer) = PNPlusK (id l) (deStringName name) (id integer)
deStringPat (PInfixApp l pat1 qName pat2) = PInfixApp (id l) (deStringPat pat1) (deStringQName qName) (deStringPat pat2)
deStringPat (PApp l qName pat) = PApp (id l) (deStringQName qName) (fmap (deStringPat) pat)
deStringPat (PTuple l boxed pat) = PTuple (id l) (deStringBoxed boxed) (fmap (deStringPat) pat)
deStringPat (PList l pat) = PList (id l) (fmap (deStringPat) pat)
deStringPat (PParen l pat) = PParen (id l) (deStringPat pat)
deStringPat (PRec l qName patField) = PRec (id l) (deStringQName qName) (fmap (deStringPatField) patField)
deStringPat (PAsPat l name pat) = PAsPat (id l) (deStringName name) (deStringPat pat)
deStringPat (PWildCard l) = PWildCard (id l)
deStringPat (PIrrPat l pat) = PIrrPat (id l) (deStringPat pat)
deStringPat (PatTypeSig l pat type0) = PatTypeSig (id l) (deStringPat pat) (deStringType type0)
deStringPat (PViewPat l exp pat) = PViewPat (id l) (deStringExp exp) (deStringPat pat)
deStringPat (PRPat l rPat) = PRPat (id l) (fmap (deStringRPat) rPat)
deStringPat (PXTag l xName pXAttr pat1 pat2) = PXTag (id l) (deStringXName xName) (fmap (deStringPXAttr) pXAttr) (fmap (deStringPat) pat1) (fmap (deStringPat) pat2)
deStringPat (PXETag l xName pXAttr pat) = PXETag (id l) (deStringXName xName) (fmap (deStringPXAttr) pXAttr) (fmap (deStringPat) pat)
deStringPat (PXPcdata l string) = PXPcdata (id l) (id string)
deStringPat (PXPatTag l pat) = PXPatTag (id l) (deStringPat pat)
deStringPat (PXRPats l rPat) = PXRPats (id l) (fmap (deStringRPat) rPat)
deStringPat (PQuasiQuote l string1 string2) = PQuasiQuote (id l) (id string1) (id string2)
deStringPat (PBangPat l pat) = PBangPat (id l) (deStringPat pat)
deStringPatField :: PatField l -> PatField l
deStringPatField (PFieldPat l qName pat) = PFieldPat (id l) (deStringQName qName) (deStringPat pat)
deStringPatField (PFieldPun l qName) = PFieldPun (id l) (deStringQName qName)
deStringPatField (PFieldWildcard l) = PFieldWildcard (id l)
deStringPatternSynDirection :: PatternSynDirection l -> PatternSynDirection l
deStringPatternSynDirection (Unidirectional) = Unidirectional
deStringPatternSynDirection (ImplicitBidirectional) = ImplicitBidirectional
deStringPatternSynDirection (ExplicitBidirectional l decl) = ExplicitBidirectional (id l) (fmap (deStringDecl) decl)
deStringPromoted :: Promoted l -> Promoted l
deStringPromoted (PromotedInteger l integer string) = PromotedInteger (id l) (id integer) (id string)
deStringPromoted (PromotedString l string1 string2) = PromotedString (id l) (id string1) (id string2)
deStringPromoted (PromotedCon l bool qName) = PromotedCon (id l) (id bool) (deStringQName qName)
deStringPromoted (PromotedList l bool type0) = PromotedList (id l) (id bool) (fmap (deStringType) type0)
deStringPromoted (PromotedTuple l type0) = PromotedTuple (id l) (fmap (deStringType) type0)
deStringPromoted (PromotedUnit l) = PromotedUnit (id l)
deStringQName :: QName l -> QName l
deStringQName (Qual l moduleName name) = Qual (id l) (deStringModuleName moduleName) (deStringName name)
deStringQName (UnQual l name) = UnQual (id l) (deStringName name)
deStringQName (Special l specialCon) = Special (id l) (deStringSpecialCon specialCon)
deStringQOp :: QOp l -> QOp l
deStringQOp (QVarOp l qName) = QVarOp (id l) (deStringQName qName)
deStringQOp (QConOp l qName) = QConOp (id l) (deStringQName qName)
deStringQualConDecl :: QualConDecl l -> QualConDecl l
deStringQualConDecl (QualConDecl l tyVarBind context conDecl) = QualConDecl (id l) (fmap (fmap (deStringTyVarBind)) tyVarBind) (fmap (deStringContext) context) (deStringConDecl conDecl)
deStringQualStmt :: QualStmt l -> QualStmt l
deStringQualStmt (QualStmt l stmt) = QualStmt (id l) (deStringStmt stmt)
deStringQualStmt (ThenTrans l exp) = ThenTrans (id l) (deStringExp exp)
deStringQualStmt (ThenBy l exp1 exp2) = ThenBy (id l) (deStringExp exp1) (deStringExp exp2)
deStringQualStmt (GroupBy l exp) = GroupBy (id l) (deStringExp exp)
deStringQualStmt (GroupUsing l exp) = GroupUsing (id l) (deStringExp exp)
deStringQualStmt (GroupByUsing l exp1 exp2) = GroupByUsing (id l) (deStringExp exp1) (deStringExp exp2)
deStringRPat :: RPat l -> RPat l
deStringRPat (RPOp l rPat rPatOp) = RPOp (id l) (deStringRPat rPat) (deStringRPatOp rPatOp)
deStringRPat (RPEither l rPat1 rPat2) = RPEither (id l) (deStringRPat rPat1) (deStringRPat rPat2)
deStringRPat (RPSeq l rPat) = RPSeq (id l) (fmap (deStringRPat) rPat)
deStringRPat (RPGuard l pat stmt) = RPGuard (id l) (deStringPat pat) (fmap (deStringStmt) stmt)
deStringRPat (RPCAs l name rPat) = RPCAs (id l) (deStringName name) (deStringRPat rPat)
deStringRPat (RPAs l name rPat) = RPAs (id l) (deStringName name) (deStringRPat rPat)
deStringRPat (RPParen l rPat) = RPParen (id l) (deStringRPat rPat)
deStringRPat (RPPat l pat) = RPPat (id l) (deStringPat pat)
deStringRPatOp :: RPatOp l -> RPatOp l
deStringRPatOp (RPStar l) = RPStar (id l)
deStringRPatOp (RPStarG l) = RPStarG (id l)
deStringRPatOp (RPPlus l) = RPPlus (id l)
deStringRPatOp (RPPlusG l) = RPPlusG (id l)
deStringRPatOp (RPOpt l) = RPOpt (id l)
deStringRPatOp (RPOptG l) = RPOptG (id l)
deStringRhs :: Rhs l -> Rhs l
deStringRhs (UnGuardedRhs l exp) = UnGuardedRhs (id l) (deStringExp exp)
deStringRhs (GuardedRhss l guardedRhs) = GuardedRhss (id l) (fmap (deStringGuardedRhs) guardedRhs)
deStringRole :: Role l -> Role l
deStringRole (Nominal l) = Nominal (id l)
deStringRole (Representational l) = Representational (id l)
deStringRole (Phantom l) = Phantom (id l)
deStringRole (RoleWildcard l) = RoleWildcard (id l)
deStringRule :: Rule l -> Rule l
deStringRule (Rule l string activation ruleVar exp1 exp2) = Rule (id l) (id string) (fmap (deStringActivation) activation) (fmap (fmap (deStringRuleVar)) ruleVar) (deStringExp exp1) (deStringExp exp2)
deStringRuleVar :: RuleVar l -> RuleVar l
deStringRuleVar (RuleVar l name) = RuleVar (id l) (deStringName name)
deStringRuleVar (TypedRuleVar l name type0) = TypedRuleVar (id l) (deStringName name) (deStringType type0)
deStringSafety :: Safety l -> Safety l
deStringSafety (PlayRisky l) = PlayRisky (id l)
deStringSafety (PlaySafe l bool) = PlaySafe (id l) (id bool)
deStringSafety (PlayInterruptible l) = PlayInterruptible (id l)
deStringSign :: Sign l -> Sign l
deStringSign (Signless l) = Signless (id l)
deStringSign (Negative l) = Negative (id l)
deStringSpecialCon :: SpecialCon l -> SpecialCon l
deStringSpecialCon (UnitCon l) = UnitCon (id l)
deStringSpecialCon (ListCon l) = ListCon (id l)
deStringSpecialCon (FunCon l) = FunCon (id l)
deStringSpecialCon (TupleCon l boxed int) = TupleCon (id l) (deStringBoxed boxed) (id int)
deStringSpecialCon (Cons l) = Cons (id l)
deStringSpecialCon (UnboxedSingleCon l) = UnboxedSingleCon (id l)
deStringSplice :: Splice l -> Splice l
deStringSplice (IdSplice l string) = IdSplice (id l) (id string)
deStringSplice (ParenSplice l exp) = ParenSplice (id l) (deStringExp exp)
deStringStmt :: Stmt l -> Stmt l
deStringStmt (Generator l pat exp) = Generator (id l) (deStringPat pat) (deStringExp exp)
deStringStmt (Qualifier l exp) = Qualifier (id l) (deStringExp exp)
deStringStmt (LetStmt l binds) = LetStmt (id l) (deStringBinds binds)
deStringStmt (RecStmt l stmt) = RecStmt (id l) (fmap (deStringStmt) stmt)
deStringTool :: Tool -> Tool
deStringTool (GHC) = GHC
deStringTool (HUGS) = HUGS
deStringTool (NHC98) = NHC98
deStringTool (YHC) = YHC
deStringTool (HADDOCK) = HADDOCK
deStringTool (UnknownTool string) = UnknownTool (id string)
deStringTyVarBind :: TyVarBind l -> TyVarBind l
deStringTyVarBind (KindedVar l name kind) = KindedVar (id l) (deStringName name) (deStringKind kind)
deStringTyVarBind (UnkindedVar l name) = UnkindedVar (id l) (deStringName name)
deStringType :: Type l -> Type l
deStringType (TyForall l tyVarBind context type0) = TyForall (id l) (fmap (fmap (deStringTyVarBind)) tyVarBind) (fmap (deStringContext) context) (deStringType type0)
deStringType (TyFun l type1 type2) = TyFun (id l) (deStringType type1) (deStringType type2)
deStringType (TyTuple l boxed type0) = TyTuple (id l) (deStringBoxed boxed) (fmap (deStringType) type0)
deStringType (TyList l type0) = TyList (id l) (deStringType type0)
deStringType (TyParArray l type0) = TyParArray (id l) (deStringType type0)
deStringType (TyApp l type1 type2) = TyApp (id l) (deStringType type1) (deStringType type2)
deStringType (TyVar l name) = TyVar (id l) (deStringName name)
deStringType (TyCon l qName) = TyCon (id l) (deStringQName qName)
deStringType (TyParen l type0) = TyParen (id l) (deStringType type0)
deStringType (TyInfix l type1 qName type2) = TyInfix (id l) (deStringType type1) (deStringQName qName) (deStringType type2)
deStringType (TyKind l type0 kind) = TyKind (id l) (deStringType type0) (deStringKind kind)
deStringType (TyPromoted l promoted) = TyPromoted (id l) (deStringPromoted promoted)
deStringType (TyEquals l type1 type2) = TyEquals (id l) (deStringType type1) (deStringType type2)
deStringType (TySplice l splice) = TySplice (id l) (deStringSplice splice)
deStringType (TyBang l bangType type0) = TyBang (id l) (deStringBangType bangType) (deStringType type0)
deStringType (TyWildCard l name) = TyWildCard (id l) (fmap (deStringName) name)
deStringTypeEqn :: TypeEqn l -> TypeEqn l
deStringTypeEqn (TypeEqn l type1 type2) = TypeEqn (id l) (deStringType type1) (deStringType type2)
deStringWarningText :: WarningText l -> WarningText l
deStringWarningText (DeprText l string) = DeprText (id l) (id string)
deStringWarningText (WarnText l string) = WarnText (id l) (id string)
deStringXAttr :: XAttr l -> XAttr l
deStringXAttr (XAttr l xName exp) = XAttr (id l) (deStringXName xName) (deStringExp exp)
deStringXName :: XName l -> XName l
deStringXName (XName l string) = XName (id l) (id string)
deStringXName (XDomName l string1 string2) = XDomName (id l) (id string1) (id string2)

