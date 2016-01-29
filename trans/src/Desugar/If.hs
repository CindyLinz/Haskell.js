module Desugar.If where
import Language.Haskell.Exts.Annotated.Syntax
import Control.Arrow ((***))
deIfActivation :: Activation l -> Activation l
deIfActivation (ActiveFrom l int) = ActiveFrom (id l) (id int)
deIfActivation (ActiveUntil l int) = ActiveUntil (id l) (id int)
deIfAlt :: Alt l -> Alt l
deIfAlt (Alt l pat rhs binds) = Alt (id l) (deIfPat pat) (deIfRhs rhs) (fmap (deIfBinds) binds)
deIfAnnotation :: Annotation l -> Annotation l
deIfAnnotation (Ann l name exp) = Ann (id l) (deIfName name) (deIfExp exp)
deIfAnnotation (TypeAnn l name exp) = TypeAnn (id l) (deIfName name) (deIfExp exp)
deIfAnnotation (ModuleAnn l exp) = ModuleAnn (id l) (deIfExp exp)
deIfAssoc :: Assoc l -> Assoc l
deIfAssoc (AssocNone l) = AssocNone (id l)
deIfAssoc (AssocLeft l) = AssocLeft (id l)
deIfAssoc (AssocRight l) = AssocRight (id l)
deIfAsst :: Asst l -> Asst l
deIfAsst (ClassA l qName type0) = ClassA (id l) (deIfQName qName) (fmap (deIfType) type0)
deIfAsst (AppA l name type0) = AppA (id l) (deIfName name) (fmap (deIfType) type0)
deIfAsst (InfixA l type1 qName type2) = InfixA (id l) (deIfType type1) (deIfQName qName) (deIfType type2)
deIfAsst (IParam l iPName type0) = IParam (id l) (deIfIPName iPName) (deIfType type0)
deIfAsst (EqualP l type1 type2) = EqualP (id l) (deIfType type1) (deIfType type2)
deIfAsst (ParenA l asst) = ParenA (id l) (deIfAsst asst)
deIfAsst (WildCardA l name) = WildCardA (id l) (fmap (deIfName) name)
deIfBangType :: BangType l -> BangType l
deIfBangType (BangedTy l) = BangedTy (id l)
deIfBangType (UnpackedTy l) = UnpackedTy (id l)
deIfBinds :: Binds l -> Binds l
deIfBinds (BDecls l decl) = BDecls (id l) (fmap (deIfDecl) decl)
deIfBinds (IPBinds l iPBind) = IPBinds (id l) (fmap (deIfIPBind) iPBind)
deIfBooleanFormula :: BooleanFormula l -> BooleanFormula l
deIfBooleanFormula (VarFormula l name) = VarFormula (id l) (deIfName name)
deIfBooleanFormula (AndFormula l booleanFormula) = AndFormula (id l) (fmap (deIfBooleanFormula) booleanFormula)
deIfBooleanFormula (OrFormula l booleanFormula) = OrFormula (id l) (fmap (deIfBooleanFormula) booleanFormula)
deIfBooleanFormula (ParenFormula l booleanFormula) = ParenFormula (id l) (deIfBooleanFormula booleanFormula)
deIfBoxed :: Boxed -> Boxed
deIfBoxed (Boxed) = Boxed
deIfBoxed (Unboxed) = Unboxed
deIfBracket :: Bracket l -> Bracket l
deIfBracket (ExpBracket l exp) = ExpBracket (id l) (deIfExp exp)
deIfBracket (PatBracket l pat) = PatBracket (id l) (deIfPat pat)
deIfBracket (TypeBracket l type0) = TypeBracket (id l) (deIfType type0)
deIfBracket (DeclBracket l decl) = DeclBracket (id l) (fmap (deIfDecl) decl)
deIfCName :: CName l -> CName l
deIfCName (VarName l name) = VarName (id l) (deIfName name)
deIfCName (ConName l name) = ConName (id l) (deIfName name)
deIfCallConv :: CallConv l -> CallConv l
deIfCallConv (StdCall l) = StdCall (id l)
deIfCallConv (CCall l) = CCall (id l)
deIfCallConv (CPlusPlus l) = CPlusPlus (id l)
deIfCallConv (DotNet l) = DotNet (id l)
deIfCallConv (Jvm l) = Jvm (id l)
deIfCallConv (Js l) = Js (id l)
deIfCallConv (JavaScript l) = JavaScript (id l)
deIfCallConv (CApi l) = CApi (id l)
deIfClassDecl :: ClassDecl l -> ClassDecl l
deIfClassDecl (ClsDecl l decl) = ClsDecl (id l) (deIfDecl decl)
deIfClassDecl (ClsDataFam l context declHead kind) = ClsDataFam (id l) (fmap (deIfContext) context) (deIfDeclHead declHead) (fmap (deIfKind) kind)
deIfClassDecl (ClsTyFam l declHead kind) = ClsTyFam (id l) (deIfDeclHead declHead) (fmap (deIfKind) kind)
deIfClassDecl (ClsTyDef l type1 type2) = ClsTyDef (id l) (deIfType type1) (deIfType type2)
deIfClassDecl (ClsDefSig l name type0) = ClsDefSig (id l) (deIfName name) (deIfType type0)
deIfConDecl :: ConDecl l -> ConDecl l
deIfConDecl (ConDecl l name type0) = ConDecl (id l) (deIfName name) (fmap (deIfType) type0)
deIfConDecl (InfixConDecl l type1 name type2) = InfixConDecl (id l) (deIfType type1) (deIfName name) (deIfType type2)
deIfConDecl (RecDecl l name fieldDecl) = RecDecl (id l) (deIfName name) (fmap (deIfFieldDecl) fieldDecl)
deIfContext :: Context l -> Context l
deIfContext (CxSingle l asst) = CxSingle (id l) (deIfAsst asst)
deIfContext (CxTuple l asst) = CxTuple (id l) (fmap (deIfAsst) asst)
deIfContext (CxEmpty l) = CxEmpty (id l)
deIfDataOrNew :: DataOrNew l -> DataOrNew l
deIfDataOrNew (DataType l) = DataType (id l)
deIfDataOrNew (NewType l) = NewType (id l)
deIfDecl :: Decl l -> Decl l
deIfDecl (TypeDecl l declHead type0) = TypeDecl (id l) (deIfDeclHead declHead) (deIfType type0)
deIfDecl (TypeFamDecl l declHead kind) = TypeFamDecl (id l) (deIfDeclHead declHead) (fmap (deIfKind) kind)
deIfDecl (ClosedTypeFamDecl l declHead kind typeEqn) = ClosedTypeFamDecl (id l) (deIfDeclHead declHead) (fmap (deIfKind) kind) (fmap (deIfTypeEqn) typeEqn)
deIfDecl (DataDecl l dataOrNew context declHead qualConDecl deriving0) = DataDecl (id l) (deIfDataOrNew dataOrNew) (fmap (deIfContext) context) (deIfDeclHead declHead) (fmap (deIfQualConDecl) qualConDecl) (fmap (deIfDeriving) deriving0)
deIfDecl (GDataDecl l dataOrNew context declHead kind gadtDecl deriving0) = GDataDecl (id l) (deIfDataOrNew dataOrNew) (fmap (deIfContext) context) (deIfDeclHead declHead) (fmap (deIfKind) kind) (fmap (deIfGadtDecl) gadtDecl) (fmap (deIfDeriving) deriving0)
deIfDecl (DataFamDecl l context declHead kind) = DataFamDecl (id l) (fmap (deIfContext) context) (deIfDeclHead declHead) (fmap (deIfKind) kind)
deIfDecl (TypeInsDecl l type1 type2) = TypeInsDecl (id l) (deIfType type1) (deIfType type2)
deIfDecl (DataInsDecl l dataOrNew type0 qualConDecl deriving0) = DataInsDecl (id l) (deIfDataOrNew dataOrNew) (deIfType type0) (fmap (deIfQualConDecl) qualConDecl) (fmap (deIfDeriving) deriving0)
deIfDecl (GDataInsDecl l dataOrNew type0 kind gadtDecl deriving0) = GDataInsDecl (id l) (deIfDataOrNew dataOrNew) (deIfType type0) (fmap (deIfKind) kind) (fmap (deIfGadtDecl) gadtDecl) (fmap (deIfDeriving) deriving0)
deIfDecl (ClassDecl l context declHead funDep classDecl) = ClassDecl (id l) (fmap (deIfContext) context) (deIfDeclHead declHead) (fmap (deIfFunDep) funDep) (fmap (fmap (deIfClassDecl)) classDecl)
deIfDecl (InstDecl l overlap instRule instDecl) = InstDecl (id l) (fmap (deIfOverlap) overlap) (deIfInstRule instRule) (fmap (fmap (deIfInstDecl)) instDecl)
deIfDecl (DerivDecl l overlap instRule) = DerivDecl (id l) (fmap (deIfOverlap) overlap) (deIfInstRule instRule)
deIfDecl (InfixDecl l assoc int op) = InfixDecl (id l) (deIfAssoc assoc) (fmap (id) int) (fmap (deIfOp) op)
deIfDecl (DefaultDecl l type0) = DefaultDecl (id l) (fmap (deIfType) type0)
deIfDecl (SpliceDecl l exp) = SpliceDecl (id l) (deIfExp exp)
deIfDecl (TypeSig l name type0) = TypeSig (id l) (fmap (deIfName) name) (deIfType type0)
deIfDecl (PatSynSig l name tyVarBind context1 context2 type0) = PatSynSig (id l) (deIfName name) (fmap (fmap (deIfTyVarBind)) tyVarBind) (fmap (deIfContext) context1) (fmap (deIfContext) context2) (deIfType type0)
deIfDecl (FunBind l match) = FunBind (id l) (fmap (deIfMatch) match)
deIfDecl (PatBind l pat rhs binds) = PatBind (id l) (deIfPat pat) (deIfRhs rhs) (fmap (deIfBinds) binds)
deIfDecl (PatSyn l pat1 pat2 patternSynDirection) = PatSyn (id l) (deIfPat pat1) (deIfPat pat2) (deIfPatternSynDirection patternSynDirection)
deIfDecl (ForImp l callConv safety string name type0) = ForImp (id l) (deIfCallConv callConv) (fmap (deIfSafety) safety) (fmap (id) string) (deIfName name) (deIfType type0)
deIfDecl (ForExp l callConv string name type0) = ForExp (id l) (deIfCallConv callConv) (fmap (id) string) (deIfName name) (deIfType type0)
deIfDecl (RulePragmaDecl l rule) = RulePragmaDecl (id l) (fmap (deIfRule) rule)
deIfDecl (DeprPragmaDecl l name) = DeprPragmaDecl (id l) (fmap (((fmap (deIfName)) *** (id))) name)
deIfDecl (WarnPragmaDecl l name) = WarnPragmaDecl (id l) (fmap (((fmap (deIfName)) *** (id))) name)
deIfDecl (InlineSig l bool activation qName) = InlineSig (id l) (id bool) (fmap (deIfActivation) activation) (deIfQName qName)
deIfDecl (InlineConlikeSig l activation qName) = InlineConlikeSig (id l) (fmap (deIfActivation) activation) (deIfQName qName)
deIfDecl (SpecSig l activation qName type0) = SpecSig (id l) (fmap (deIfActivation) activation) (deIfQName qName) (fmap (deIfType) type0)
deIfDecl (SpecInlineSig l bool activation qName type0) = SpecInlineSig (id l) (id bool) (fmap (deIfActivation) activation) (deIfQName qName) (fmap (deIfType) type0)
deIfDecl (InstSig l instRule) = InstSig (id l) (deIfInstRule instRule)
deIfDecl (AnnPragma l annotation) = AnnPragma (id l) (deIfAnnotation annotation)
deIfDecl (MinimalPragma l booleanFormula) = MinimalPragma (id l) (fmap (deIfBooleanFormula) booleanFormula)
deIfDecl (RoleAnnotDecl l qName role) = RoleAnnotDecl (id l) (deIfQName qName) (fmap (deIfRole) role)
deIfDeclHead :: DeclHead l -> DeclHead l
deIfDeclHead (DHead l name) = DHead (id l) (deIfName name)
deIfDeclHead (DHInfix l tyVarBind name) = DHInfix (id l) (deIfTyVarBind tyVarBind) (deIfName name)
deIfDeclHead (DHParen l declHead) = DHParen (id l) (deIfDeclHead declHead)
deIfDeclHead (DHApp l declHead tyVarBind) = DHApp (id l) (deIfDeclHead declHead) (deIfTyVarBind tyVarBind)
deIfDeriving :: Deriving l -> Deriving l
deIfDeriving (Deriving l instRule) = Deriving (id l) (fmap (deIfInstRule) instRule)
deIfExp :: Exp l -> Exp l
deIfExp (Var l qName) = Var (id l) (deIfQName qName)
deIfExp (IPVar l iPName) = IPVar (id l) (deIfIPName iPName)
deIfExp (Con l qName) = Con (id l) (deIfQName qName)
deIfExp (Lit l literal) = Lit (id l) (deIfLiteral literal)
deIfExp (InfixApp l exp1 qOp exp2) = InfixApp (id l) (deIfExp exp1) (deIfQOp qOp) (deIfExp exp2)
deIfExp (App l exp1 exp2) = App (id l) (deIfExp exp1) (deIfExp exp2)
deIfExp (NegApp l exp) = NegApp (id l) (deIfExp exp)
deIfExp (Lambda l pat exp) = Lambda (id l) (fmap (deIfPat) pat) (deIfExp exp)
deIfExp (Let l binds exp) = Let (id l) (deIfBinds binds) (deIfExp exp)
deIfExp (If l exp1 exp2 exp3) = deIfExp $
  Case l exp1
    [ Alt l (PApp l (UnQual l (Ident l "False")) []) (UnGuardedRhs l exp3) Nothing
    , Alt l (PApp l (UnQual l (Ident l "True")) []) (UnGuardedRhs l exp2) Nothing
    ]
deIfExp (MultiIf l guardedRhs) = MultiIf (id l) (fmap (deIfGuardedRhs) guardedRhs)
deIfExp (Case l exp alt) = Case (id l) (deIfExp exp) (fmap (deIfAlt) alt)
deIfExp (Do l stmt) = Do (id l) (fmap (deIfStmt) stmt)
deIfExp (MDo l stmt) = MDo (id l) (fmap (deIfStmt) stmt)
deIfExp (Tuple l boxed exp) = Tuple (id l) (deIfBoxed boxed) (fmap (deIfExp) exp)
deIfExp (TupleSection l boxed exp) = TupleSection (id l) (deIfBoxed boxed) (fmap (fmap (deIfExp)) exp)
deIfExp (List l exp) = List (id l) (fmap (deIfExp) exp)
deIfExp (ParArray l exp) = ParArray (id l) (fmap (deIfExp) exp)
deIfExp (Paren l exp) = Paren (id l) (deIfExp exp)
deIfExp (LeftSection l exp qOp) = LeftSection (id l) (deIfExp exp) (deIfQOp qOp)
deIfExp (RightSection l qOp exp) = RightSection (id l) (deIfQOp qOp) (deIfExp exp)
deIfExp (RecConstr l qName fieldUpdate) = RecConstr (id l) (deIfQName qName) (fmap (deIfFieldUpdate) fieldUpdate)
deIfExp (RecUpdate l exp fieldUpdate) = RecUpdate (id l) (deIfExp exp) (fmap (deIfFieldUpdate) fieldUpdate)
deIfExp (EnumFrom l exp) = EnumFrom (id l) (deIfExp exp)
deIfExp (EnumFromTo l exp1 exp2) = EnumFromTo (id l) (deIfExp exp1) (deIfExp exp2)
deIfExp (EnumFromThen l exp1 exp2) = EnumFromThen (id l) (deIfExp exp1) (deIfExp exp2)
deIfExp (EnumFromThenTo l exp1 exp2 exp3) = EnumFromThenTo (id l) (deIfExp exp1) (deIfExp exp2) (deIfExp exp3)
deIfExp (ParArrayFromTo l exp1 exp2) = ParArrayFromTo (id l) (deIfExp exp1) (deIfExp exp2)
deIfExp (ParArrayFromThenTo l exp1 exp2 exp3) = ParArrayFromThenTo (id l) (deIfExp exp1) (deIfExp exp2) (deIfExp exp3)
deIfExp (ListComp l exp qualStmt) = ListComp (id l) (deIfExp exp) (fmap (deIfQualStmt) qualStmt)
deIfExp (ParComp l exp qualStmt) = ParComp (id l) (deIfExp exp) (fmap (fmap (deIfQualStmt)) qualStmt)
deIfExp (ParArrayComp l exp qualStmt) = ParArrayComp (id l) (deIfExp exp) (fmap (fmap (deIfQualStmt)) qualStmt)
deIfExp (ExpTypeSig l exp type0) = ExpTypeSig (id l) (deIfExp exp) (deIfType type0)
deIfExp (VarQuote l qName) = VarQuote (id l) (deIfQName qName)
deIfExp (TypQuote l qName) = TypQuote (id l) (deIfQName qName)
deIfExp (BracketExp l bracket) = BracketExp (id l) (deIfBracket bracket)
deIfExp (SpliceExp l splice) = SpliceExp (id l) (deIfSplice splice)
deIfExp (QuasiQuote l string1 string2) = QuasiQuote (id l) (id string1) (id string2)
deIfExp (XTag l xName xAttr exp1 exp2) = XTag (id l) (deIfXName xName) (fmap (deIfXAttr) xAttr) (fmap (deIfExp) exp1) (fmap (deIfExp) exp2)
deIfExp (XETag l xName xAttr exp) = XETag (id l) (deIfXName xName) (fmap (deIfXAttr) xAttr) (fmap (deIfExp) exp)
deIfExp (XPcdata l string) = XPcdata (id l) (id string)
deIfExp (XExpTag l exp) = XExpTag (id l) (deIfExp exp)
deIfExp (XChildTag l exp) = XChildTag (id l) (fmap (deIfExp) exp)
deIfExp (CorePragma l string exp) = CorePragma (id l) (id string) (deIfExp exp)
deIfExp (SCCPragma l string exp) = SCCPragma (id l) (id string) (deIfExp exp)
deIfExp (GenPragma l string int1 int2 exp) = GenPragma (id l) (id string) (((id) *** (id)) int1) (((id) *** (id)) int2) (deIfExp exp)
deIfExp (Proc l pat exp) = Proc (id l) (deIfPat pat) (deIfExp exp)
deIfExp (LeftArrApp l exp1 exp2) = LeftArrApp (id l) (deIfExp exp1) (deIfExp exp2)
deIfExp (RightArrApp l exp1 exp2) = RightArrApp (id l) (deIfExp exp1) (deIfExp exp2)
deIfExp (LeftArrHighApp l exp1 exp2) = LeftArrHighApp (id l) (deIfExp exp1) (deIfExp exp2)
deIfExp (RightArrHighApp l exp1 exp2) = RightArrHighApp (id l) (deIfExp exp1) (deIfExp exp2)
deIfExp (LCase l alt) = LCase (id l) (fmap (deIfAlt) alt)
deIfExp (ExprHole l) = ExprHole (id l)
deIfExportSpec :: ExportSpec l -> ExportSpec l
deIfExportSpec (EVar l qName) = EVar (id l) (deIfQName qName)
deIfExportSpec (EAbs l namespace qName) = EAbs (id l) (deIfNamespace namespace) (deIfQName qName)
deIfExportSpec (EThingAll l qName) = EThingAll (id l) (deIfQName qName)
deIfExportSpec (EThingWith l qName cName) = EThingWith (id l) (deIfQName qName) (fmap (deIfCName) cName)
deIfExportSpec (EModuleContents l moduleName) = EModuleContents (id l) (deIfModuleName moduleName)
deIfExportSpecList :: ExportSpecList l -> ExportSpecList l
deIfExportSpecList (ExportSpecList l exportSpec) = ExportSpecList (id l) (fmap (deIfExportSpec) exportSpec)
deIfFieldDecl :: FieldDecl l -> FieldDecl l
deIfFieldDecl (FieldDecl l name type0) = FieldDecl (id l) (fmap (deIfName) name) (deIfType type0)
deIfFieldUpdate :: FieldUpdate l -> FieldUpdate l
deIfFieldUpdate (FieldUpdate l qName exp) = FieldUpdate (id l) (deIfQName qName) (deIfExp exp)
deIfFieldUpdate (FieldPun l qName) = FieldPun (id l) (deIfQName qName)
deIfFieldUpdate (FieldWildcard l) = FieldWildcard (id l)
deIfFunDep :: FunDep l -> FunDep l
deIfFunDep (FunDep l name1 name2) = FunDep (id l) (fmap (deIfName) name1) (fmap (deIfName) name2)
deIfGadtDecl :: GadtDecl l -> GadtDecl l
deIfGadtDecl (GadtDecl l name fieldDecl type0) = GadtDecl (id l) (deIfName name) (fmap (fmap (deIfFieldDecl)) fieldDecl) (deIfType type0)
deIfGuardedRhs :: GuardedRhs l -> GuardedRhs l
deIfGuardedRhs (GuardedRhs l stmt exp) = GuardedRhs (id l) (fmap (deIfStmt) stmt) (deIfExp exp)
deIfIPBind :: IPBind l -> IPBind l
deIfIPBind (IPBind l iPName exp) = IPBind (id l) (deIfIPName iPName) (deIfExp exp)
deIfIPName :: IPName l -> IPName l
deIfIPName (IPDup l string) = IPDup (id l) (id string)
deIfIPName (IPLin l string) = IPLin (id l) (id string)
deIfImportDecl :: ImportDecl l -> ImportDecl l
deIfImportDecl (ImportDecl importAnn importModule importQualified importSrc importSafe importPkg importAs importSpecs) = ImportDecl (id importAnn) (deIfModuleName importModule) (id importQualified) (id importSrc) (id importSafe) (fmap (id) importPkg) (fmap (deIfModuleName) importAs) (fmap (deIfImportSpecList) importSpecs)
deIfImportSpec :: ImportSpec l -> ImportSpec l
deIfImportSpec (IVar l name) = IVar (id l) (deIfName name)
deIfImportSpec (IAbs l namespace name) = IAbs (id l) (deIfNamespace namespace) (deIfName name)
deIfImportSpec (IThingAll l name) = IThingAll (id l) (deIfName name)
deIfImportSpec (IThingWith l name cName) = IThingWith (id l) (deIfName name) (fmap (deIfCName) cName)
deIfImportSpecList :: ImportSpecList l -> ImportSpecList l
deIfImportSpecList (ImportSpecList l bool importSpec) = ImportSpecList (id l) (id bool) (fmap (deIfImportSpec) importSpec)
deIfInstDecl :: InstDecl l -> InstDecl l
deIfInstDecl (InsDecl l decl) = InsDecl (id l) (deIfDecl decl)
deIfInstDecl (InsType l type1 type2) = InsType (id l) (deIfType type1) (deIfType type2)
deIfInstDecl (InsData l dataOrNew type0 qualConDecl deriving0) = InsData (id l) (deIfDataOrNew dataOrNew) (deIfType type0) (fmap (deIfQualConDecl) qualConDecl) (fmap (deIfDeriving) deriving0)
deIfInstDecl (InsGData l dataOrNew type0 kind gadtDecl deriving0) = InsGData (id l) (deIfDataOrNew dataOrNew) (deIfType type0) (fmap (deIfKind) kind) (fmap (deIfGadtDecl) gadtDecl) (fmap (deIfDeriving) deriving0)
deIfInstHead :: InstHead l -> InstHead l
deIfInstHead (IHCon l qName) = IHCon (id l) (deIfQName qName)
deIfInstHead (IHInfix l type0 qName) = IHInfix (id l) (deIfType type0) (deIfQName qName)
deIfInstHead (IHParen l instHead) = IHParen (id l) (deIfInstHead instHead)
deIfInstHead (IHApp l instHead type0) = IHApp (id l) (deIfInstHead instHead) (deIfType type0)
deIfInstRule :: InstRule l -> InstRule l
deIfInstRule (IRule l tyVarBind context instHead) = IRule (id l) (fmap (fmap (deIfTyVarBind)) tyVarBind) (fmap (deIfContext) context) (deIfInstHead instHead)
deIfInstRule (IParen l instRule) = IParen (id l) (deIfInstRule instRule)
deIfKind :: Kind l -> Kind l
deIfKind (KindStar l) = KindStar (id l)
deIfKind (KindFn l kind1 kind2) = KindFn (id l) (deIfKind kind1) (deIfKind kind2)
deIfKind (KindParen l kind) = KindParen (id l) (deIfKind kind)
deIfKind (KindVar l qName) = KindVar (id l) (deIfQName qName)
deIfKind (KindApp l kind1 kind2) = KindApp (id l) (deIfKind kind1) (deIfKind kind2)
deIfKind (KindTuple l kind) = KindTuple (id l) (fmap (deIfKind) kind)
deIfKind (KindList l kind) = KindList (id l) (deIfKind kind)
deIfLiteral :: Literal l -> Literal l
deIfLiteral (Char l char string) = Char (id l) (id char) (id string)
deIfLiteral (String l string1 string2) = String (id l) (id string1) (id string2)
deIfLiteral (Int l integer string) = Int (id l) (id integer) (id string)
deIfLiteral (Frac l rational string) = Frac (id l) (id rational) (id string)
deIfLiteral (PrimInt l integer string) = PrimInt (id l) (id integer) (id string)
deIfLiteral (PrimWord l integer string) = PrimWord (id l) (id integer) (id string)
deIfLiteral (PrimFloat l rational string) = PrimFloat (id l) (id rational) (id string)
deIfLiteral (PrimDouble l rational string) = PrimDouble (id l) (id rational) (id string)
deIfLiteral (PrimChar l char string) = PrimChar (id l) (id char) (id string)
deIfLiteral (PrimString l string1 string2) = PrimString (id l) (id string1) (id string2)
deIfMatch :: Match l -> Match l
deIfMatch (Match l name pat rhs binds) = Match (id l) (deIfName name) (fmap (deIfPat) pat) (deIfRhs rhs) (fmap (deIfBinds) binds)
deIfMatch (InfixMatch l pat1 name pat2 rhs binds) = InfixMatch (id l) (deIfPat pat1) (deIfName name) (fmap (deIfPat) pat2) (deIfRhs rhs) (fmap (deIfBinds) binds)
deIfModule :: Module l -> Module l
deIfModule (Module l moduleHead modulePragma importDecl decl) = Module (id l) (fmap (deIfModuleHead) moduleHead) (fmap (deIfModulePragma) modulePragma) (fmap (deIfImportDecl) importDecl) (fmap (deIfDecl) decl)
deIfModule (XmlPage l moduleName modulePragma xName xAttr exp1 exp2) = XmlPage (id l) (deIfModuleName moduleName) (fmap (deIfModulePragma) modulePragma) (deIfXName xName) (fmap (deIfXAttr) xAttr) (fmap (deIfExp) exp1) (fmap (deIfExp) exp2)
deIfModule (XmlHybrid l moduleHead modulePragma importDecl decl xName xAttr exp1 exp2) = XmlHybrid (id l) (fmap (deIfModuleHead) moduleHead) (fmap (deIfModulePragma) modulePragma) (fmap (deIfImportDecl) importDecl) (fmap (deIfDecl) decl) (deIfXName xName) (fmap (deIfXAttr) xAttr) (fmap (deIfExp) exp1) (fmap (deIfExp) exp2)
deIfModuleHead :: ModuleHead l -> ModuleHead l
deIfModuleHead (ModuleHead l moduleName warningText exportSpecList) = ModuleHead (id l) (deIfModuleName moduleName) (fmap (deIfWarningText) warningText) (fmap (deIfExportSpecList) exportSpecList)
deIfModuleName :: ModuleName l -> ModuleName l
deIfModuleName (ModuleName l string) = ModuleName (id l) (id string)
deIfModulePragma :: ModulePragma l -> ModulePragma l
deIfModulePragma (LanguagePragma l name) = LanguagePragma (id l) (fmap (deIfName) name)
deIfModulePragma (OptionsPragma l tool string) = OptionsPragma (id l) (fmap (deIfTool) tool) (id string)
deIfModulePragma (AnnModulePragma l annotation) = AnnModulePragma (id l) (deIfAnnotation annotation)
deIfName :: Name l -> Name l
deIfName (Ident l string) = Ident (id l) (id string)
deIfName (Symbol l string) = Symbol (id l) (id string)
deIfNamespace :: Namespace l -> Namespace l
deIfNamespace (NoNamespace l) = NoNamespace (id l)
deIfNamespace (TypeNamespace l) = TypeNamespace (id l)
deIfNamespace (PatternNamespace l) = PatternNamespace (id l)
deIfOp :: Op l -> Op l
deIfOp (VarOp l name) = VarOp (id l) (deIfName name)
deIfOp (ConOp l name) = ConOp (id l) (deIfName name)
deIfOverlap :: Overlap l -> Overlap l
deIfOverlap (NoOverlap l) = NoOverlap (id l)
deIfOverlap (Overlap l) = Overlap (id l)
deIfOverlap (Incoherent l) = Incoherent (id l)
deIfPXAttr :: PXAttr l -> PXAttr l
deIfPXAttr (PXAttr l xName pat) = PXAttr (id l) (deIfXName xName) (deIfPat pat)
deIfPat :: Pat l -> Pat l
deIfPat (PVar l name) = PVar (id l) (deIfName name)
deIfPat (PLit l sign literal) = PLit (id l) (deIfSign sign) (deIfLiteral literal)
deIfPat (PNPlusK l name integer) = PNPlusK (id l) (deIfName name) (id integer)
deIfPat (PInfixApp l pat1 qName pat2) = PInfixApp (id l) (deIfPat pat1) (deIfQName qName) (deIfPat pat2)
deIfPat (PApp l qName pat) = PApp (id l) (deIfQName qName) (fmap (deIfPat) pat)
deIfPat (PTuple l boxed pat) = PTuple (id l) (deIfBoxed boxed) (fmap (deIfPat) pat)
deIfPat (PList l pat) = PList (id l) (fmap (deIfPat) pat)
deIfPat (PParen l pat) = PParen (id l) (deIfPat pat)
deIfPat (PRec l qName patField) = PRec (id l) (deIfQName qName) (fmap (deIfPatField) patField)
deIfPat (PAsPat l name pat) = PAsPat (id l) (deIfName name) (deIfPat pat)
deIfPat (PWildCard l) = PWildCard (id l)
deIfPat (PIrrPat l pat) = PIrrPat (id l) (deIfPat pat)
deIfPat (PatTypeSig l pat type0) = PatTypeSig (id l) (deIfPat pat) (deIfType type0)
deIfPat (PViewPat l exp pat) = PViewPat (id l) (deIfExp exp) (deIfPat pat)
deIfPat (PRPat l rPat) = PRPat (id l) (fmap (deIfRPat) rPat)
deIfPat (PXTag l xName pXAttr pat1 pat2) = PXTag (id l) (deIfXName xName) (fmap (deIfPXAttr) pXAttr) (fmap (deIfPat) pat1) (fmap (deIfPat) pat2)
deIfPat (PXETag l xName pXAttr pat) = PXETag (id l) (deIfXName xName) (fmap (deIfPXAttr) pXAttr) (fmap (deIfPat) pat)
deIfPat (PXPcdata l string) = PXPcdata (id l) (id string)
deIfPat (PXPatTag l pat) = PXPatTag (id l) (deIfPat pat)
deIfPat (PXRPats l rPat) = PXRPats (id l) (fmap (deIfRPat) rPat)
deIfPat (PQuasiQuote l string1 string2) = PQuasiQuote (id l) (id string1) (id string2)
deIfPat (PBangPat l pat) = PBangPat (id l) (deIfPat pat)
deIfPatField :: PatField l -> PatField l
deIfPatField (PFieldPat l qName pat) = PFieldPat (id l) (deIfQName qName) (deIfPat pat)
deIfPatField (PFieldPun l qName) = PFieldPun (id l) (deIfQName qName)
deIfPatField (PFieldWildcard l) = PFieldWildcard (id l)
deIfPatternSynDirection :: PatternSynDirection l -> PatternSynDirection l
deIfPatternSynDirection (Unidirectional) = Unidirectional
deIfPatternSynDirection (ImplicitBidirectional) = ImplicitBidirectional
deIfPatternSynDirection (ExplicitBidirectional l decl) = ExplicitBidirectional (id l) (fmap (deIfDecl) decl)
deIfPromoted :: Promoted l -> Promoted l
deIfPromoted (PromotedInteger l integer string) = PromotedInteger (id l) (id integer) (id string)
deIfPromoted (PromotedString l string1 string2) = PromotedString (id l) (id string1) (id string2)
deIfPromoted (PromotedCon l bool qName) = PromotedCon (id l) (id bool) (deIfQName qName)
deIfPromoted (PromotedList l bool type0) = PromotedList (id l) (id bool) (fmap (deIfType) type0)
deIfPromoted (PromotedTuple l type0) = PromotedTuple (id l) (fmap (deIfType) type0)
deIfPromoted (PromotedUnit l) = PromotedUnit (id l)
deIfQName :: QName l -> QName l
deIfQName (Qual l moduleName name) = Qual (id l) (deIfModuleName moduleName) (deIfName name)
deIfQName (UnQual l name) = UnQual (id l) (deIfName name)
deIfQName (Special l specialCon) = Special (id l) (deIfSpecialCon specialCon)
deIfQOp :: QOp l -> QOp l
deIfQOp (QVarOp l qName) = QVarOp (id l) (deIfQName qName)
deIfQOp (QConOp l qName) = QConOp (id l) (deIfQName qName)
deIfQualConDecl :: QualConDecl l -> QualConDecl l
deIfQualConDecl (QualConDecl l tyVarBind context conDecl) = QualConDecl (id l) (fmap (fmap (deIfTyVarBind)) tyVarBind) (fmap (deIfContext) context) (deIfConDecl conDecl)
deIfQualStmt :: QualStmt l -> QualStmt l
deIfQualStmt (QualStmt l stmt) = QualStmt (id l) (deIfStmt stmt)
deIfQualStmt (ThenTrans l exp) = ThenTrans (id l) (deIfExp exp)
deIfQualStmt (ThenBy l exp1 exp2) = ThenBy (id l) (deIfExp exp1) (deIfExp exp2)
deIfQualStmt (GroupBy l exp) = GroupBy (id l) (deIfExp exp)
deIfQualStmt (GroupUsing l exp) = GroupUsing (id l) (deIfExp exp)
deIfQualStmt (GroupByUsing l exp1 exp2) = GroupByUsing (id l) (deIfExp exp1) (deIfExp exp2)
deIfRPat :: RPat l -> RPat l
deIfRPat (RPOp l rPat rPatOp) = RPOp (id l) (deIfRPat rPat) (deIfRPatOp rPatOp)
deIfRPat (RPEither l rPat1 rPat2) = RPEither (id l) (deIfRPat rPat1) (deIfRPat rPat2)
deIfRPat (RPSeq l rPat) = RPSeq (id l) (fmap (deIfRPat) rPat)
deIfRPat (RPGuard l pat stmt) = RPGuard (id l) (deIfPat pat) (fmap (deIfStmt) stmt)
deIfRPat (RPCAs l name rPat) = RPCAs (id l) (deIfName name) (deIfRPat rPat)
deIfRPat (RPAs l name rPat) = RPAs (id l) (deIfName name) (deIfRPat rPat)
deIfRPat (RPParen l rPat) = RPParen (id l) (deIfRPat rPat)
deIfRPat (RPPat l pat) = RPPat (id l) (deIfPat pat)
deIfRPatOp :: RPatOp l -> RPatOp l
deIfRPatOp (RPStar l) = RPStar (id l)
deIfRPatOp (RPStarG l) = RPStarG (id l)
deIfRPatOp (RPPlus l) = RPPlus (id l)
deIfRPatOp (RPPlusG l) = RPPlusG (id l)
deIfRPatOp (RPOpt l) = RPOpt (id l)
deIfRPatOp (RPOptG l) = RPOptG (id l)
deIfRhs :: Rhs l -> Rhs l
deIfRhs (UnGuardedRhs l exp) = UnGuardedRhs (id l) (deIfExp exp)
deIfRhs (GuardedRhss l guardedRhs) = GuardedRhss (id l) (fmap (deIfGuardedRhs) guardedRhs)
deIfRole :: Role l -> Role l
deIfRole (Nominal l) = Nominal (id l)
deIfRole (Representational l) = Representational (id l)
deIfRole (Phantom l) = Phantom (id l)
deIfRole (RoleWildcard l) = RoleWildcard (id l)
deIfRule :: Rule l -> Rule l
deIfRule (Rule l string activation ruleVar exp1 exp2) = Rule (id l) (id string) (fmap (deIfActivation) activation) (fmap (fmap (deIfRuleVar)) ruleVar) (deIfExp exp1) (deIfExp exp2)
deIfRuleVar :: RuleVar l -> RuleVar l
deIfRuleVar (RuleVar l name) = RuleVar (id l) (deIfName name)
deIfRuleVar (TypedRuleVar l name type0) = TypedRuleVar (id l) (deIfName name) (deIfType type0)
deIfSafety :: Safety l -> Safety l
deIfSafety (PlayRisky l) = PlayRisky (id l)
deIfSafety (PlaySafe l bool) = PlaySafe (id l) (id bool)
deIfSafety (PlayInterruptible l) = PlayInterruptible (id l)
deIfSign :: Sign l -> Sign l
deIfSign (Signless l) = Signless (id l)
deIfSign (Negative l) = Negative (id l)
deIfSpecialCon :: SpecialCon l -> SpecialCon l
deIfSpecialCon (UnitCon l) = UnitCon (id l)
deIfSpecialCon (ListCon l) = ListCon (id l)
deIfSpecialCon (FunCon l) = FunCon (id l)
deIfSpecialCon (TupleCon l boxed int) = TupleCon (id l) (deIfBoxed boxed) (id int)
deIfSpecialCon (Cons l) = Cons (id l)
deIfSpecialCon (UnboxedSingleCon l) = UnboxedSingleCon (id l)
deIfSplice :: Splice l -> Splice l
deIfSplice (IdSplice l string) = IdSplice (id l) (id string)
deIfSplice (ParenSplice l exp) = ParenSplice (id l) (deIfExp exp)
deIfStmt :: Stmt l -> Stmt l
deIfStmt (Generator l pat exp) = Generator (id l) (deIfPat pat) (deIfExp exp)
deIfStmt (Qualifier l exp) = Qualifier (id l) (deIfExp exp)
deIfStmt (LetStmt l binds) = LetStmt (id l) (deIfBinds binds)
deIfStmt (RecStmt l stmt) = RecStmt (id l) (fmap (deIfStmt) stmt)
deIfTool :: Tool -> Tool
deIfTool (GHC) = GHC
deIfTool (HUGS) = HUGS
deIfTool (NHC98) = NHC98
deIfTool (YHC) = YHC
deIfTool (HADDOCK) = HADDOCK
deIfTool (UnknownTool string) = UnknownTool (id string)
deIfTyVarBind :: TyVarBind l -> TyVarBind l
deIfTyVarBind (KindedVar l name kind) = KindedVar (id l) (deIfName name) (deIfKind kind)
deIfTyVarBind (UnkindedVar l name) = UnkindedVar (id l) (deIfName name)
deIfType :: Type l -> Type l
deIfType (TyForall l tyVarBind context type0) = TyForall (id l) (fmap (fmap (deIfTyVarBind)) tyVarBind) (fmap (deIfContext) context) (deIfType type0)
deIfType (TyFun l type1 type2) = TyFun (id l) (deIfType type1) (deIfType type2)
deIfType (TyTuple l boxed type0) = TyTuple (id l) (deIfBoxed boxed) (fmap (deIfType) type0)
deIfType (TyList l type0) = TyList (id l) (deIfType type0)
deIfType (TyParArray l type0) = TyParArray (id l) (deIfType type0)
deIfType (TyApp l type1 type2) = TyApp (id l) (deIfType type1) (deIfType type2)
deIfType (TyVar l name) = TyVar (id l) (deIfName name)
deIfType (TyCon l qName) = TyCon (id l) (deIfQName qName)
deIfType (TyParen l type0) = TyParen (id l) (deIfType type0)
deIfType (TyInfix l type1 qName type2) = TyInfix (id l) (deIfType type1) (deIfQName qName) (deIfType type2)
deIfType (TyKind l type0 kind) = TyKind (id l) (deIfType type0) (deIfKind kind)
deIfType (TyPromoted l promoted) = TyPromoted (id l) (deIfPromoted promoted)
deIfType (TyEquals l type1 type2) = TyEquals (id l) (deIfType type1) (deIfType type2)
deIfType (TySplice l splice) = TySplice (id l) (deIfSplice splice)
deIfType (TyBang l bangType type0) = TyBang (id l) (deIfBangType bangType) (deIfType type0)
deIfType (TyWildCard l name) = TyWildCard (id l) (fmap (deIfName) name)
deIfTypeEqn :: TypeEqn l -> TypeEqn l
deIfTypeEqn (TypeEqn l type1 type2) = TypeEqn (id l) (deIfType type1) (deIfType type2)
deIfWarningText :: WarningText l -> WarningText l
deIfWarningText (DeprText l string) = DeprText (id l) (id string)
deIfWarningText (WarnText l string) = WarnText (id l) (id string)
deIfXAttr :: XAttr l -> XAttr l
deIfXAttr (XAttr l xName exp) = XAttr (id l) (deIfXName xName) (deIfExp exp)
deIfXName :: XName l -> XName l
deIfXName (XName l string) = XName (id l) (id string)
deIfXName (XDomName l string1 string2) = XDomName (id l) (id string1) (id string2)

