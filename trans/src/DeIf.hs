module DeIf where
import Language.Haskell.Exts.Syntax
import Control.Arrow ((***))
deIfBoxed :: Boxed -> Boxed
deIfBoxed (Boxed) = Boxed
deIfBoxed (Unboxed) = Unboxed
deIfTool :: Tool -> Tool
deIfTool (GHC) = GHC
deIfTool (HUGS) = HUGS
deIfTool (NHC98) = NHC98
deIfTool (YHC) = YHC
deIfTool (HADDOCK) = HADDOCK
deIfTool (UnknownTool string) = UnknownTool (id string)
deIfSrcLoc :: SrcLoc -> SrcLoc
deIfSrcLoc (SrcLoc srcFilename srcLine srcColumn) = SrcLoc (id srcFilename) (id srcLine) (id srcColumn)
deIfActivation :: Activation -> Activation
deIfActivation (AlwaysActive) = AlwaysActive
deIfActivation (ActiveFrom int) = ActiveFrom (id int)
deIfActivation (ActiveUntil int) = ActiveUntil (id int)
deIfAlt :: Alt -> Alt
deIfAlt (Alt srcLoc pat rhs binds) = Alt (deIfSrcLoc srcLoc) (deIfPat pat) (deIfRhs rhs) (fmap (deIfBinds) binds)
deIfAnnotation :: Annotation -> Annotation
deIfAnnotation (Ann name exp) = Ann (deIfName name) (deIfExp exp)
deIfAnnotation (TypeAnn name exp) = TypeAnn (deIfName name) (deIfExp exp)
deIfAnnotation (ModuleAnn exp) = ModuleAnn (deIfExp exp)
deIfAssoc :: Assoc -> Assoc
deIfAssoc (AssocNone) = AssocNone
deIfAssoc (AssocLeft) = AssocLeft
deIfAssoc (AssocRight) = AssocRight
deIfAsst :: Asst -> Asst
deIfAsst (ClassA qName type0) = ClassA (deIfQName qName) (fmap (deIfType) type0)
deIfAsst (AppA name type0) = AppA (deIfName name) (fmap (deIfType) type0)
deIfAsst (InfixA type1 qName type2) = InfixA (deIfType type1) (deIfQName qName) (deIfType type2)
deIfAsst (IParam iPName type0) = IParam (deIfIPName iPName) (deIfType type0)
deIfAsst (EqualP type1 type2) = EqualP (deIfType type1) (deIfType type2)
deIfAsst (ParenA asst) = ParenA (deIfAsst asst)
deIfAsst (WildCardA name) = WildCardA (fmap (deIfName) name)
deIfBangType :: BangType -> BangType
deIfBangType (BangedTy) = BangedTy
deIfBangType (UnpackedTy) = UnpackedTy
deIfBinds :: Binds -> Binds
deIfBinds (BDecls decl) = BDecls (fmap (deIfDecl) decl)
deIfBinds (IPBinds iPBind) = IPBinds (fmap (deIfIPBind) iPBind)
deIfBooleanFormula :: BooleanFormula -> BooleanFormula
deIfBooleanFormula (VarFormula name) = VarFormula (deIfName name)
deIfBooleanFormula (AndFormula booleanFormula) = AndFormula (fmap (deIfBooleanFormula) booleanFormula)
deIfBooleanFormula (OrFormula booleanFormula) = OrFormula (fmap (deIfBooleanFormula) booleanFormula)
deIfBooleanFormula (ParenFormula booleanFormula) = ParenFormula (deIfBooleanFormula booleanFormula)
deIfBracket :: Bracket -> Bracket
deIfBracket (ExpBracket exp) = ExpBracket (deIfExp exp)
deIfBracket (PatBracket pat) = PatBracket (deIfPat pat)
deIfBracket (TypeBracket type0) = TypeBracket (deIfType type0)
deIfBracket (DeclBracket decl) = DeclBracket (fmap (deIfDecl) decl)
deIfCName :: CName -> CName
deIfCName (VarName name) = VarName (deIfName name)
deIfCName (ConName name) = ConName (deIfName name)
deIfCallConv :: CallConv -> CallConv
deIfCallConv (StdCall) = StdCall
deIfCallConv (CCall) = CCall
deIfCallConv (CPlusPlus) = CPlusPlus
deIfCallConv (DotNet) = DotNet
deIfCallConv (Jvm) = Jvm
deIfCallConv (Js) = Js
deIfCallConv (JavaScript) = JavaScript
deIfCallConv (CApi) = CApi
deIfClassDecl :: ClassDecl -> ClassDecl
deIfClassDecl (ClsDecl decl) = ClsDecl (deIfDecl decl)
deIfClassDecl (ClsDataFam srcLoc context name tyVarBind kind) = ClsDataFam (deIfSrcLoc srcLoc) (deIfContext context) (deIfName name) (fmap (deIfTyVarBind) tyVarBind) (fmap (deIfKind) kind)
deIfClassDecl (ClsTyFam srcLoc name tyVarBind kind) = ClsTyFam (deIfSrcLoc srcLoc) (deIfName name) (fmap (deIfTyVarBind) tyVarBind) (fmap (deIfKind) kind)
deIfClassDecl (ClsTyDef srcLoc type1 type2) = ClsTyDef (deIfSrcLoc srcLoc) (deIfType type1) (deIfType type2)
deIfClassDecl (ClsDefSig srcLoc name type0) = ClsDefSig (deIfSrcLoc srcLoc) (deIfName name) (deIfType type0)
deIfConDecl :: ConDecl -> ConDecl
deIfConDecl (ConDecl name type0) = ConDecl (deIfName name) (fmap (deIfType) type0)
deIfConDecl (InfixConDecl type1 name type2) = InfixConDecl (deIfType type1) (deIfName name) (deIfType type2)
deIfConDecl (RecDecl name1 name2) = RecDecl (deIfName name1) (fmap (((fmap (deIfName)) *** (deIfType))) name2)
deIfContext :: Context -> Context
deIfContext a = (fmap (deIfAsst) a)
deIfDataOrNew :: DataOrNew -> DataOrNew
deIfDataOrNew (DataType) = DataType
deIfDataOrNew (NewType) = NewType
deIfDecl :: Decl -> Decl
deIfDecl (TypeDecl srcLoc name tyVarBind type0) = TypeDecl (deIfSrcLoc srcLoc) (deIfName name) (fmap (deIfTyVarBind) tyVarBind) (deIfType type0)
deIfDecl (TypeFamDecl srcLoc name tyVarBind kind) = TypeFamDecl (deIfSrcLoc srcLoc) (deIfName name) (fmap (deIfTyVarBind) tyVarBind) (fmap (deIfKind) kind)
deIfDecl (ClosedTypeFamDecl srcLoc name tyVarBind kind typeEqn) = ClosedTypeFamDecl (deIfSrcLoc srcLoc) (deIfName name) (fmap (deIfTyVarBind) tyVarBind) (fmap (deIfKind) kind) (fmap (deIfTypeEqn) typeEqn)
deIfDecl (DataDecl srcLoc dataOrNew context name tyVarBind qualConDecl deriving0) = DataDecl (deIfSrcLoc srcLoc) (deIfDataOrNew dataOrNew) (deIfContext context) (deIfName name) (fmap (deIfTyVarBind) tyVarBind) (fmap (deIfQualConDecl) qualConDecl) (fmap (deIfDeriving) deriving0)
deIfDecl (GDataDecl srcLoc dataOrNew context name tyVarBind kind gadtDecl deriving0) = GDataDecl (deIfSrcLoc srcLoc) (deIfDataOrNew dataOrNew) (deIfContext context) (deIfName name) (fmap (deIfTyVarBind) tyVarBind) (fmap (deIfKind) kind) (fmap (deIfGadtDecl) gadtDecl) (fmap (deIfDeriving) deriving0)
deIfDecl (DataFamDecl srcLoc context name tyVarBind kind) = DataFamDecl (deIfSrcLoc srcLoc) (deIfContext context) (deIfName name) (fmap (deIfTyVarBind) tyVarBind) (fmap (deIfKind) kind)
deIfDecl (TypeInsDecl srcLoc type1 type2) = TypeInsDecl (deIfSrcLoc srcLoc) (deIfType type1) (deIfType type2)
deIfDecl (DataInsDecl srcLoc dataOrNew type0 qualConDecl deriving0) = DataInsDecl (deIfSrcLoc srcLoc) (deIfDataOrNew dataOrNew) (deIfType type0) (fmap (deIfQualConDecl) qualConDecl) (fmap (deIfDeriving) deriving0)
deIfDecl (GDataInsDecl srcLoc dataOrNew type0 kind gadtDecl deriving0) = GDataInsDecl (deIfSrcLoc srcLoc) (deIfDataOrNew dataOrNew) (deIfType type0) (fmap (deIfKind) kind) (fmap (deIfGadtDecl) gadtDecl) (fmap (deIfDeriving) deriving0)
deIfDecl (ClassDecl srcLoc context name tyVarBind funDep classDecl) = ClassDecl (deIfSrcLoc srcLoc) (deIfContext context) (deIfName name) (fmap (deIfTyVarBind) tyVarBind) (fmap (deIfFunDep) funDep) (fmap (deIfClassDecl) classDecl)
deIfDecl (InstDecl srcLoc overlap tyVarBind context qName type0 instDecl) = InstDecl (deIfSrcLoc srcLoc) (fmap (deIfOverlap) overlap) (fmap (deIfTyVarBind) tyVarBind) (deIfContext context) (deIfQName qName) (fmap (deIfType) type0) (fmap (deIfInstDecl) instDecl)
deIfDecl (DerivDecl srcLoc overlap tyVarBind context qName type0) = DerivDecl (deIfSrcLoc srcLoc) (fmap (deIfOverlap) overlap) (fmap (deIfTyVarBind) tyVarBind) (deIfContext context) (deIfQName qName) (fmap (deIfType) type0)
deIfDecl (InfixDecl srcLoc assoc int op) = InfixDecl (deIfSrcLoc srcLoc) (deIfAssoc assoc) (id int) (fmap (deIfOp) op)
deIfDecl (DefaultDecl srcLoc type0) = DefaultDecl (deIfSrcLoc srcLoc) (fmap (deIfType) type0)
deIfDecl (SpliceDecl srcLoc exp) = SpliceDecl (deIfSrcLoc srcLoc) (deIfExp exp)
deIfDecl (TypeSig srcLoc name type0) = TypeSig (deIfSrcLoc srcLoc) (fmap (deIfName) name) (deIfType type0)
deIfDecl (PatSynSig srcLoc name tyVarBind context1 context2 type0) = PatSynSig (deIfSrcLoc srcLoc) (deIfName name) (fmap (fmap (deIfTyVarBind)) tyVarBind) (deIfContext context1) (deIfContext context2) (deIfType type0)
deIfDecl (FunBind match) = FunBind (fmap (deIfMatch) match)
deIfDecl (PatBind srcLoc pat rhs binds) = PatBind (deIfSrcLoc srcLoc) (deIfPat pat) (deIfRhs rhs) (fmap (deIfBinds) binds)
deIfDecl (ForImp srcLoc callConv safety string name type0) = ForImp (deIfSrcLoc srcLoc) (deIfCallConv callConv) (deIfSafety safety) (id string) (deIfName name) (deIfType type0)
deIfDecl (ForExp srcLoc callConv string name type0) = ForExp (deIfSrcLoc srcLoc) (deIfCallConv callConv) (id string) (deIfName name) (deIfType type0)
deIfDecl (PatSyn srcLoc pat1 pat2 patternSynDirection) = PatSyn (deIfSrcLoc srcLoc) (deIfPat pat1) (deIfPat pat2) (deIfPatternSynDirection patternSynDirection)
deIfDecl (RulePragmaDecl srcLoc rule) = RulePragmaDecl (deIfSrcLoc srcLoc) (fmap (deIfRule) rule)
deIfDecl (DeprPragmaDecl srcLoc name) = DeprPragmaDecl (deIfSrcLoc srcLoc) (fmap (((fmap (deIfName)) *** (id))) name)
deIfDecl (WarnPragmaDecl srcLoc name) = WarnPragmaDecl (deIfSrcLoc srcLoc) (fmap (((fmap (deIfName)) *** (id))) name)
deIfDecl (InlineSig srcLoc bool activation qName) = InlineSig (deIfSrcLoc srcLoc) (id bool) (deIfActivation activation) (deIfQName qName)
deIfDecl (InlineConlikeSig srcLoc activation qName) = InlineConlikeSig (deIfSrcLoc srcLoc) (deIfActivation activation) (deIfQName qName)
deIfDecl (SpecSig srcLoc activation qName type0) = SpecSig (deIfSrcLoc srcLoc) (deIfActivation activation) (deIfQName qName) (fmap (deIfType) type0)
deIfDecl (SpecInlineSig srcLoc bool activation qName type0) = SpecInlineSig (deIfSrcLoc srcLoc) (id bool) (deIfActivation activation) (deIfQName qName) (fmap (deIfType) type0)
deIfDecl (InstSig srcLoc tyVarBind context qName type0) = InstSig (deIfSrcLoc srcLoc) (fmap (deIfTyVarBind) tyVarBind) (deIfContext context) (deIfQName qName) (fmap (deIfType) type0)
deIfDecl (AnnPragma srcLoc annotation) = AnnPragma (deIfSrcLoc srcLoc) (deIfAnnotation annotation)
deIfDecl (MinimalPragma srcLoc booleanFormula) = MinimalPragma (deIfSrcLoc srcLoc) (fmap (deIfBooleanFormula) booleanFormula)
deIfDecl (RoleAnnotDecl srcLoc qName role) = RoleAnnotDecl (deIfSrcLoc srcLoc) (deIfQName qName) (fmap (deIfRole) role)
deIfDeriving :: Deriving -> Deriving
deIfDeriving a = (((deIfQName) *** (fmap (deIfType))) a)
deIfExp :: Exp -> Exp
deIfExp (Var qName) = Var (deIfQName qName)
deIfExp (IPVar iPName) = IPVar (deIfIPName iPName)
deIfExp (Con qName) = Con (deIfQName qName)
deIfExp (Lit literal) = Lit (deIfLiteral literal)
deIfExp (InfixApp exp1 qOp exp2) = InfixApp (deIfExp exp1) (deIfQOp qOp) (deIfExp exp2)
deIfExp (App exp1 exp2) = App (deIfExp exp1) (deIfExp exp2)
deIfExp (NegApp exp) = NegApp (deIfExp exp)
deIfExp (Lambda srcLoc pat exp) = Lambda (deIfSrcLoc srcLoc) (fmap (deIfPat) pat) (deIfExp exp)
deIfExp (Let binds exp) = Let (deIfBinds binds) (deIfExp exp)
deIfExp (If exp1 exp2 exp3) = deIfExp $
  Case exp1
    [ Alt (SrcLoc "" 0 0) (PApp (UnQual (Ident "False")) []) (UnGuardedRhs exp3) Nothing
    , Alt (SrcLoc "" 0 0) (PApp (UnQual (Ident "True")) []) (UnGuardedRhs exp2) Nothing
    ]
deIfExp (MultiIf guardedRhs) = MultiIf (fmap (deIfGuardedRhs) guardedRhs)
deIfExp (Case exp alt) = Case (deIfExp exp) (fmap (deIfAlt) alt)
deIfExp (Do stmt) = Do (fmap (deIfStmt) stmt)
deIfExp (MDo stmt) = MDo (fmap (deIfStmt) stmt)
deIfExp (Tuple boxed exp) = Tuple (deIfBoxed boxed) (fmap (deIfExp) exp)
deIfExp (TupleSection boxed exp) = TupleSection (deIfBoxed boxed) (fmap (fmap (deIfExp)) exp)
deIfExp (List exp) = List (fmap (deIfExp) exp)
deIfExp (ParArray exp) = ParArray (fmap (deIfExp) exp)
deIfExp (Paren exp) = Paren (deIfExp exp)
deIfExp (LeftSection exp qOp) = LeftSection (deIfExp exp) (deIfQOp qOp)
deIfExp (RightSection qOp exp) = RightSection (deIfQOp qOp) (deIfExp exp)
deIfExp (RecConstr qName fieldUpdate) = RecConstr (deIfQName qName) (fmap (deIfFieldUpdate) fieldUpdate)
deIfExp (RecUpdate exp fieldUpdate) = RecUpdate (deIfExp exp) (fmap (deIfFieldUpdate) fieldUpdate)
deIfExp (EnumFrom exp) = EnumFrom (deIfExp exp)
deIfExp (EnumFromTo exp1 exp2) = EnumFromTo (deIfExp exp1) (deIfExp exp2)
deIfExp (EnumFromThen exp1 exp2) = EnumFromThen (deIfExp exp1) (deIfExp exp2)
deIfExp (EnumFromThenTo exp1 exp2 exp3) = EnumFromThenTo (deIfExp exp1) (deIfExp exp2) (deIfExp exp3)
deIfExp (ParArrayFromTo exp1 exp2) = ParArrayFromTo (deIfExp exp1) (deIfExp exp2)
deIfExp (ParArrayFromThenTo exp1 exp2 exp3) = ParArrayFromThenTo (deIfExp exp1) (deIfExp exp2) (deIfExp exp3)
deIfExp (ListComp exp qualStmt) = ListComp (deIfExp exp) (fmap (deIfQualStmt) qualStmt)
deIfExp (ParComp exp qualStmt) = ParComp (deIfExp exp) (fmap (fmap (deIfQualStmt)) qualStmt)
deIfExp (ParArrayComp exp qualStmt) = ParArrayComp (deIfExp exp) (fmap (fmap (deIfQualStmt)) qualStmt)
deIfExp (ExpTypeSig srcLoc exp type0) = ExpTypeSig (deIfSrcLoc srcLoc) (deIfExp exp) (deIfType type0)
deIfExp (VarQuote qName) = VarQuote (deIfQName qName)
deIfExp (TypQuote qName) = TypQuote (deIfQName qName)
deIfExp (BracketExp bracket) = BracketExp (deIfBracket bracket)
deIfExp (SpliceExp splice) = SpliceExp (deIfSplice splice)
deIfExp (QuasiQuote string1 string2) = QuasiQuote (id string1) (id string2)
deIfExp (XTag srcLoc xName xAttr exp1 exp2) = XTag (deIfSrcLoc srcLoc) (deIfXName xName) (fmap (deIfXAttr) xAttr) (fmap (deIfExp) exp1) (fmap (deIfExp) exp2)
deIfExp (XETag srcLoc xName xAttr exp) = XETag (deIfSrcLoc srcLoc) (deIfXName xName) (fmap (deIfXAttr) xAttr) (fmap (deIfExp) exp)
deIfExp (XPcdata string) = XPcdata (id string)
deIfExp (XExpTag exp) = XExpTag (deIfExp exp)
deIfExp (XChildTag srcLoc exp) = XChildTag (deIfSrcLoc srcLoc) (fmap (deIfExp) exp)
deIfExp (CorePragma string exp) = CorePragma (id string) (deIfExp exp)
deIfExp (SCCPragma string exp) = SCCPragma (id string) (deIfExp exp)
deIfExp (GenPragma string int1 int2 exp) = GenPragma (id string) (((id) *** (id)) int1) (((id) *** (id)) int2) (deIfExp exp)
deIfExp (Proc srcLoc pat exp) = Proc (deIfSrcLoc srcLoc) (deIfPat pat) (deIfExp exp)
deIfExp (LeftArrApp exp1 exp2) = LeftArrApp (deIfExp exp1) (deIfExp exp2)
deIfExp (RightArrApp exp1 exp2) = RightArrApp (deIfExp exp1) (deIfExp exp2)
deIfExp (LeftArrHighApp exp1 exp2) = LeftArrHighApp (deIfExp exp1) (deIfExp exp2)
deIfExp (RightArrHighApp exp1 exp2) = RightArrHighApp (deIfExp exp1) (deIfExp exp2)
deIfExp (LCase alt) = LCase (fmap (deIfAlt) alt)
deIfExp (ExprHole) = ExprHole
deIfExportSpec :: ExportSpec -> ExportSpec
deIfExportSpec (EVar qName) = EVar (deIfQName qName)
deIfExportSpec (EAbs namespace qName) = EAbs (deIfNamespace namespace) (deIfQName qName)
deIfExportSpec (EThingAll qName) = EThingAll (deIfQName qName)
deIfExportSpec (EThingWith qName cName) = EThingWith (deIfQName qName) (fmap (deIfCName) cName)
deIfExportSpec (EModuleContents moduleName) = EModuleContents (deIfModuleName moduleName)
deIfFieldUpdate :: FieldUpdate -> FieldUpdate
deIfFieldUpdate (FieldUpdate qName exp) = FieldUpdate (deIfQName qName) (deIfExp exp)
deIfFieldUpdate (FieldPun qName) = FieldPun (deIfQName qName)
deIfFieldUpdate (FieldWildcard) = FieldWildcard
deIfFunDep :: FunDep -> FunDep
deIfFunDep (FunDep name1 name2) = FunDep (fmap (deIfName) name1) (fmap (deIfName) name2)
deIfGadtDecl :: GadtDecl -> GadtDecl
deIfGadtDecl (GadtDecl srcLoc name1 name2 type0) = GadtDecl (deIfSrcLoc srcLoc) (deIfName name1) (fmap (((fmap (deIfName)) *** (deIfType))) name2) (deIfType type0)
deIfGuardedRhs :: GuardedRhs -> GuardedRhs
deIfGuardedRhs (GuardedRhs srcLoc stmt exp) = GuardedRhs (deIfSrcLoc srcLoc) (fmap (deIfStmt) stmt) (deIfExp exp)
deIfIPBind :: IPBind -> IPBind
deIfIPBind (IPBind srcLoc iPName exp) = IPBind (deIfSrcLoc srcLoc) (deIfIPName iPName) (deIfExp exp)
deIfIPName :: IPName -> IPName
deIfIPName (IPDup string) = IPDup (id string)
deIfIPName (IPLin string) = IPLin (id string)
deIfImportDecl :: ImportDecl -> ImportDecl
deIfImportDecl (ImportDecl importLoc importModule importQualified importSrc importSafe importPkg importAs importSpecs) = ImportDecl (deIfSrcLoc importLoc) (deIfModuleName importModule) (id importQualified) (id importSrc) (id importSafe) (fmap (id) importPkg) (fmap (deIfModuleName) importAs) (fmap (((id) *** (fmap (deIfImportSpec)))) importSpecs)
deIfImportSpec :: ImportSpec -> ImportSpec
deIfImportSpec (IVar name) = IVar (deIfName name)
deIfImportSpec (IAbs namespace name) = IAbs (deIfNamespace namespace) (deIfName name)
deIfImportSpec (IThingAll name) = IThingAll (deIfName name)
deIfImportSpec (IThingWith name cName) = IThingWith (deIfName name) (fmap (deIfCName) cName)
deIfInstDecl :: InstDecl -> InstDecl
deIfInstDecl (InsDecl decl) = InsDecl (deIfDecl decl)
deIfInstDecl (InsType srcLoc type1 type2) = InsType (deIfSrcLoc srcLoc) (deIfType type1) (deIfType type2)
deIfInstDecl (InsData srcLoc dataOrNew type0 qualConDecl deriving0) = InsData (deIfSrcLoc srcLoc) (deIfDataOrNew dataOrNew) (deIfType type0) (fmap (deIfQualConDecl) qualConDecl) (fmap (deIfDeriving) deriving0)
deIfInstDecl (InsGData srcLoc dataOrNew type0 kind gadtDecl deriving0) = InsGData (deIfSrcLoc srcLoc) (deIfDataOrNew dataOrNew) (deIfType type0) (fmap (deIfKind) kind) (fmap (deIfGadtDecl) gadtDecl) (fmap (deIfDeriving) deriving0)
deIfKind :: Kind -> Kind
deIfKind (KindStar) = KindStar
deIfKind (KindFn kind1 kind2) = KindFn (deIfKind kind1) (deIfKind kind2)
deIfKind (KindParen kind) = KindParen (deIfKind kind)
deIfKind (KindVar qName) = KindVar (deIfQName qName)
deIfKind (KindApp kind1 kind2) = KindApp (deIfKind kind1) (deIfKind kind2)
deIfKind (KindTuple kind) = KindTuple (fmap (deIfKind) kind)
deIfKind (KindList kind) = KindList (deIfKind kind)
deIfLiteral :: Literal -> Literal
deIfLiteral (Char char) = Char (id char)
deIfLiteral (String string) = String (id string)
deIfLiteral (Int integer) = Int (id integer)
deIfLiteral (Frac rational) = Frac (id rational)
deIfLiteral (PrimInt integer) = PrimInt (id integer)
deIfLiteral (PrimWord integer) = PrimWord (id integer)
deIfLiteral (PrimFloat rational) = PrimFloat (id rational)
deIfLiteral (PrimDouble rational) = PrimDouble (id rational)
deIfLiteral (PrimChar char) = PrimChar (id char)
deIfLiteral (PrimString string) = PrimString (id string)
deIfMatch :: Match -> Match
deIfMatch (Match srcLoc name pat type0 rhs binds) = Match (deIfSrcLoc srcLoc) (deIfName name) (fmap (deIfPat) pat) (fmap (deIfType) type0) (deIfRhs rhs) (fmap (deIfBinds) binds)
deIfModule :: Module -> Module
deIfModule (Module srcLoc moduleName modulePragma warningText exportSpec importDecl decl) = Module (deIfSrcLoc srcLoc) (deIfModuleName moduleName) (fmap (deIfModulePragma) modulePragma) (fmap (deIfWarningText) warningText) (fmap (fmap (deIfExportSpec)) exportSpec) (fmap (deIfImportDecl) importDecl) (fmap (deIfDecl) decl)
deIfModuleName :: ModuleName -> ModuleName
deIfModuleName (ModuleName string) = ModuleName (id string)
deIfModulePragma :: ModulePragma -> ModulePragma
deIfModulePragma (LanguagePragma srcLoc name) = LanguagePragma (deIfSrcLoc srcLoc) (fmap (deIfName) name)
deIfModulePragma (OptionsPragma srcLoc tool string) = OptionsPragma (deIfSrcLoc srcLoc) (fmap (deIfTool) tool) (id string)
deIfModulePragma (AnnModulePragma srcLoc annotation) = AnnModulePragma (deIfSrcLoc srcLoc) (deIfAnnotation annotation)
deIfName :: Name -> Name
deIfName (Ident string) = Ident (id string)
deIfName (Symbol string) = Symbol (id string)
deIfNamespace :: Namespace -> Namespace
deIfNamespace (NoNamespace) = NoNamespace
deIfNamespace (TypeNamespace) = TypeNamespace
deIfNamespace (PatternNamespace) = PatternNamespace
deIfOp :: Op -> Op
deIfOp (VarOp name) = VarOp (deIfName name)
deIfOp (ConOp name) = ConOp (deIfName name)
deIfOverlap :: Overlap -> Overlap
deIfOverlap (NoOverlap) = NoOverlap
deIfOverlap (Overlap) = Overlap
deIfOverlap (Incoherent) = Incoherent
deIfPXAttr :: PXAttr -> PXAttr
deIfPXAttr (PXAttr xName pat) = PXAttr (deIfXName xName) (deIfPat pat)
deIfPat :: Pat -> Pat
deIfPat (PVar name) = PVar (deIfName name)
deIfPat (PLit sign literal) = PLit (deIfSign sign) (deIfLiteral literal)
deIfPat (PNPlusK name integer) = PNPlusK (deIfName name) (id integer)
deIfPat (PInfixApp pat1 qName pat2) = PInfixApp (deIfPat pat1) (deIfQName qName) (deIfPat pat2)
deIfPat (PApp qName pat) = PApp (deIfQName qName) (fmap (deIfPat) pat)
deIfPat (PTuple boxed pat) = PTuple (deIfBoxed boxed) (fmap (deIfPat) pat)
deIfPat (PList pat) = PList (fmap (deIfPat) pat)
deIfPat (PParen pat) = PParen (deIfPat pat)
deIfPat (PRec qName patField) = PRec (deIfQName qName) (fmap (deIfPatField) patField)
deIfPat (PAsPat name pat) = PAsPat (deIfName name) (deIfPat pat)
deIfPat (PWildCard) = PWildCard
deIfPat (PIrrPat pat) = PIrrPat (deIfPat pat)
deIfPat (PatTypeSig srcLoc pat type0) = PatTypeSig (deIfSrcLoc srcLoc) (deIfPat pat) (deIfType type0)
deIfPat (PViewPat exp pat) = PViewPat (deIfExp exp) (deIfPat pat)
deIfPat (PRPat rPat) = PRPat (fmap (deIfRPat) rPat)
deIfPat (PXTag srcLoc xName pXAttr pat1 pat2) = PXTag (deIfSrcLoc srcLoc) (deIfXName xName) (fmap (deIfPXAttr) pXAttr) (fmap (deIfPat) pat1) (fmap (deIfPat) pat2)
deIfPat (PXETag srcLoc xName pXAttr pat) = PXETag (deIfSrcLoc srcLoc) (deIfXName xName) (fmap (deIfPXAttr) pXAttr) (fmap (deIfPat) pat)
deIfPat (PXPcdata string) = PXPcdata (id string)
deIfPat (PXPatTag pat) = PXPatTag (deIfPat pat)
deIfPat (PXRPats rPat) = PXRPats (fmap (deIfRPat) rPat)
deIfPat (PQuasiQuote string1 string2) = PQuasiQuote (id string1) (id string2)
deIfPat (PBangPat pat) = PBangPat (deIfPat pat)
deIfPatField :: PatField -> PatField
deIfPatField (PFieldPat qName pat) = PFieldPat (deIfQName qName) (deIfPat pat)
deIfPatField (PFieldPun qName) = PFieldPun (deIfQName qName)
deIfPatField (PFieldWildcard) = PFieldWildcard
deIfPatternSynDirection :: PatternSynDirection -> PatternSynDirection
deIfPatternSynDirection (Unidirectional) = Unidirectional
deIfPatternSynDirection (ImplicitBidirectional) = ImplicitBidirectional
deIfPatternSynDirection (ExplicitBidirectional decl) = ExplicitBidirectional (fmap (deIfDecl) decl)
deIfPromoted :: Promoted -> Promoted
deIfPromoted (PromotedInteger integer) = PromotedInteger (id integer)
deIfPromoted (PromotedString string) = PromotedString (id string)
deIfPromoted (PromotedCon bool qName) = PromotedCon (id bool) (deIfQName qName)
deIfPromoted (PromotedList bool type0) = PromotedList (id bool) (fmap (deIfType) type0)
deIfPromoted (PromotedTuple type0) = PromotedTuple (fmap (deIfType) type0)
deIfPromoted (PromotedUnit) = PromotedUnit
deIfQName :: QName -> QName
deIfQName (Qual moduleName name) = Qual (deIfModuleName moduleName) (deIfName name)
deIfQName (UnQual name) = UnQual (deIfName name)
deIfQName (Special specialCon) = Special (deIfSpecialCon specialCon)
deIfQOp :: QOp -> QOp
deIfQOp (QVarOp qName) = QVarOp (deIfQName qName)
deIfQOp (QConOp qName) = QConOp (deIfQName qName)
deIfQualConDecl :: QualConDecl -> QualConDecl
deIfQualConDecl (QualConDecl srcLoc tyVarBind context conDecl) = QualConDecl (deIfSrcLoc srcLoc) (fmap (deIfTyVarBind) tyVarBind) (deIfContext context) (deIfConDecl conDecl)
deIfQualStmt :: QualStmt -> QualStmt
deIfQualStmt (QualStmt stmt) = QualStmt (deIfStmt stmt)
deIfQualStmt (ThenTrans exp) = ThenTrans (deIfExp exp)
deIfQualStmt (ThenBy exp1 exp2) = ThenBy (deIfExp exp1) (deIfExp exp2)
deIfQualStmt (GroupBy exp) = GroupBy (deIfExp exp)
deIfQualStmt (GroupUsing exp) = GroupUsing (deIfExp exp)
deIfQualStmt (GroupByUsing exp1 exp2) = GroupByUsing (deIfExp exp1) (deIfExp exp2)
deIfRPat :: RPat -> RPat
deIfRPat (RPOp rPat rPatOp) = RPOp (deIfRPat rPat) (deIfRPatOp rPatOp)
deIfRPat (RPEither rPat1 rPat2) = RPEither (deIfRPat rPat1) (deIfRPat rPat2)
deIfRPat (RPSeq rPat) = RPSeq (fmap (deIfRPat) rPat)
deIfRPat (RPGuard pat stmt) = RPGuard (deIfPat pat) (fmap (deIfStmt) stmt)
deIfRPat (RPCAs name rPat) = RPCAs (deIfName name) (deIfRPat rPat)
deIfRPat (RPAs name rPat) = RPAs (deIfName name) (deIfRPat rPat)
deIfRPat (RPParen rPat) = RPParen (deIfRPat rPat)
deIfRPat (RPPat pat) = RPPat (deIfPat pat)
deIfRPatOp :: RPatOp -> RPatOp
deIfRPatOp (RPStar) = RPStar
deIfRPatOp (RPStarG) = RPStarG
deIfRPatOp (RPPlus) = RPPlus
deIfRPatOp (RPPlusG) = RPPlusG
deIfRPatOp (RPOpt) = RPOpt
deIfRPatOp (RPOptG) = RPOptG
deIfRhs :: Rhs -> Rhs
deIfRhs (UnGuardedRhs exp) = UnGuardedRhs (deIfExp exp)
deIfRhs (GuardedRhss guardedRhs) = GuardedRhss (fmap (deIfGuardedRhs) guardedRhs)
deIfRole :: Role -> Role
deIfRole (Nominal) = Nominal
deIfRole (Representational) = Representational
deIfRole (Phantom) = Phantom
deIfRole (RoleWildcard) = RoleWildcard
deIfRule :: Rule -> Rule
deIfRule (Rule string activation ruleVar exp1 exp2) = Rule (id string) (deIfActivation activation) (fmap (fmap (deIfRuleVar)) ruleVar) (deIfExp exp1) (deIfExp exp2)
deIfRuleVar :: RuleVar -> RuleVar
deIfRuleVar (RuleVar name) = RuleVar (deIfName name)
deIfRuleVar (TypedRuleVar name type0) = TypedRuleVar (deIfName name) (deIfType type0)
deIfSafety :: Safety -> Safety
deIfSafety (PlayRisky) = PlayRisky
deIfSafety (PlaySafe bool) = PlaySafe (id bool)
deIfSafety (PlayInterruptible) = PlayInterruptible
deIfSign :: Sign -> Sign
deIfSign (Signless) = Signless
deIfSign (Negative) = Negative
deIfSpecialCon :: SpecialCon -> SpecialCon
deIfSpecialCon (UnitCon) = UnitCon
deIfSpecialCon (ListCon) = ListCon
deIfSpecialCon (FunCon) = FunCon
deIfSpecialCon (TupleCon boxed int) = TupleCon (deIfBoxed boxed) (id int)
deIfSpecialCon (Cons) = Cons
deIfSpecialCon (UnboxedSingleCon) = UnboxedSingleCon
deIfSplice :: Splice -> Splice
deIfSplice (IdSplice string) = IdSplice (id string)
deIfSplice (ParenSplice exp) = ParenSplice (deIfExp exp)
deIfStmt :: Stmt -> Stmt
deIfStmt (Generator srcLoc pat exp) = Generator (deIfSrcLoc srcLoc) (deIfPat pat) (deIfExp exp)
deIfStmt (Qualifier exp) = Qualifier (deIfExp exp)
deIfStmt (LetStmt binds) = LetStmt (deIfBinds binds)
deIfStmt (RecStmt stmt) = RecStmt (fmap (deIfStmt) stmt)
deIfTyVarBind :: TyVarBind -> TyVarBind
deIfTyVarBind (KindedVar name kind) = KindedVar (deIfName name) (deIfKind kind)
deIfTyVarBind (UnkindedVar name) = UnkindedVar (deIfName name)
deIfType :: Type -> Type
deIfType (TyForall tyVarBind context type0) = TyForall (fmap (fmap (deIfTyVarBind)) tyVarBind) (deIfContext context) (deIfType type0)
deIfType (TyFun type1 type2) = TyFun (deIfType type1) (deIfType type2)
deIfType (TyTuple boxed type0) = TyTuple (deIfBoxed boxed) (fmap (deIfType) type0)
deIfType (TyList type0) = TyList (deIfType type0)
deIfType (TyParArray type0) = TyParArray (deIfType type0)
deIfType (TyApp type1 type2) = TyApp (deIfType type1) (deIfType type2)
deIfType (TyVar name) = TyVar (deIfName name)
deIfType (TyCon qName) = TyCon (deIfQName qName)
deIfType (TyParen type0) = TyParen (deIfType type0)
deIfType (TyInfix type1 qName type2) = TyInfix (deIfType type1) (deIfQName qName) (deIfType type2)
deIfType (TyKind type0 kind) = TyKind (deIfType type0) (deIfKind kind)
deIfType (TyPromoted promoted) = TyPromoted (deIfPromoted promoted)
deIfType (TyEquals type1 type2) = TyEquals (deIfType type1) (deIfType type2)
deIfType (TySplice splice) = TySplice (deIfSplice splice)
deIfType (TyBang bangType type0) = TyBang (deIfBangType bangType) (deIfType type0)
deIfType (TyWildCard name) = TyWildCard (fmap (deIfName) name)
deIfTypeEqn :: TypeEqn -> TypeEqn
deIfTypeEqn (TypeEqn type1 type2) = TypeEqn (deIfType type1) (deIfType type2)
deIfWarningText :: WarningText -> WarningText
deIfWarningText (DeprText string) = DeprText (id string)
deIfWarningText (WarnText string) = WarnText (id string)
deIfXAttr :: XAttr -> XAttr
deIfXAttr (XAttr xName exp) = XAttr (deIfXName xName) (deIfExp exp)
deIfXName :: XName -> XName
deIfXName (XName string) = XName (id string)
deIfXName (XDomName string1 string2) = XDomName (id string1) (id string2)

