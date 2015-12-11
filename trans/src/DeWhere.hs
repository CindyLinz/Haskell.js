module DeWhere where
import Language.Haskell.Exts.Syntax
import Control.Arrow ((***))
deWhereBoxed :: Boxed -> Boxed
deWhereBoxed (Boxed) = Boxed
deWhereBoxed (Unboxed) = Unboxed
deWhereTool :: Tool -> Tool
deWhereTool (GHC) = GHC
deWhereTool (HUGS) = HUGS
deWhereTool (NHC98) = NHC98
deWhereTool (YHC) = YHC
deWhereTool (HADDOCK) = HADDOCK
deWhereTool (UnknownTool string) = UnknownTool (id string)
deWhereSrcLoc :: SrcLoc -> SrcLoc
deWhereSrcLoc (SrcLoc srcFilename srcLine srcColumn) = SrcLoc (id srcFilename) (id srcLine) (id srcColumn)
deWhereActivation :: Activation -> Activation
deWhereActivation (AlwaysActive) = AlwaysActive
deWhereActivation (ActiveFrom int) = ActiveFrom (id int)
deWhereActivation (ActiveUntil int) = ActiveUntil (id int)
deWhereAlt :: Alt -> Alt
deWhereAlt (Alt srcLoc pat (UnGuardedRhs exp) (Just binds)) =
  deWhereAlt (Alt srcLoc pat (UnGuardedRhs (Let binds exp)) Nothing)
deWhereAlt (Alt srcLoc pat rhs binds) = Alt (deWhereSrcLoc srcLoc) (deWherePat pat) (deWhereRhs rhs) (fmap (deWhereBinds) binds)
deWhereAnnotation :: Annotation -> Annotation
deWhereAnnotation (Ann name exp) = Ann (deWhereName name) (deWhereExp exp)
deWhereAnnotation (TypeAnn name exp) = TypeAnn (deWhereName name) (deWhereExp exp)
deWhereAnnotation (ModuleAnn exp) = ModuleAnn (deWhereExp exp)
deWhereAssoc :: Assoc -> Assoc
deWhereAssoc (AssocNone) = AssocNone
deWhereAssoc (AssocLeft) = AssocLeft
deWhereAssoc (AssocRight) = AssocRight
deWhereAsst :: Asst -> Asst
deWhereAsst (ClassA qName type0) = ClassA (deWhereQName qName) (fmap (deWhereType) type0)
deWhereAsst (AppA name type0) = AppA (deWhereName name) (fmap (deWhereType) type0)
deWhereAsst (InfixA type1 qName type2) = InfixA (deWhereType type1) (deWhereQName qName) (deWhereType type2)
deWhereAsst (IParam iPName type0) = IParam (deWhereIPName iPName) (deWhereType type0)
deWhereAsst (EqualP type1 type2) = EqualP (deWhereType type1) (deWhereType type2)
deWhereAsst (ParenA asst) = ParenA (deWhereAsst asst)
deWhereAsst (WildCardA name) = WildCardA (fmap (deWhereName) name)
deWhereBangType :: BangType -> BangType
deWhereBangType (BangedTy) = BangedTy
deWhereBangType (UnpackedTy) = UnpackedTy
deWhereBinds :: Binds -> Binds
deWhereBinds (BDecls decl) = BDecls (fmap (deWhereDecl) decl)
deWhereBinds (IPBinds iPBind) = IPBinds (fmap (deWhereIPBind) iPBind)
deWhereBooleanFormula :: BooleanFormula -> BooleanFormula
deWhereBooleanFormula (VarFormula name) = VarFormula (deWhereName name)
deWhereBooleanFormula (AndFormula booleanFormula) = AndFormula (fmap (deWhereBooleanFormula) booleanFormula)
deWhereBooleanFormula (OrFormula booleanFormula) = OrFormula (fmap (deWhereBooleanFormula) booleanFormula)
deWhereBooleanFormula (ParenFormula booleanFormula) = ParenFormula (deWhereBooleanFormula booleanFormula)
deWhereBracket :: Bracket -> Bracket
deWhereBracket (ExpBracket exp) = ExpBracket (deWhereExp exp)
deWhereBracket (PatBracket pat) = PatBracket (deWherePat pat)
deWhereBracket (TypeBracket type0) = TypeBracket (deWhereType type0)
deWhereBracket (DeclBracket decl) = DeclBracket (fmap (deWhereDecl) decl)
deWhereCName :: CName -> CName
deWhereCName (VarName name) = VarName (deWhereName name)
deWhereCName (ConName name) = ConName (deWhereName name)
deWhereCallConv :: CallConv -> CallConv
deWhereCallConv (StdCall) = StdCall
deWhereCallConv (CCall) = CCall
deWhereCallConv (CPlusPlus) = CPlusPlus
deWhereCallConv (DotNet) = DotNet
deWhereCallConv (Jvm) = Jvm
deWhereCallConv (Js) = Js
deWhereCallConv (JavaScript) = JavaScript
deWhereCallConv (CApi) = CApi
deWhereClassDecl :: ClassDecl -> ClassDecl
deWhereClassDecl (ClsDecl decl) = ClsDecl (deWhereDecl decl)
deWhereClassDecl (ClsDataFam srcLoc context name tyVarBind kind) = ClsDataFam (deWhereSrcLoc srcLoc) (deWhereContext context) (deWhereName name) (fmap (deWhereTyVarBind) tyVarBind) (fmap (deWhereKind) kind)
deWhereClassDecl (ClsTyFam srcLoc name tyVarBind kind) = ClsTyFam (deWhereSrcLoc srcLoc) (deWhereName name) (fmap (deWhereTyVarBind) tyVarBind) (fmap (deWhereKind) kind)
deWhereClassDecl (ClsTyDef srcLoc type1 type2) = ClsTyDef (deWhereSrcLoc srcLoc) (deWhereType type1) (deWhereType type2)
deWhereClassDecl (ClsDefSig srcLoc name type0) = ClsDefSig (deWhereSrcLoc srcLoc) (deWhereName name) (deWhereType type0)
deWhereConDecl :: ConDecl -> ConDecl
deWhereConDecl (ConDecl name type0) = ConDecl (deWhereName name) (fmap (deWhereType) type0)
deWhereConDecl (InfixConDecl type1 name type2) = InfixConDecl (deWhereType type1) (deWhereName name) (deWhereType type2)
deWhereConDecl (RecDecl name1 name2) = RecDecl (deWhereName name1) (fmap (((fmap (deWhereName)) *** (deWhereType))) name2)
deWhereContext :: Context -> Context
deWhereContext a = (fmap (deWhereAsst) a)
deWhereDataOrNew :: DataOrNew -> DataOrNew
deWhereDataOrNew (DataType) = DataType
deWhereDataOrNew (NewType) = NewType
deWhereDecl :: Decl -> Decl
deWhereDecl (TypeDecl srcLoc name tyVarBind type0) = TypeDecl (deWhereSrcLoc srcLoc) (deWhereName name) (fmap (deWhereTyVarBind) tyVarBind) (deWhereType type0)
deWhereDecl (TypeFamDecl srcLoc name tyVarBind kind) = TypeFamDecl (deWhereSrcLoc srcLoc) (deWhereName name) (fmap (deWhereTyVarBind) tyVarBind) (fmap (deWhereKind) kind)
deWhereDecl (ClosedTypeFamDecl srcLoc name tyVarBind kind typeEqn) = ClosedTypeFamDecl (deWhereSrcLoc srcLoc) (deWhereName name) (fmap (deWhereTyVarBind) tyVarBind) (fmap (deWhereKind) kind) (fmap (deWhereTypeEqn) typeEqn)
deWhereDecl (DataDecl srcLoc dataOrNew context name tyVarBind qualConDecl deriving0) = DataDecl (deWhereSrcLoc srcLoc) (deWhereDataOrNew dataOrNew) (deWhereContext context) (deWhereName name) (fmap (deWhereTyVarBind) tyVarBind) (fmap (deWhereQualConDecl) qualConDecl) (fmap (deWhereDeriving) deriving0)
deWhereDecl (GDataDecl srcLoc dataOrNew context name tyVarBind kind gadtDecl deriving0) = GDataDecl (deWhereSrcLoc srcLoc) (deWhereDataOrNew dataOrNew) (deWhereContext context) (deWhereName name) (fmap (deWhereTyVarBind) tyVarBind) (fmap (deWhereKind) kind) (fmap (deWhereGadtDecl) gadtDecl) (fmap (deWhereDeriving) deriving0)
deWhereDecl (DataFamDecl srcLoc context name tyVarBind kind) = DataFamDecl (deWhereSrcLoc srcLoc) (deWhereContext context) (deWhereName name) (fmap (deWhereTyVarBind) tyVarBind) (fmap (deWhereKind) kind)
deWhereDecl (TypeInsDecl srcLoc type1 type2) = TypeInsDecl (deWhereSrcLoc srcLoc) (deWhereType type1) (deWhereType type2)
deWhereDecl (DataInsDecl srcLoc dataOrNew type0 qualConDecl deriving0) = DataInsDecl (deWhereSrcLoc srcLoc) (deWhereDataOrNew dataOrNew) (deWhereType type0) (fmap (deWhereQualConDecl) qualConDecl) (fmap (deWhereDeriving) deriving0)
deWhereDecl (GDataInsDecl srcLoc dataOrNew type0 kind gadtDecl deriving0) = GDataInsDecl (deWhereSrcLoc srcLoc) (deWhereDataOrNew dataOrNew) (deWhereType type0) (fmap (deWhereKind) kind) (fmap (deWhereGadtDecl) gadtDecl) (fmap (deWhereDeriving) deriving0)
deWhereDecl (ClassDecl srcLoc context name tyVarBind funDep classDecl) = ClassDecl (deWhereSrcLoc srcLoc) (deWhereContext context) (deWhereName name) (fmap (deWhereTyVarBind) tyVarBind) (fmap (deWhereFunDep) funDep) (fmap (deWhereClassDecl) classDecl)
deWhereDecl (InstDecl srcLoc overlap tyVarBind context qName type0 instDecl) = InstDecl (deWhereSrcLoc srcLoc) (fmap (deWhereOverlap) overlap) (fmap (deWhereTyVarBind) tyVarBind) (deWhereContext context) (deWhereQName qName) (fmap (deWhereType) type0) (fmap (deWhereInstDecl) instDecl)
deWhereDecl (DerivDecl srcLoc overlap tyVarBind context qName type0) = DerivDecl (deWhereSrcLoc srcLoc) (fmap (deWhereOverlap) overlap) (fmap (deWhereTyVarBind) tyVarBind) (deWhereContext context) (deWhereQName qName) (fmap (deWhereType) type0)
deWhereDecl (InfixDecl srcLoc assoc int op) = InfixDecl (deWhereSrcLoc srcLoc) (deWhereAssoc assoc) (id int) (fmap (deWhereOp) op)
deWhereDecl (DefaultDecl srcLoc type0) = DefaultDecl (deWhereSrcLoc srcLoc) (fmap (deWhereType) type0)
deWhereDecl (SpliceDecl srcLoc exp) = SpliceDecl (deWhereSrcLoc srcLoc) (deWhereExp exp)
deWhereDecl (TypeSig srcLoc name type0) = TypeSig (deWhereSrcLoc srcLoc) (fmap (deWhereName) name) (deWhereType type0)
deWhereDecl (PatSynSig srcLoc name tyVarBind context1 context2 type0) = PatSynSig (deWhereSrcLoc srcLoc) (deWhereName name) (fmap (fmap (deWhereTyVarBind)) tyVarBind) (deWhereContext context1) (deWhereContext context2) (deWhereType type0)
deWhereDecl (FunBind match) = FunBind (fmap (deWhereMatch) match)
deWhereDecl (PatBind srcLoc pat (UnGuardedRhs exp) (Just binds)) =
  deWhereDecl (PatBind srcLoc pat (UnGuardedRhs (Let binds exp)) Nothing)
deWhereDecl (PatBind srcLoc pat rhs binds) = PatBind (deWhereSrcLoc srcLoc) (deWherePat pat) (deWhereRhs rhs) (fmap (deWhereBinds) binds)
deWhereDecl (ForImp srcLoc callConv safety string name type0) = ForImp (deWhereSrcLoc srcLoc) (deWhereCallConv callConv) (deWhereSafety safety) (id string) (deWhereName name) (deWhereType type0)
deWhereDecl (ForExp srcLoc callConv string name type0) = ForExp (deWhereSrcLoc srcLoc) (deWhereCallConv callConv) (id string) (deWhereName name) (deWhereType type0)
deWhereDecl (PatSyn srcLoc pat1 pat2 patternSynDirection) = PatSyn (deWhereSrcLoc srcLoc) (deWherePat pat1) (deWherePat pat2) (deWherePatternSynDirection patternSynDirection)
deWhereDecl (RulePragmaDecl srcLoc rule) = RulePragmaDecl (deWhereSrcLoc srcLoc) (fmap (deWhereRule) rule)
deWhereDecl (DeprPragmaDecl srcLoc name) = DeprPragmaDecl (deWhereSrcLoc srcLoc) (fmap (((fmap (deWhereName)) *** (id))) name)
deWhereDecl (WarnPragmaDecl srcLoc name) = WarnPragmaDecl (deWhereSrcLoc srcLoc) (fmap (((fmap (deWhereName)) *** (id))) name)
deWhereDecl (InlineSig srcLoc bool activation qName) = InlineSig (deWhereSrcLoc srcLoc) (id bool) (deWhereActivation activation) (deWhereQName qName)
deWhereDecl (InlineConlikeSig srcLoc activation qName) = InlineConlikeSig (deWhereSrcLoc srcLoc) (deWhereActivation activation) (deWhereQName qName)
deWhereDecl (SpecSig srcLoc activation qName type0) = SpecSig (deWhereSrcLoc srcLoc) (deWhereActivation activation) (deWhereQName qName) (fmap (deWhereType) type0)
deWhereDecl (SpecInlineSig srcLoc bool activation qName type0) = SpecInlineSig (deWhereSrcLoc srcLoc) (id bool) (deWhereActivation activation) (deWhereQName qName) (fmap (deWhereType) type0)
deWhereDecl (InstSig srcLoc tyVarBind context qName type0) = InstSig (deWhereSrcLoc srcLoc) (fmap (deWhereTyVarBind) tyVarBind) (deWhereContext context) (deWhereQName qName) (fmap (deWhereType) type0)
deWhereDecl (AnnPragma srcLoc annotation) = AnnPragma (deWhereSrcLoc srcLoc) (deWhereAnnotation annotation)
deWhereDecl (MinimalPragma srcLoc booleanFormula) = MinimalPragma (deWhereSrcLoc srcLoc) (fmap (deWhereBooleanFormula) booleanFormula)
deWhereDecl (RoleAnnotDecl srcLoc qName role) = RoleAnnotDecl (deWhereSrcLoc srcLoc) (deWhereQName qName) (fmap (deWhereRole) role)
deWhereDeriving :: Deriving -> Deriving
deWhereDeriving a = (((deWhereQName) *** (fmap (deWhereType))) a)
deWhereExp :: Exp -> Exp
deWhereExp (Var qName) = Var (deWhereQName qName)
deWhereExp (IPVar iPName) = IPVar (deWhereIPName iPName)
deWhereExp (Con qName) = Con (deWhereQName qName)
deWhereExp (Lit literal) = Lit (deWhereLiteral literal)
deWhereExp (InfixApp exp1 qOp exp2) = InfixApp (deWhereExp exp1) (deWhereQOp qOp) (deWhereExp exp2)
deWhereExp (App exp1 exp2) = App (deWhereExp exp1) (deWhereExp exp2)
deWhereExp (NegApp exp) = NegApp (deWhereExp exp)
deWhereExp (Lambda srcLoc pat exp) = Lambda (deWhereSrcLoc srcLoc) (fmap (deWherePat) pat) (deWhereExp exp)
deWhereExp (Let binds exp) = Let (deWhereBinds binds) (deWhereExp exp)
deWhereExp (If exp1 exp2 exp3) = If (deWhereExp exp1) (deWhereExp exp2) (deWhereExp exp3)
deWhereExp (MultiIf guardedRhs) = MultiIf (fmap (deWhereGuardedRhs) guardedRhs)
deWhereExp (Case exp alt) = Case (deWhereExp exp) (fmap (deWhereAlt) alt)
deWhereExp (Do stmt) = Do (fmap (deWhereStmt) stmt)
deWhereExp (MDo stmt) = MDo (fmap (deWhereStmt) stmt)
deWhereExp (Tuple boxed exp) = Tuple (deWhereBoxed boxed) (fmap (deWhereExp) exp)
deWhereExp (TupleSection boxed exp) = TupleSection (deWhereBoxed boxed) (fmap (fmap (deWhereExp)) exp)
deWhereExp (List exp) = List (fmap (deWhereExp) exp)
deWhereExp (ParArray exp) = ParArray (fmap (deWhereExp) exp)
deWhereExp (Paren exp) = Paren (deWhereExp exp)
deWhereExp (LeftSection exp qOp) = LeftSection (deWhereExp exp) (deWhereQOp qOp)
deWhereExp (RightSection qOp exp) = RightSection (deWhereQOp qOp) (deWhereExp exp)
deWhereExp (RecConstr qName fieldUpdate) = RecConstr (deWhereQName qName) (fmap (deWhereFieldUpdate) fieldUpdate)
deWhereExp (RecUpdate exp fieldUpdate) = RecUpdate (deWhereExp exp) (fmap (deWhereFieldUpdate) fieldUpdate)
deWhereExp (EnumFrom exp) = EnumFrom (deWhereExp exp)
deWhereExp (EnumFromTo exp1 exp2) = EnumFromTo (deWhereExp exp1) (deWhereExp exp2)
deWhereExp (EnumFromThen exp1 exp2) = EnumFromThen (deWhereExp exp1) (deWhereExp exp2)
deWhereExp (EnumFromThenTo exp1 exp2 exp3) = EnumFromThenTo (deWhereExp exp1) (deWhereExp exp2) (deWhereExp exp3)
deWhereExp (ParArrayFromTo exp1 exp2) = ParArrayFromTo (deWhereExp exp1) (deWhereExp exp2)
deWhereExp (ParArrayFromThenTo exp1 exp2 exp3) = ParArrayFromThenTo (deWhereExp exp1) (deWhereExp exp2) (deWhereExp exp3)
deWhereExp (ListComp exp qualStmt) = ListComp (deWhereExp exp) (fmap (deWhereQualStmt) qualStmt)
deWhereExp (ParComp exp qualStmt) = ParComp (deWhereExp exp) (fmap (fmap (deWhereQualStmt)) qualStmt)
deWhereExp (ParArrayComp exp qualStmt) = ParArrayComp (deWhereExp exp) (fmap (fmap (deWhereQualStmt)) qualStmt)
deWhereExp (ExpTypeSig srcLoc exp type0) = ExpTypeSig (deWhereSrcLoc srcLoc) (deWhereExp exp) (deWhereType type0)
deWhereExp (VarQuote qName) = VarQuote (deWhereQName qName)
deWhereExp (TypQuote qName) = TypQuote (deWhereQName qName)
deWhereExp (BracketExp bracket) = BracketExp (deWhereBracket bracket)
deWhereExp (SpliceExp splice) = SpliceExp (deWhereSplice splice)
deWhereExp (QuasiQuote string1 string2) = QuasiQuote (id string1) (id string2)
deWhereExp (XTag srcLoc xName xAttr exp1 exp2) = XTag (deWhereSrcLoc srcLoc) (deWhereXName xName) (fmap (deWhereXAttr) xAttr) (fmap (deWhereExp) exp1) (fmap (deWhereExp) exp2)
deWhereExp (XETag srcLoc xName xAttr exp) = XETag (deWhereSrcLoc srcLoc) (deWhereXName xName) (fmap (deWhereXAttr) xAttr) (fmap (deWhereExp) exp)
deWhereExp (XPcdata string) = XPcdata (id string)
deWhereExp (XExpTag exp) = XExpTag (deWhereExp exp)
deWhereExp (XChildTag srcLoc exp) = XChildTag (deWhereSrcLoc srcLoc) (fmap (deWhereExp) exp)
deWhereExp (CorePragma string exp) = CorePragma (id string) (deWhereExp exp)
deWhereExp (SCCPragma string exp) = SCCPragma (id string) (deWhereExp exp)
deWhereExp (GenPragma string int1 int2 exp) = GenPragma (id string) (((id) *** (id)) int1) (((id) *** (id)) int2) (deWhereExp exp)
deWhereExp (Proc srcLoc pat exp) = Proc (deWhereSrcLoc srcLoc) (deWherePat pat) (deWhereExp exp)
deWhereExp (LeftArrApp exp1 exp2) = LeftArrApp (deWhereExp exp1) (deWhereExp exp2)
deWhereExp (RightArrApp exp1 exp2) = RightArrApp (deWhereExp exp1) (deWhereExp exp2)
deWhereExp (LeftArrHighApp exp1 exp2) = LeftArrHighApp (deWhereExp exp1) (deWhereExp exp2)
deWhereExp (RightArrHighApp exp1 exp2) = RightArrHighApp (deWhereExp exp1) (deWhereExp exp2)
deWhereExp (LCase alt) = LCase (fmap (deWhereAlt) alt)
deWhereExp (ExprHole) = ExprHole
deWhereExportSpec :: ExportSpec -> ExportSpec
deWhereExportSpec (EVar qName) = EVar (deWhereQName qName)
deWhereExportSpec (EAbs namespace qName) = EAbs (deWhereNamespace namespace) (deWhereQName qName)
deWhereExportSpec (EThingAll qName) = EThingAll (deWhereQName qName)
deWhereExportSpec (EThingWith qName cName) = EThingWith (deWhereQName qName) (fmap (deWhereCName) cName)
deWhereExportSpec (EModuleContents moduleName) = EModuleContents (deWhereModuleName moduleName)
deWhereFieldUpdate :: FieldUpdate -> FieldUpdate
deWhereFieldUpdate (FieldUpdate qName exp) = FieldUpdate (deWhereQName qName) (deWhereExp exp)
deWhereFieldUpdate (FieldPun qName) = FieldPun (deWhereQName qName)
deWhereFieldUpdate (FieldWildcard) = FieldWildcard
deWhereFunDep :: FunDep -> FunDep
deWhereFunDep (FunDep name1 name2) = FunDep (fmap (deWhereName) name1) (fmap (deWhereName) name2)
deWhereGadtDecl :: GadtDecl -> GadtDecl
deWhereGadtDecl (GadtDecl srcLoc name1 name2 type0) = GadtDecl (deWhereSrcLoc srcLoc) (deWhereName name1) (fmap (((fmap (deWhereName)) *** (deWhereType))) name2) (deWhereType type0)
deWhereGuardedRhs :: GuardedRhs -> GuardedRhs
deWhereGuardedRhs (GuardedRhs srcLoc stmt exp) = GuardedRhs (deWhereSrcLoc srcLoc) (fmap (deWhereStmt) stmt) (deWhereExp exp)
deWhereIPBind :: IPBind -> IPBind
deWhereIPBind (IPBind srcLoc iPName exp) = IPBind (deWhereSrcLoc srcLoc) (deWhereIPName iPName) (deWhereExp exp)
deWhereIPName :: IPName -> IPName
deWhereIPName (IPDup string) = IPDup (id string)
deWhereIPName (IPLin string) = IPLin (id string)
deWhereImportDecl :: ImportDecl -> ImportDecl
deWhereImportDecl (ImportDecl importLoc importModule importQualified importSrc importSafe importPkg importAs importSpecs) = ImportDecl (deWhereSrcLoc importLoc) (deWhereModuleName importModule) (id importQualified) (id importSrc) (id importSafe) (fmap (id) importPkg) (fmap (deWhereModuleName) importAs) (fmap (((id) *** (fmap (deWhereImportSpec)))) importSpecs)
deWhereImportSpec :: ImportSpec -> ImportSpec
deWhereImportSpec (IVar name) = IVar (deWhereName name)
deWhereImportSpec (IAbs namespace name) = IAbs (deWhereNamespace namespace) (deWhereName name)
deWhereImportSpec (IThingAll name) = IThingAll (deWhereName name)
deWhereImportSpec (IThingWith name cName) = IThingWith (deWhereName name) (fmap (deWhereCName) cName)
deWhereInstDecl :: InstDecl -> InstDecl
deWhereInstDecl (InsDecl decl) = InsDecl (deWhereDecl decl)
deWhereInstDecl (InsType srcLoc type1 type2) = InsType (deWhereSrcLoc srcLoc) (deWhereType type1) (deWhereType type2)
deWhereInstDecl (InsData srcLoc dataOrNew type0 qualConDecl deriving0) = InsData (deWhereSrcLoc srcLoc) (deWhereDataOrNew dataOrNew) (deWhereType type0) (fmap (deWhereQualConDecl) qualConDecl) (fmap (deWhereDeriving) deriving0)
deWhereInstDecl (InsGData srcLoc dataOrNew type0 kind gadtDecl deriving0) = InsGData (deWhereSrcLoc srcLoc) (deWhereDataOrNew dataOrNew) (deWhereType type0) (fmap (deWhereKind) kind) (fmap (deWhereGadtDecl) gadtDecl) (fmap (deWhereDeriving) deriving0)
deWhereKind :: Kind -> Kind
deWhereKind (KindStar) = KindStar
deWhereKind (KindFn kind1 kind2) = KindFn (deWhereKind kind1) (deWhereKind kind2)
deWhereKind (KindParen kind) = KindParen (deWhereKind kind)
deWhereKind (KindVar qName) = KindVar (deWhereQName qName)
deWhereKind (KindApp kind1 kind2) = KindApp (deWhereKind kind1) (deWhereKind kind2)
deWhereKind (KindTuple kind) = KindTuple (fmap (deWhereKind) kind)
deWhereKind (KindList kind) = KindList (deWhereKind kind)
deWhereLiteral :: Literal -> Literal
deWhereLiteral (Char char) = Char (id char)
deWhereLiteral (String string) = String (id string)
deWhereLiteral (Int integer) = Int (id integer)
deWhereLiteral (Frac rational) = Frac (id rational)
deWhereLiteral (PrimInt integer) = PrimInt (id integer)
deWhereLiteral (PrimWord integer) = PrimWord (id integer)
deWhereLiteral (PrimFloat rational) = PrimFloat (id rational)
deWhereLiteral (PrimDouble rational) = PrimDouble (id rational)
deWhereLiteral (PrimChar char) = PrimChar (id char)
deWhereLiteral (PrimString string) = PrimString (id string)
deWhereMatch :: Match -> Match
deWhereMatch (Match srcLoc name pat type0 (UnGuardedRhs exp) (Just binds)) =
  deWhereMatch (Match srcLoc name pat type0 (UnGuardedRhs (Let binds exp)) Nothing)
deWhereMatch (Match srcLoc name pat type0 rhs binds) = Match (deWhereSrcLoc srcLoc) (deWhereName name) (fmap (deWherePat) pat) (fmap (deWhereType) type0) (deWhereRhs rhs) (fmap (deWhereBinds) binds)
deWhereModule :: Module -> Module
deWhereModule (Module srcLoc moduleName modulePragma warningText exportSpec importDecl decl) = Module (deWhereSrcLoc srcLoc) (deWhereModuleName moduleName) (fmap (deWhereModulePragma) modulePragma) (fmap (deWhereWarningText) warningText) (fmap (fmap (deWhereExportSpec)) exportSpec) (fmap (deWhereImportDecl) importDecl) (fmap (deWhereDecl) decl)
deWhereModuleName :: ModuleName -> ModuleName
deWhereModuleName (ModuleName string) = ModuleName (id string)
deWhereModulePragma :: ModulePragma -> ModulePragma
deWhereModulePragma (LanguagePragma srcLoc name) = LanguagePragma (deWhereSrcLoc srcLoc) (fmap (deWhereName) name)
deWhereModulePragma (OptionsPragma srcLoc tool string) = OptionsPragma (deWhereSrcLoc srcLoc) (fmap (deWhereTool) tool) (id string)
deWhereModulePragma (AnnModulePragma srcLoc annotation) = AnnModulePragma (deWhereSrcLoc srcLoc) (deWhereAnnotation annotation)
deWhereName :: Name -> Name
deWhereName (Ident string) = Ident (id string)
deWhereName (Symbol string) = Symbol (id string)
deWhereNamespace :: Namespace -> Namespace
deWhereNamespace (NoNamespace) = NoNamespace
deWhereNamespace (TypeNamespace) = TypeNamespace
deWhereNamespace (PatternNamespace) = PatternNamespace
deWhereOp :: Op -> Op
deWhereOp (VarOp name) = VarOp (deWhereName name)
deWhereOp (ConOp name) = ConOp (deWhereName name)
deWhereOverlap :: Overlap -> Overlap
deWhereOverlap (NoOverlap) = NoOverlap
deWhereOverlap (Overlap) = Overlap
deWhereOverlap (Incoherent) = Incoherent
deWherePXAttr :: PXAttr -> PXAttr
deWherePXAttr (PXAttr xName pat) = PXAttr (deWhereXName xName) (deWherePat pat)
deWherePat :: Pat -> Pat
deWherePat (PVar name) = PVar (deWhereName name)
deWherePat (PLit sign literal) = PLit (deWhereSign sign) (deWhereLiteral literal)
deWherePat (PNPlusK name integer) = PNPlusK (deWhereName name) (id integer)
deWherePat (PInfixApp pat1 qName pat2) = PInfixApp (deWherePat pat1) (deWhereQName qName) (deWherePat pat2)
deWherePat (PApp qName pat) = PApp (deWhereQName qName) (fmap (deWherePat) pat)
deWherePat (PTuple boxed pat) = PTuple (deWhereBoxed boxed) (fmap (deWherePat) pat)
deWherePat (PList pat) = PList (fmap (deWherePat) pat)
deWherePat (PParen pat) = PParen (deWherePat pat)
deWherePat (PRec qName patField) = PRec (deWhereQName qName) (fmap (deWherePatField) patField)
deWherePat (PAsPat name pat) = PAsPat (deWhereName name) (deWherePat pat)
deWherePat (PWildCard) = PWildCard
deWherePat (PIrrPat pat) = PIrrPat (deWherePat pat)
deWherePat (PatTypeSig srcLoc pat type0) = PatTypeSig (deWhereSrcLoc srcLoc) (deWherePat pat) (deWhereType type0)
deWherePat (PViewPat exp pat) = PViewPat (deWhereExp exp) (deWherePat pat)
deWherePat (PRPat rPat) = PRPat (fmap (deWhereRPat) rPat)
deWherePat (PXTag srcLoc xName pXAttr pat1 pat2) = PXTag (deWhereSrcLoc srcLoc) (deWhereXName xName) (fmap (deWherePXAttr) pXAttr) (fmap (deWherePat) pat1) (fmap (deWherePat) pat2)
deWherePat (PXETag srcLoc xName pXAttr pat) = PXETag (deWhereSrcLoc srcLoc) (deWhereXName xName) (fmap (deWherePXAttr) pXAttr) (fmap (deWherePat) pat)
deWherePat (PXPcdata string) = PXPcdata (id string)
deWherePat (PXPatTag pat) = PXPatTag (deWherePat pat)
deWherePat (PXRPats rPat) = PXRPats (fmap (deWhereRPat) rPat)
deWherePat (PQuasiQuote string1 string2) = PQuasiQuote (id string1) (id string2)
deWherePat (PBangPat pat) = PBangPat (deWherePat pat)
deWherePatField :: PatField -> PatField
deWherePatField (PFieldPat qName pat) = PFieldPat (deWhereQName qName) (deWherePat pat)
deWherePatField (PFieldPun qName) = PFieldPun (deWhereQName qName)
deWherePatField (PFieldWildcard) = PFieldWildcard
deWherePatternSynDirection :: PatternSynDirection -> PatternSynDirection
deWherePatternSynDirection (Unidirectional) = Unidirectional
deWherePatternSynDirection (ImplicitBidirectional) = ImplicitBidirectional
deWherePatternSynDirection (ExplicitBidirectional decl) = ExplicitBidirectional (fmap (deWhereDecl) decl)
deWherePromoted :: Promoted -> Promoted
deWherePromoted (PromotedInteger integer) = PromotedInteger (id integer)
deWherePromoted (PromotedString string) = PromotedString (id string)
deWherePromoted (PromotedCon bool qName) = PromotedCon (id bool) (deWhereQName qName)
deWherePromoted (PromotedList bool type0) = PromotedList (id bool) (fmap (deWhereType) type0)
deWherePromoted (PromotedTuple type0) = PromotedTuple (fmap (deWhereType) type0)
deWherePromoted (PromotedUnit) = PromotedUnit
deWhereQName :: QName -> QName
deWhereQName (Qual moduleName name) = Qual (deWhereModuleName moduleName) (deWhereName name)
deWhereQName (UnQual name) = UnQual (deWhereName name)
deWhereQName (Special specialCon) = Special (deWhereSpecialCon specialCon)
deWhereQOp :: QOp -> QOp
deWhereQOp (QVarOp qName) = QVarOp (deWhereQName qName)
deWhereQOp (QConOp qName) = QConOp (deWhereQName qName)
deWhereQualConDecl :: QualConDecl -> QualConDecl
deWhereQualConDecl (QualConDecl srcLoc tyVarBind context conDecl) = QualConDecl (deWhereSrcLoc srcLoc) (fmap (deWhereTyVarBind) tyVarBind) (deWhereContext context) (deWhereConDecl conDecl)
deWhereQualStmt :: QualStmt -> QualStmt
deWhereQualStmt (QualStmt stmt) = QualStmt (deWhereStmt stmt)
deWhereQualStmt (ThenTrans exp) = ThenTrans (deWhereExp exp)
deWhereQualStmt (ThenBy exp1 exp2) = ThenBy (deWhereExp exp1) (deWhereExp exp2)
deWhereQualStmt (GroupBy exp) = GroupBy (deWhereExp exp)
deWhereQualStmt (GroupUsing exp) = GroupUsing (deWhereExp exp)
deWhereQualStmt (GroupByUsing exp1 exp2) = GroupByUsing (deWhereExp exp1) (deWhereExp exp2)
deWhereRPat :: RPat -> RPat
deWhereRPat (RPOp rPat rPatOp) = RPOp (deWhereRPat rPat) (deWhereRPatOp rPatOp)
deWhereRPat (RPEither rPat1 rPat2) = RPEither (deWhereRPat rPat1) (deWhereRPat rPat2)
deWhereRPat (RPSeq rPat) = RPSeq (fmap (deWhereRPat) rPat)
deWhereRPat (RPGuard pat stmt) = RPGuard (deWherePat pat) (fmap (deWhereStmt) stmt)
deWhereRPat (RPCAs name rPat) = RPCAs (deWhereName name) (deWhereRPat rPat)
deWhereRPat (RPAs name rPat) = RPAs (deWhereName name) (deWhereRPat rPat)
deWhereRPat (RPParen rPat) = RPParen (deWhereRPat rPat)
deWhereRPat (RPPat pat) = RPPat (deWherePat pat)
deWhereRPatOp :: RPatOp -> RPatOp
deWhereRPatOp (RPStar) = RPStar
deWhereRPatOp (RPStarG) = RPStarG
deWhereRPatOp (RPPlus) = RPPlus
deWhereRPatOp (RPPlusG) = RPPlusG
deWhereRPatOp (RPOpt) = RPOpt
deWhereRPatOp (RPOptG) = RPOptG
deWhereRhs :: Rhs -> Rhs
deWhereRhs (UnGuardedRhs exp) = UnGuardedRhs (deWhereExp exp)
deWhereRhs (GuardedRhss guardedRhs) = GuardedRhss (fmap (deWhereGuardedRhs) guardedRhs)
deWhereRole :: Role -> Role
deWhereRole (Nominal) = Nominal
deWhereRole (Representational) = Representational
deWhereRole (Phantom) = Phantom
deWhereRole (RoleWildcard) = RoleWildcard
deWhereRule :: Rule -> Rule
deWhereRule (Rule string activation ruleVar exp1 exp2) = Rule (id string) (deWhereActivation activation) (fmap (fmap (deWhereRuleVar)) ruleVar) (deWhereExp exp1) (deWhereExp exp2)
deWhereRuleVar :: RuleVar -> RuleVar
deWhereRuleVar (RuleVar name) = RuleVar (deWhereName name)
deWhereRuleVar (TypedRuleVar name type0) = TypedRuleVar (deWhereName name) (deWhereType type0)
deWhereSafety :: Safety -> Safety
deWhereSafety (PlayRisky) = PlayRisky
deWhereSafety (PlaySafe bool) = PlaySafe (id bool)
deWhereSafety (PlayInterruptible) = PlayInterruptible
deWhereSign :: Sign -> Sign
deWhereSign (Signless) = Signless
deWhereSign (Negative) = Negative
deWhereSpecialCon :: SpecialCon -> SpecialCon
deWhereSpecialCon (UnitCon) = UnitCon
deWhereSpecialCon (ListCon) = ListCon
deWhereSpecialCon (FunCon) = FunCon
deWhereSpecialCon (TupleCon boxed int) = TupleCon (deWhereBoxed boxed) (id int)
deWhereSpecialCon (Cons) = Cons
deWhereSpecialCon (UnboxedSingleCon) = UnboxedSingleCon
deWhereSplice :: Splice -> Splice
deWhereSplice (IdSplice string) = IdSplice (id string)
deWhereSplice (ParenSplice exp) = ParenSplice (deWhereExp exp)
deWhereStmt :: Stmt -> Stmt
deWhereStmt (Generator srcLoc pat exp) = Generator (deWhereSrcLoc srcLoc) (deWherePat pat) (deWhereExp exp)
deWhereStmt (Qualifier exp) = Qualifier (deWhereExp exp)
deWhereStmt (LetStmt binds) = LetStmt (deWhereBinds binds)
deWhereStmt (RecStmt stmt) = RecStmt (fmap (deWhereStmt) stmt)
deWhereTyVarBind :: TyVarBind -> TyVarBind
deWhereTyVarBind (KindedVar name kind) = KindedVar (deWhereName name) (deWhereKind kind)
deWhereTyVarBind (UnkindedVar name) = UnkindedVar (deWhereName name)
deWhereType :: Type -> Type
deWhereType (TyForall tyVarBind context type0) = TyForall (fmap (fmap (deWhereTyVarBind)) tyVarBind) (deWhereContext context) (deWhereType type0)
deWhereType (TyFun type1 type2) = TyFun (deWhereType type1) (deWhereType type2)
deWhereType (TyTuple boxed type0) = TyTuple (deWhereBoxed boxed) (fmap (deWhereType) type0)
deWhereType (TyList type0) = TyList (deWhereType type0)
deWhereType (TyParArray type0) = TyParArray (deWhereType type0)
deWhereType (TyApp type1 type2) = TyApp (deWhereType type1) (deWhereType type2)
deWhereType (TyVar name) = TyVar (deWhereName name)
deWhereType (TyCon qName) = TyCon (deWhereQName qName)
deWhereType (TyParen type0) = TyParen (deWhereType type0)
deWhereType (TyInfix type1 qName type2) = TyInfix (deWhereType type1) (deWhereQName qName) (deWhereType type2)
deWhereType (TyKind type0 kind) = TyKind (deWhereType type0) (deWhereKind kind)
deWhereType (TyPromoted promoted) = TyPromoted (deWherePromoted promoted)
deWhereType (TyEquals type1 type2) = TyEquals (deWhereType type1) (deWhereType type2)
deWhereType (TySplice splice) = TySplice (deWhereSplice splice)
deWhereType (TyBang bangType type0) = TyBang (deWhereBangType bangType) (deWhereType type0)
deWhereType (TyWildCard name) = TyWildCard (fmap (deWhereName) name)
deWhereTypeEqn :: TypeEqn -> TypeEqn
deWhereTypeEqn (TypeEqn type1 type2) = TypeEqn (deWhereType type1) (deWhereType type2)
deWhereWarningText :: WarningText -> WarningText
deWhereWarningText (DeprText string) = DeprText (id string)
deWhereWarningText (WarnText string) = WarnText (id string)
deWhereXAttr :: XAttr -> XAttr
deWhereXAttr (XAttr xName exp) = XAttr (deWhereXName xName) (deWhereExp exp)
deWhereXName :: XName -> XName
deWhereXName (XName string) = XName (id string)
deWhereXName (XDomName string1 string2) = XDomName (id string1) (id string2)

