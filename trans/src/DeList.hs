module DeList where
import Language.Haskell.Exts.Syntax
import Control.Arrow ((***))
deListBoxed :: Boxed -> Boxed
deListBoxed (Boxed) = Boxed
deListBoxed (Unboxed) = Unboxed
deListTool :: Tool -> Tool
deListTool (GHC) = GHC
deListTool (HUGS) = HUGS
deListTool (NHC98) = NHC98
deListTool (YHC) = YHC
deListTool (HADDOCK) = HADDOCK
deListTool (UnknownTool string) = UnknownTool (id string)
deListSrcLoc :: SrcLoc -> SrcLoc
deListSrcLoc (SrcLoc srcFilename srcLine srcColumn) = SrcLoc (id srcFilename) (id srcLine) (id srcColumn)
deListActivation :: Activation -> Activation
deListActivation (AlwaysActive) = AlwaysActive
deListActivation (ActiveFrom int) = ActiveFrom (id int)
deListActivation (ActiveUntil int) = ActiveUntil (id int)
deListAlt :: Alt -> Alt
deListAlt (Alt srcLoc pat rhs binds) = Alt (deListSrcLoc srcLoc) (deListPat pat) (deListRhs rhs) (fmap (deListBinds) binds)
deListAnnotation :: Annotation -> Annotation
deListAnnotation (Ann name exp) = Ann (deListName name) (deListExp exp)
deListAnnotation (TypeAnn name exp) = TypeAnn (deListName name) (deListExp exp)
deListAnnotation (ModuleAnn exp) = ModuleAnn (deListExp exp)
deListAssoc :: Assoc -> Assoc
deListAssoc (AssocNone) = AssocNone
deListAssoc (AssocLeft) = AssocLeft
deListAssoc (AssocRight) = AssocRight
deListAsst :: Asst -> Asst
deListAsst (ClassA qName type0) = ClassA (deListQName qName) (fmap (deListType) type0)
deListAsst (AppA name type0) = AppA (deListName name) (fmap (deListType) type0)
deListAsst (InfixA type1 qName type2) = InfixA (deListType type1) (deListQName qName) (deListType type2)
deListAsst (IParam iPName type0) = IParam (deListIPName iPName) (deListType type0)
deListAsst (EqualP type1 type2) = EqualP (deListType type1) (deListType type2)
deListAsst (ParenA asst) = ParenA (deListAsst asst)
deListAsst (WildCardA name) = WildCardA (fmap (deListName) name)
deListBangType :: BangType -> BangType
deListBangType (BangedTy) = BangedTy
deListBangType (UnpackedTy) = UnpackedTy
deListBinds :: Binds -> Binds
deListBinds (BDecls decl) = BDecls (fmap (deListDecl) decl)
deListBinds (IPBinds iPBind) = IPBinds (fmap (deListIPBind) iPBind)
deListBooleanFormula :: BooleanFormula -> BooleanFormula
deListBooleanFormula (VarFormula name) = VarFormula (deListName name)
deListBooleanFormula (AndFormula booleanFormula) = AndFormula (fmap (deListBooleanFormula) booleanFormula)
deListBooleanFormula (OrFormula booleanFormula) = OrFormula (fmap (deListBooleanFormula) booleanFormula)
deListBooleanFormula (ParenFormula booleanFormula) = ParenFormula (deListBooleanFormula booleanFormula)
deListBracket :: Bracket -> Bracket
deListBracket (ExpBracket exp) = ExpBracket (deListExp exp)
deListBracket (PatBracket pat) = PatBracket (deListPat pat)
deListBracket (TypeBracket type0) = TypeBracket (deListType type0)
deListBracket (DeclBracket decl) = DeclBracket (fmap (deListDecl) decl)
deListCName :: CName -> CName
deListCName (VarName name) = VarName (deListName name)
deListCName (ConName name) = ConName (deListName name)
deListCallConv :: CallConv -> CallConv
deListCallConv (StdCall) = StdCall
deListCallConv (CCall) = CCall
deListCallConv (CPlusPlus) = CPlusPlus
deListCallConv (DotNet) = DotNet
deListCallConv (Jvm) = Jvm
deListCallConv (Js) = Js
deListCallConv (JavaScript) = JavaScript
deListCallConv (CApi) = CApi
deListClassDecl :: ClassDecl -> ClassDecl
deListClassDecl (ClsDecl decl) = ClsDecl (deListDecl decl)
deListClassDecl (ClsDataFam srcLoc context name tyVarBind kind) = ClsDataFam (deListSrcLoc srcLoc) (deListContext context) (deListName name) (fmap (deListTyVarBind) tyVarBind) (fmap (deListKind) kind)
deListClassDecl (ClsTyFam srcLoc name tyVarBind kind) = ClsTyFam (deListSrcLoc srcLoc) (deListName name) (fmap (deListTyVarBind) tyVarBind) (fmap (deListKind) kind)
deListClassDecl (ClsTyDef srcLoc type1 type2) = ClsTyDef (deListSrcLoc srcLoc) (deListType type1) (deListType type2)
deListClassDecl (ClsDefSig srcLoc name type0) = ClsDefSig (deListSrcLoc srcLoc) (deListName name) (deListType type0)
deListConDecl :: ConDecl -> ConDecl
deListConDecl (ConDecl name type0) = ConDecl (deListName name) (fmap (deListType) type0)
deListConDecl (InfixConDecl type1 name type2) = InfixConDecl (deListType type1) (deListName name) (deListType type2)
deListConDecl (RecDecl name1 name2) = RecDecl (deListName name1) (fmap (((fmap (deListName)) *** (deListType))) name2)
deListContext :: Context -> Context
deListContext a = (fmap (deListAsst) a)
deListDataOrNew :: DataOrNew -> DataOrNew
deListDataOrNew (DataType) = DataType
deListDataOrNew (NewType) = NewType
deListDecl :: Decl -> Decl
deListDecl (TypeDecl srcLoc name tyVarBind type0) = TypeDecl (deListSrcLoc srcLoc) (deListName name) (fmap (deListTyVarBind) tyVarBind) (deListType type0)
deListDecl (TypeFamDecl srcLoc name tyVarBind kind) = TypeFamDecl (deListSrcLoc srcLoc) (deListName name) (fmap (deListTyVarBind) tyVarBind) (fmap (deListKind) kind)
deListDecl (ClosedTypeFamDecl srcLoc name tyVarBind kind typeEqn) = ClosedTypeFamDecl (deListSrcLoc srcLoc) (deListName name) (fmap (deListTyVarBind) tyVarBind) (fmap (deListKind) kind) (fmap (deListTypeEqn) typeEqn)
deListDecl (DataDecl srcLoc dataOrNew context name tyVarBind qualConDecl deriving0) = DataDecl (deListSrcLoc srcLoc) (deListDataOrNew dataOrNew) (deListContext context) (deListName name) (fmap (deListTyVarBind) tyVarBind) (fmap (deListQualConDecl) qualConDecl) (fmap (deListDeriving) deriving0)
deListDecl (GDataDecl srcLoc dataOrNew context name tyVarBind kind gadtDecl deriving0) = GDataDecl (deListSrcLoc srcLoc) (deListDataOrNew dataOrNew) (deListContext context) (deListName name) (fmap (deListTyVarBind) tyVarBind) (fmap (deListKind) kind) (fmap (deListGadtDecl) gadtDecl) (fmap (deListDeriving) deriving0)
deListDecl (DataFamDecl srcLoc context name tyVarBind kind) = DataFamDecl (deListSrcLoc srcLoc) (deListContext context) (deListName name) (fmap (deListTyVarBind) tyVarBind) (fmap (deListKind) kind)
deListDecl (TypeInsDecl srcLoc type1 type2) = TypeInsDecl (deListSrcLoc srcLoc) (deListType type1) (deListType type2)
deListDecl (DataInsDecl srcLoc dataOrNew type0 qualConDecl deriving0) = DataInsDecl (deListSrcLoc srcLoc) (deListDataOrNew dataOrNew) (deListType type0) (fmap (deListQualConDecl) qualConDecl) (fmap (deListDeriving) deriving0)
deListDecl (GDataInsDecl srcLoc dataOrNew type0 kind gadtDecl deriving0) = GDataInsDecl (deListSrcLoc srcLoc) (deListDataOrNew dataOrNew) (deListType type0) (fmap (deListKind) kind) (fmap (deListGadtDecl) gadtDecl) (fmap (deListDeriving) deriving0)
deListDecl (ClassDecl srcLoc context name tyVarBind funDep classDecl) = ClassDecl (deListSrcLoc srcLoc) (deListContext context) (deListName name) (fmap (deListTyVarBind) tyVarBind) (fmap (deListFunDep) funDep) (fmap (deListClassDecl) classDecl)
deListDecl (InstDecl srcLoc overlap tyVarBind context qName type0 instDecl) = InstDecl (deListSrcLoc srcLoc) (fmap (deListOverlap) overlap) (fmap (deListTyVarBind) tyVarBind) (deListContext context) (deListQName qName) (fmap (deListType) type0) (fmap (deListInstDecl) instDecl)
deListDecl (DerivDecl srcLoc overlap tyVarBind context qName type0) = DerivDecl (deListSrcLoc srcLoc) (fmap (deListOverlap) overlap) (fmap (deListTyVarBind) tyVarBind) (deListContext context) (deListQName qName) (fmap (deListType) type0)
deListDecl (InfixDecl srcLoc assoc int op) = InfixDecl (deListSrcLoc srcLoc) (deListAssoc assoc) (id int) (fmap (deListOp) op)
deListDecl (DefaultDecl srcLoc type0) = DefaultDecl (deListSrcLoc srcLoc) (fmap (deListType) type0)
deListDecl (SpliceDecl srcLoc exp) = SpliceDecl (deListSrcLoc srcLoc) (deListExp exp)
deListDecl (TypeSig srcLoc name type0) = TypeSig (deListSrcLoc srcLoc) (fmap (deListName) name) (deListType type0)
deListDecl (PatSynSig srcLoc name tyVarBind context1 context2 type0) = PatSynSig (deListSrcLoc srcLoc) (deListName name) (fmap (fmap (deListTyVarBind)) tyVarBind) (deListContext context1) (deListContext context2) (deListType type0)
deListDecl (FunBind match) = FunBind (fmap (deListMatch) match)
deListDecl (PatBind srcLoc pat rhs binds) = PatBind (deListSrcLoc srcLoc) (deListPat pat) (deListRhs rhs) (fmap (deListBinds) binds)
deListDecl (ForImp srcLoc callConv safety string name type0) = ForImp (deListSrcLoc srcLoc) (deListCallConv callConv) (deListSafety safety) (id string) (deListName name) (deListType type0)
deListDecl (ForExp srcLoc callConv string name type0) = ForExp (deListSrcLoc srcLoc) (deListCallConv callConv) (id string) (deListName name) (deListType type0)
deListDecl (PatSyn srcLoc pat1 pat2 patternSynDirection) = PatSyn (deListSrcLoc srcLoc) (deListPat pat1) (deListPat pat2) (deListPatternSynDirection patternSynDirection)
deListDecl (RulePragmaDecl srcLoc rule) = RulePragmaDecl (deListSrcLoc srcLoc) (fmap (deListRule) rule)
deListDecl (DeprPragmaDecl srcLoc name) = DeprPragmaDecl (deListSrcLoc srcLoc) (fmap (((fmap (deListName)) *** (id))) name)
deListDecl (WarnPragmaDecl srcLoc name) = WarnPragmaDecl (deListSrcLoc srcLoc) (fmap (((fmap (deListName)) *** (id))) name)
deListDecl (InlineSig srcLoc bool activation qName) = InlineSig (deListSrcLoc srcLoc) (id bool) (deListActivation activation) (deListQName qName)
deListDecl (InlineConlikeSig srcLoc activation qName) = InlineConlikeSig (deListSrcLoc srcLoc) (deListActivation activation) (deListQName qName)
deListDecl (SpecSig srcLoc activation qName type0) = SpecSig (deListSrcLoc srcLoc) (deListActivation activation) (deListQName qName) (fmap (deListType) type0)
deListDecl (SpecInlineSig srcLoc bool activation qName type0) = SpecInlineSig (deListSrcLoc srcLoc) (id bool) (deListActivation activation) (deListQName qName) (fmap (deListType) type0)
deListDecl (InstSig srcLoc tyVarBind context qName type0) = InstSig (deListSrcLoc srcLoc) (fmap (deListTyVarBind) tyVarBind) (deListContext context) (deListQName qName) (fmap (deListType) type0)
deListDecl (AnnPragma srcLoc annotation) = AnnPragma (deListSrcLoc srcLoc) (deListAnnotation annotation)
deListDecl (MinimalPragma srcLoc booleanFormula) = MinimalPragma (deListSrcLoc srcLoc) (fmap (deListBooleanFormula) booleanFormula)
deListDecl (RoleAnnotDecl srcLoc qName role) = RoleAnnotDecl (deListSrcLoc srcLoc) (deListQName qName) (fmap (deListRole) role)
deListDeriving :: Deriving -> Deriving
deListDeriving a = (((deListQName) *** (fmap (deListType))) a)
deListExp :: Exp -> Exp
deListExp (Var qName) = Var (deListQName qName)
deListExp (IPVar iPName) = IPVar (deListIPName iPName)
deListExp (Con qName) = Con (deListQName qName)
deListExp (Lit literal) = Lit (deListLiteral literal)
deListExp (InfixApp exp1 qOp exp2) = InfixApp (deListExp exp1) (deListQOp qOp) (deListExp exp2)
deListExp (App exp1 exp2) = App (deListExp exp1) (deListExp exp2)
deListExp (NegApp exp) = NegApp (deListExp exp)
deListExp (Lambda srcLoc pat exp) = Lambda (deListSrcLoc srcLoc) (fmap (deListPat) pat) (deListExp exp)
deListExp (Let binds exp) = Let (deListBinds binds) (deListExp exp)
deListExp (If exp1 exp2 exp3) = If (deListExp exp1) (deListExp exp2) (deListExp exp3)
deListExp (MultiIf guardedRhs) = MultiIf (fmap (deListGuardedRhs) guardedRhs)
deListExp (Case exp alt) = Case (deListExp exp) (fmap (deListAlt) alt)
deListExp (Do stmt) = Do (fmap (deListStmt) stmt)
deListExp (MDo stmt) = MDo (fmap (deListStmt) stmt)
deListExp (Tuple boxed exp) = Tuple (deListBoxed boxed) (fmap (deListExp) exp)
deListExp (TupleSection boxed exp) = TupleSection (deListBoxed boxed) (fmap (fmap (deListExp)) exp)
deListExp (List []) = List []
deListExp (List (e:es)) = deListExp $ Con (Special Cons) `App` e `App` List es
deListExp (ParArray exp) = ParArray (fmap (deListExp) exp)
deListExp (Paren exp) = Paren (deListExp exp)
deListExp (LeftSection exp qOp) = LeftSection (deListExp exp) (deListQOp qOp)
deListExp (RightSection qOp exp) = RightSection (deListQOp qOp) (deListExp exp)
deListExp (RecConstr qName fieldUpdate) = RecConstr (deListQName qName) (fmap (deListFieldUpdate) fieldUpdate)
deListExp (RecUpdate exp fieldUpdate) = RecUpdate (deListExp exp) (fmap (deListFieldUpdate) fieldUpdate)
deListExp (EnumFrom exp) = EnumFrom (deListExp exp)
deListExp (EnumFromTo exp1 exp2) = EnumFromTo (deListExp exp1) (deListExp exp2)
deListExp (EnumFromThen exp1 exp2) = EnumFromThen (deListExp exp1) (deListExp exp2)
deListExp (EnumFromThenTo exp1 exp2 exp3) = EnumFromThenTo (deListExp exp1) (deListExp exp2) (deListExp exp3)
deListExp (ParArrayFromTo exp1 exp2) = ParArrayFromTo (deListExp exp1) (deListExp exp2)
deListExp (ParArrayFromThenTo exp1 exp2 exp3) = ParArrayFromThenTo (deListExp exp1) (deListExp exp2) (deListExp exp3)
deListExp (ListComp exp qualStmt) = ListComp (deListExp exp) (fmap (deListQualStmt) qualStmt)
deListExp (ParComp exp qualStmt) = ParComp (deListExp exp) (fmap (fmap (deListQualStmt)) qualStmt)
deListExp (ParArrayComp exp qualStmt) = ParArrayComp (deListExp exp) (fmap (fmap (deListQualStmt)) qualStmt)
deListExp (ExpTypeSig srcLoc exp type0) = ExpTypeSig (deListSrcLoc srcLoc) (deListExp exp) (deListType type0)
deListExp (VarQuote qName) = VarQuote (deListQName qName)
deListExp (TypQuote qName) = TypQuote (deListQName qName)
deListExp (BracketExp bracket) = BracketExp (deListBracket bracket)
deListExp (SpliceExp splice) = SpliceExp (deListSplice splice)
deListExp (QuasiQuote string1 string2) = QuasiQuote (id string1) (id string2)
deListExp (XTag srcLoc xName xAttr exp1 exp2) = XTag (deListSrcLoc srcLoc) (deListXName xName) (fmap (deListXAttr) xAttr) (fmap (deListExp) exp1) (fmap (deListExp) exp2)
deListExp (XETag srcLoc xName xAttr exp) = XETag (deListSrcLoc srcLoc) (deListXName xName) (fmap (deListXAttr) xAttr) (fmap (deListExp) exp)
deListExp (XPcdata string) = XPcdata (id string)
deListExp (XExpTag exp) = XExpTag (deListExp exp)
deListExp (XChildTag srcLoc exp) = XChildTag (deListSrcLoc srcLoc) (fmap (deListExp) exp)
deListExp (CorePragma string exp) = CorePragma (id string) (deListExp exp)
deListExp (SCCPragma string exp) = SCCPragma (id string) (deListExp exp)
deListExp (GenPragma string int1 int2 exp) = GenPragma (id string) (((id) *** (id)) int1) (((id) *** (id)) int2) (deListExp exp)
deListExp (Proc srcLoc pat exp) = Proc (deListSrcLoc srcLoc) (deListPat pat) (deListExp exp)
deListExp (LeftArrApp exp1 exp2) = LeftArrApp (deListExp exp1) (deListExp exp2)
deListExp (RightArrApp exp1 exp2) = RightArrApp (deListExp exp1) (deListExp exp2)
deListExp (LeftArrHighApp exp1 exp2) = LeftArrHighApp (deListExp exp1) (deListExp exp2)
deListExp (RightArrHighApp exp1 exp2) = RightArrHighApp (deListExp exp1) (deListExp exp2)
deListExp (LCase alt) = LCase (fmap (deListAlt) alt)
deListExp (ExprHole) = ExprHole
deListExportSpec :: ExportSpec -> ExportSpec
deListExportSpec (EVar qName) = EVar (deListQName qName)
deListExportSpec (EAbs namespace qName) = EAbs (deListNamespace namespace) (deListQName qName)
deListExportSpec (EThingAll qName) = EThingAll (deListQName qName)
deListExportSpec (EThingWith qName cName) = EThingWith (deListQName qName) (fmap (deListCName) cName)
deListExportSpec (EModuleContents moduleName) = EModuleContents (deListModuleName moduleName)
deListFieldUpdate :: FieldUpdate -> FieldUpdate
deListFieldUpdate (FieldUpdate qName exp) = FieldUpdate (deListQName qName) (deListExp exp)
deListFieldUpdate (FieldPun qName) = FieldPun (deListQName qName)
deListFieldUpdate (FieldWildcard) = FieldWildcard
deListFunDep :: FunDep -> FunDep
deListFunDep (FunDep name1 name2) = FunDep (fmap (deListName) name1) (fmap (deListName) name2)
deListGadtDecl :: GadtDecl -> GadtDecl
deListGadtDecl (GadtDecl srcLoc name1 name2 type0) = GadtDecl (deListSrcLoc srcLoc) (deListName name1) (fmap (((fmap (deListName)) *** (deListType))) name2) (deListType type0)
deListGuardedRhs :: GuardedRhs -> GuardedRhs
deListGuardedRhs (GuardedRhs srcLoc stmt exp) = GuardedRhs (deListSrcLoc srcLoc) (fmap (deListStmt) stmt) (deListExp exp)
deListIPBind :: IPBind -> IPBind
deListIPBind (IPBind srcLoc iPName exp) = IPBind (deListSrcLoc srcLoc) (deListIPName iPName) (deListExp exp)
deListIPName :: IPName -> IPName
deListIPName (IPDup string) = IPDup (id string)
deListIPName (IPLin string) = IPLin (id string)
deListImportDecl :: ImportDecl -> ImportDecl
deListImportDecl (ImportDecl importLoc importModule importQualified importSrc importSafe importPkg importAs importSpecs) = ImportDecl (deListSrcLoc importLoc) (deListModuleName importModule) (id importQualified) (id importSrc) (id importSafe) (fmap (id) importPkg) (fmap (deListModuleName) importAs) (fmap (((id) *** (fmap (deListImportSpec)))) importSpecs)
deListImportSpec :: ImportSpec -> ImportSpec
deListImportSpec (IVar name) = IVar (deListName name)
deListImportSpec (IAbs namespace name) = IAbs (deListNamespace namespace) (deListName name)
deListImportSpec (IThingAll name) = IThingAll (deListName name)
deListImportSpec (IThingWith name cName) = IThingWith (deListName name) (fmap (deListCName) cName)
deListInstDecl :: InstDecl -> InstDecl
deListInstDecl (InsDecl decl) = InsDecl (deListDecl decl)
deListInstDecl (InsType srcLoc type1 type2) = InsType (deListSrcLoc srcLoc) (deListType type1) (deListType type2)
deListInstDecl (InsData srcLoc dataOrNew type0 qualConDecl deriving0) = InsData (deListSrcLoc srcLoc) (deListDataOrNew dataOrNew) (deListType type0) (fmap (deListQualConDecl) qualConDecl) (fmap (deListDeriving) deriving0)
deListInstDecl (InsGData srcLoc dataOrNew type0 kind gadtDecl deriving0) = InsGData (deListSrcLoc srcLoc) (deListDataOrNew dataOrNew) (deListType type0) (fmap (deListKind) kind) (fmap (deListGadtDecl) gadtDecl) (fmap (deListDeriving) deriving0)
deListKind :: Kind -> Kind
deListKind (KindStar) = KindStar
deListKind (KindFn kind1 kind2) = KindFn (deListKind kind1) (deListKind kind2)
deListKind (KindParen kind) = KindParen (deListKind kind)
deListKind (KindVar qName) = KindVar (deListQName qName)
deListKind (KindApp kind1 kind2) = KindApp (deListKind kind1) (deListKind kind2)
deListKind (KindTuple kind) = KindTuple (fmap (deListKind) kind)
deListKind (KindList kind) = KindList (deListKind kind)
deListLiteral :: Literal -> Literal
deListLiteral (Char char) = Char (id char)
deListLiteral (String string) = String (id string)
deListLiteral (Int integer) = Int (id integer)
deListLiteral (Frac rational) = Frac (id rational)
deListLiteral (PrimInt integer) = PrimInt (id integer)
deListLiteral (PrimWord integer) = PrimWord (id integer)
deListLiteral (PrimFloat rational) = PrimFloat (id rational)
deListLiteral (PrimDouble rational) = PrimDouble (id rational)
deListLiteral (PrimChar char) = PrimChar (id char)
deListLiteral (PrimString string) = PrimString (id string)
deListMatch :: Match -> Match
deListMatch (Match srcLoc name pat type0 rhs binds) = Match (deListSrcLoc srcLoc) (deListName name) (fmap (deListPat) pat) (fmap (deListType) type0) (deListRhs rhs) (fmap (deListBinds) binds)
deListModule :: Module -> Module
deListModule (Module srcLoc moduleName modulePragma warningText exportSpec importDecl decl) = Module (deListSrcLoc srcLoc) (deListModuleName moduleName) (fmap (deListModulePragma) modulePragma) (fmap (deListWarningText) warningText) (fmap (fmap (deListExportSpec)) exportSpec) (fmap (deListImportDecl) importDecl) (fmap (deListDecl) decl)
deListModuleName :: ModuleName -> ModuleName
deListModuleName (ModuleName string) = ModuleName (id string)
deListModulePragma :: ModulePragma -> ModulePragma
deListModulePragma (LanguagePragma srcLoc name) = LanguagePragma (deListSrcLoc srcLoc) (fmap (deListName) name)
deListModulePragma (OptionsPragma srcLoc tool string) = OptionsPragma (deListSrcLoc srcLoc) (fmap (deListTool) tool) (id string)
deListModulePragma (AnnModulePragma srcLoc annotation) = AnnModulePragma (deListSrcLoc srcLoc) (deListAnnotation annotation)
deListName :: Name -> Name
deListName (Ident string) = Ident (id string)
deListName (Symbol string) = Symbol (id string)
deListNamespace :: Namespace -> Namespace
deListNamespace (NoNamespace) = NoNamespace
deListNamespace (TypeNamespace) = TypeNamespace
deListNamespace (PatternNamespace) = PatternNamespace
deListOp :: Op -> Op
deListOp (VarOp name) = VarOp (deListName name)
deListOp (ConOp name) = ConOp (deListName name)
deListOverlap :: Overlap -> Overlap
deListOverlap (NoOverlap) = NoOverlap
deListOverlap (Overlap) = Overlap
deListOverlap (Incoherent) = Incoherent
deListPXAttr :: PXAttr -> PXAttr
deListPXAttr (PXAttr xName pat) = PXAttr (deListXName xName) (deListPat pat)
deListPat :: Pat -> Pat
deListPat (PVar name) = PVar (deListName name)
deListPat (PLit sign literal) = PLit (deListSign sign) (deListLiteral literal)
deListPat (PNPlusK name integer) = PNPlusK (deListName name) (id integer)
deListPat (PInfixApp pat1 qName pat2) = PInfixApp (deListPat pat1) (deListQName qName) (deListPat pat2)
deListPat (PApp qName pat) = PApp (deListQName qName) (fmap (deListPat) pat)
deListPat (PTuple boxed pat) = PTuple (deListBoxed boxed) (fmap (deListPat) pat)
deListPat (PList pat) = PList (fmap (deListPat) pat)
deListPat (PParen pat) = PParen (deListPat pat)
deListPat (PRec qName patField) = PRec (deListQName qName) (fmap (deListPatField) patField)
deListPat (PAsPat name pat) = PAsPat (deListName name) (deListPat pat)
deListPat (PWildCard) = PWildCard
deListPat (PIrrPat pat) = PIrrPat (deListPat pat)
deListPat (PatTypeSig srcLoc pat type0) = PatTypeSig (deListSrcLoc srcLoc) (deListPat pat) (deListType type0)
deListPat (PViewPat exp pat) = PViewPat (deListExp exp) (deListPat pat)
deListPat (PRPat rPat) = PRPat (fmap (deListRPat) rPat)
deListPat (PXTag srcLoc xName pXAttr pat1 pat2) = PXTag (deListSrcLoc srcLoc) (deListXName xName) (fmap (deListPXAttr) pXAttr) (fmap (deListPat) pat1) (fmap (deListPat) pat2)
deListPat (PXETag srcLoc xName pXAttr pat) = PXETag (deListSrcLoc srcLoc) (deListXName xName) (fmap (deListPXAttr) pXAttr) (fmap (deListPat) pat)
deListPat (PXPcdata string) = PXPcdata (id string)
deListPat (PXPatTag pat) = PXPatTag (deListPat pat)
deListPat (PXRPats rPat) = PXRPats (fmap (deListRPat) rPat)
deListPat (PQuasiQuote string1 string2) = PQuasiQuote (id string1) (id string2)
deListPat (PBangPat pat) = PBangPat (deListPat pat)
deListPatField :: PatField -> PatField
deListPatField (PFieldPat qName pat) = PFieldPat (deListQName qName) (deListPat pat)
deListPatField (PFieldPun qName) = PFieldPun (deListQName qName)
deListPatField (PFieldWildcard) = PFieldWildcard
deListPatternSynDirection :: PatternSynDirection -> PatternSynDirection
deListPatternSynDirection (Unidirectional) = Unidirectional
deListPatternSynDirection (ImplicitBidirectional) = ImplicitBidirectional
deListPatternSynDirection (ExplicitBidirectional decl) = ExplicitBidirectional (fmap (deListDecl) decl)
deListPromoted :: Promoted -> Promoted
deListPromoted (PromotedInteger integer) = PromotedInteger (id integer)
deListPromoted (PromotedString string) = PromotedString (id string)
deListPromoted (PromotedCon bool qName) = PromotedCon (id bool) (deListQName qName)
deListPromoted (PromotedList bool type0) = PromotedList (id bool) (fmap (deListType) type0)
deListPromoted (PromotedTuple type0) = PromotedTuple (fmap (deListType) type0)
deListPromoted (PromotedUnit) = PromotedUnit
deListQName :: QName -> QName
deListQName (Qual moduleName name) = Qual (deListModuleName moduleName) (deListName name)
deListQName (UnQual name) = UnQual (deListName name)
deListQName (Special specialCon) = Special (deListSpecialCon specialCon)
deListQOp :: QOp -> QOp
deListQOp (QVarOp qName) = QVarOp (deListQName qName)
deListQOp (QConOp qName) = QConOp (deListQName qName)
deListQualConDecl :: QualConDecl -> QualConDecl
deListQualConDecl (QualConDecl srcLoc tyVarBind context conDecl) = QualConDecl (deListSrcLoc srcLoc) (fmap (deListTyVarBind) tyVarBind) (deListContext context) (deListConDecl conDecl)
deListQualStmt :: QualStmt -> QualStmt
deListQualStmt (QualStmt stmt) = QualStmt (deListStmt stmt)
deListQualStmt (ThenTrans exp) = ThenTrans (deListExp exp)
deListQualStmt (ThenBy exp1 exp2) = ThenBy (deListExp exp1) (deListExp exp2)
deListQualStmt (GroupBy exp) = GroupBy (deListExp exp)
deListQualStmt (GroupUsing exp) = GroupUsing (deListExp exp)
deListQualStmt (GroupByUsing exp1 exp2) = GroupByUsing (deListExp exp1) (deListExp exp2)
deListRPat :: RPat -> RPat
deListRPat (RPOp rPat rPatOp) = RPOp (deListRPat rPat) (deListRPatOp rPatOp)
deListRPat (RPEither rPat1 rPat2) = RPEither (deListRPat rPat1) (deListRPat rPat2)
deListRPat (RPSeq rPat) = RPSeq (fmap (deListRPat) rPat)
deListRPat (RPGuard pat stmt) = RPGuard (deListPat pat) (fmap (deListStmt) stmt)
deListRPat (RPCAs name rPat) = RPCAs (deListName name) (deListRPat rPat)
deListRPat (RPAs name rPat) = RPAs (deListName name) (deListRPat rPat)
deListRPat (RPParen rPat) = RPParen (deListRPat rPat)
deListRPat (RPPat pat) = RPPat (deListPat pat)
deListRPatOp :: RPatOp -> RPatOp
deListRPatOp (RPStar) = RPStar
deListRPatOp (RPStarG) = RPStarG
deListRPatOp (RPPlus) = RPPlus
deListRPatOp (RPPlusG) = RPPlusG
deListRPatOp (RPOpt) = RPOpt
deListRPatOp (RPOptG) = RPOptG
deListRhs :: Rhs -> Rhs
deListRhs (UnGuardedRhs exp) = UnGuardedRhs (deListExp exp)
deListRhs (GuardedRhss guardedRhs) = GuardedRhss (fmap (deListGuardedRhs) guardedRhs)
deListRole :: Role -> Role
deListRole (Nominal) = Nominal
deListRole (Representational) = Representational
deListRole (Phantom) = Phantom
deListRole (RoleWildcard) = RoleWildcard
deListRule :: Rule -> Rule
deListRule (Rule string activation ruleVar exp1 exp2) = Rule (id string) (deListActivation activation) (fmap (fmap (deListRuleVar)) ruleVar) (deListExp exp1) (deListExp exp2)
deListRuleVar :: RuleVar -> RuleVar
deListRuleVar (RuleVar name) = RuleVar (deListName name)
deListRuleVar (TypedRuleVar name type0) = TypedRuleVar (deListName name) (deListType type0)
deListSafety :: Safety -> Safety
deListSafety (PlayRisky) = PlayRisky
deListSafety (PlaySafe bool) = PlaySafe (id bool)
deListSafety (PlayInterruptible) = PlayInterruptible
deListSign :: Sign -> Sign
deListSign (Signless) = Signless
deListSign (Negative) = Negative
deListSpecialCon :: SpecialCon -> SpecialCon
deListSpecialCon (UnitCon) = UnitCon
deListSpecialCon (ListCon) = ListCon
deListSpecialCon (FunCon) = FunCon
deListSpecialCon (TupleCon boxed int) = TupleCon (deListBoxed boxed) (id int)
deListSpecialCon (Cons) = Cons
deListSpecialCon (UnboxedSingleCon) = UnboxedSingleCon
deListSplice :: Splice -> Splice
deListSplice (IdSplice string) = IdSplice (id string)
deListSplice (ParenSplice exp) = ParenSplice (deListExp exp)
deListStmt :: Stmt -> Stmt
deListStmt (Generator srcLoc pat exp) = Generator (deListSrcLoc srcLoc) (deListPat pat) (deListExp exp)
deListStmt (Qualifier exp) = Qualifier (deListExp exp)
deListStmt (LetStmt binds) = LetStmt (deListBinds binds)
deListStmt (RecStmt stmt) = RecStmt (fmap (deListStmt) stmt)
deListTyVarBind :: TyVarBind -> TyVarBind
deListTyVarBind (KindedVar name kind) = KindedVar (deListName name) (deListKind kind)
deListTyVarBind (UnkindedVar name) = UnkindedVar (deListName name)
deListType :: Type -> Type
deListType (TyForall tyVarBind context type0) = TyForall (fmap (fmap (deListTyVarBind)) tyVarBind) (deListContext context) (deListType type0)
deListType (TyFun type1 type2) = TyFun (deListType type1) (deListType type2)
deListType (TyTuple boxed type0) = TyTuple (deListBoxed boxed) (fmap (deListType) type0)
deListType (TyList type0) = TyList (deListType type0)
deListType (TyParArray type0) = TyParArray (deListType type0)
deListType (TyApp type1 type2) = TyApp (deListType type1) (deListType type2)
deListType (TyVar name) = TyVar (deListName name)
deListType (TyCon qName) = TyCon (deListQName qName)
deListType (TyParen type0) = TyParen (deListType type0)
deListType (TyInfix type1 qName type2) = TyInfix (deListType type1) (deListQName qName) (deListType type2)
deListType (TyKind type0 kind) = TyKind (deListType type0) (deListKind kind)
deListType (TyPromoted promoted) = TyPromoted (deListPromoted promoted)
deListType (TyEquals type1 type2) = TyEquals (deListType type1) (deListType type2)
deListType (TySplice splice) = TySplice (deListSplice splice)
deListType (TyBang bangType type0) = TyBang (deListBangType bangType) (deListType type0)
deListType (TyWildCard name) = TyWildCard (fmap (deListName) name)
deListTypeEqn :: TypeEqn -> TypeEqn
deListTypeEqn (TypeEqn type1 type2) = TypeEqn (deListType type1) (deListType type2)
deListWarningText :: WarningText -> WarningText
deListWarningText (DeprText string) = DeprText (id string)
deListWarningText (WarnText string) = WarnText (id string)
deListXAttr :: XAttr -> XAttr
deListXAttr (XAttr xName exp) = XAttr (deListXName xName) (deListExp exp)
deListXName :: XName -> XName
deListXName (XName string) = XName (id string)
deListXName (XDomName string1 string2) = XDomName (id string1) (id string2)

