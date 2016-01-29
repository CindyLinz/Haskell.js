module Desugar.List where
import Language.Haskell.Exts.Annotated.Syntax
import Control.Arrow ((***))
deListActivation :: Activation l -> Activation l
deListActivation (ActiveFrom l int) = ActiveFrom (id l) (id int)
deListActivation (ActiveUntil l int) = ActiveUntil (id l) (id int)
deListAlt :: Alt l -> Alt l
deListAlt (Alt l pat rhs binds) = Alt (id l) (deListPat pat) (deListRhs rhs) (fmap (deListBinds) binds)
deListAnnotation :: Annotation l -> Annotation l
deListAnnotation (Ann l name exp) = Ann (id l) (deListName name) (deListExp exp)
deListAnnotation (TypeAnn l name exp) = TypeAnn (id l) (deListName name) (deListExp exp)
deListAnnotation (ModuleAnn l exp) = ModuleAnn (id l) (deListExp exp)
deListAssoc :: Assoc l -> Assoc l
deListAssoc (AssocNone l) = AssocNone (id l)
deListAssoc (AssocLeft l) = AssocLeft (id l)
deListAssoc (AssocRight l) = AssocRight (id l)
deListAsst :: Asst l -> Asst l
deListAsst (ClassA l qName type0) = ClassA (id l) (deListQName qName) (fmap (deListType) type0)
deListAsst (AppA l name type0) = AppA (id l) (deListName name) (fmap (deListType) type0)
deListAsst (InfixA l type1 qName type2) = InfixA (id l) (deListType type1) (deListQName qName) (deListType type2)
deListAsst (IParam l iPName type0) = IParam (id l) (deListIPName iPName) (deListType type0)
deListAsst (EqualP l type1 type2) = EqualP (id l) (deListType type1) (deListType type2)
deListAsst (ParenA l asst) = ParenA (id l) (deListAsst asst)
deListAsst (WildCardA l name) = WildCardA (id l) (fmap (deListName) name)
deListBangType :: BangType l -> BangType l
deListBangType (BangedTy l) = BangedTy (id l)
deListBangType (UnpackedTy l) = UnpackedTy (id l)
deListBinds :: Binds l -> Binds l
deListBinds (BDecls l decl) = BDecls (id l) (fmap (deListDecl) decl)
deListBinds (IPBinds l iPBind) = IPBinds (id l) (fmap (deListIPBind) iPBind)
deListBooleanFormula :: BooleanFormula l -> BooleanFormula l
deListBooleanFormula (VarFormula l name) = VarFormula (id l) (deListName name)
deListBooleanFormula (AndFormula l booleanFormula) = AndFormula (id l) (fmap (deListBooleanFormula) booleanFormula)
deListBooleanFormula (OrFormula l booleanFormula) = OrFormula (id l) (fmap (deListBooleanFormula) booleanFormula)
deListBooleanFormula (ParenFormula l booleanFormula) = ParenFormula (id l) (deListBooleanFormula booleanFormula)
deListBoxed :: Boxed -> Boxed
deListBoxed (Boxed) = Boxed
deListBoxed (Unboxed) = Unboxed
deListBracket :: Bracket l -> Bracket l
deListBracket (ExpBracket l exp) = ExpBracket (id l) (deListExp exp)
deListBracket (PatBracket l pat) = PatBracket (id l) (deListPat pat)
deListBracket (TypeBracket l type0) = TypeBracket (id l) (deListType type0)
deListBracket (DeclBracket l decl) = DeclBracket (id l) (fmap (deListDecl) decl)
deListCName :: CName l -> CName l
deListCName (VarName l name) = VarName (id l) (deListName name)
deListCName (ConName l name) = ConName (id l) (deListName name)
deListCallConv :: CallConv l -> CallConv l
deListCallConv (StdCall l) = StdCall (id l)
deListCallConv (CCall l) = CCall (id l)
deListCallConv (CPlusPlus l) = CPlusPlus (id l)
deListCallConv (DotNet l) = DotNet (id l)
deListCallConv (Jvm l) = Jvm (id l)
deListCallConv (Js l) = Js (id l)
deListCallConv (JavaScript l) = JavaScript (id l)
deListCallConv (CApi l) = CApi (id l)
deListClassDecl :: ClassDecl l -> ClassDecl l
deListClassDecl (ClsDecl l decl) = ClsDecl (id l) (deListDecl decl)
deListClassDecl (ClsDataFam l context declHead kind) = ClsDataFam (id l) (fmap (deListContext) context) (deListDeclHead declHead) (fmap (deListKind) kind)
deListClassDecl (ClsTyFam l declHead kind) = ClsTyFam (id l) (deListDeclHead declHead) (fmap (deListKind) kind)
deListClassDecl (ClsTyDef l type1 type2) = ClsTyDef (id l) (deListType type1) (deListType type2)
deListClassDecl (ClsDefSig l name type0) = ClsDefSig (id l) (deListName name) (deListType type0)
deListConDecl :: ConDecl l -> ConDecl l
deListConDecl (ConDecl l name type0) = ConDecl (id l) (deListName name) (fmap (deListType) type0)
deListConDecl (InfixConDecl l type1 name type2) = InfixConDecl (id l) (deListType type1) (deListName name) (deListType type2)
deListConDecl (RecDecl l name fieldDecl) = RecDecl (id l) (deListName name) (fmap (deListFieldDecl) fieldDecl)
deListContext :: Context l -> Context l
deListContext (CxSingle l asst) = CxSingle (id l) (deListAsst asst)
deListContext (CxTuple l asst) = CxTuple (id l) (fmap (deListAsst) asst)
deListContext (CxEmpty l) = CxEmpty (id l)
deListDataOrNew :: DataOrNew l -> DataOrNew l
deListDataOrNew (DataType l) = DataType (id l)
deListDataOrNew (NewType l) = NewType (id l)
deListDecl :: Decl l -> Decl l
deListDecl (TypeDecl l declHead type0) = TypeDecl (id l) (deListDeclHead declHead) (deListType type0)
deListDecl (TypeFamDecl l declHead kind) = TypeFamDecl (id l) (deListDeclHead declHead) (fmap (deListKind) kind)
deListDecl (ClosedTypeFamDecl l declHead kind typeEqn) = ClosedTypeFamDecl (id l) (deListDeclHead declHead) (fmap (deListKind) kind) (fmap (deListTypeEqn) typeEqn)
deListDecl (DataDecl l dataOrNew context declHead qualConDecl deriving0) = DataDecl (id l) (deListDataOrNew dataOrNew) (fmap (deListContext) context) (deListDeclHead declHead) (fmap (deListQualConDecl) qualConDecl) (fmap (deListDeriving) deriving0)
deListDecl (GDataDecl l dataOrNew context declHead kind gadtDecl deriving0) = GDataDecl (id l) (deListDataOrNew dataOrNew) (fmap (deListContext) context) (deListDeclHead declHead) (fmap (deListKind) kind) (fmap (deListGadtDecl) gadtDecl) (fmap (deListDeriving) deriving0)
deListDecl (DataFamDecl l context declHead kind) = DataFamDecl (id l) (fmap (deListContext) context) (deListDeclHead declHead) (fmap (deListKind) kind)
deListDecl (TypeInsDecl l type1 type2) = TypeInsDecl (id l) (deListType type1) (deListType type2)
deListDecl (DataInsDecl l dataOrNew type0 qualConDecl deriving0) = DataInsDecl (id l) (deListDataOrNew dataOrNew) (deListType type0) (fmap (deListQualConDecl) qualConDecl) (fmap (deListDeriving) deriving0)
deListDecl (GDataInsDecl l dataOrNew type0 kind gadtDecl deriving0) = GDataInsDecl (id l) (deListDataOrNew dataOrNew) (deListType type0) (fmap (deListKind) kind) (fmap (deListGadtDecl) gadtDecl) (fmap (deListDeriving) deriving0)
deListDecl (ClassDecl l context declHead funDep classDecl) = ClassDecl (id l) (fmap (deListContext) context) (deListDeclHead declHead) (fmap (deListFunDep) funDep) (fmap (fmap (deListClassDecl)) classDecl)
deListDecl (InstDecl l overlap instRule instDecl) = InstDecl (id l) (fmap (deListOverlap) overlap) (deListInstRule instRule) (fmap (fmap (deListInstDecl)) instDecl)
deListDecl (DerivDecl l overlap instRule) = DerivDecl (id l) (fmap (deListOverlap) overlap) (deListInstRule instRule)
deListDecl (InfixDecl l assoc int op) = InfixDecl (id l) (deListAssoc assoc) (fmap (id) int) (fmap (deListOp) op)
deListDecl (DefaultDecl l type0) = DefaultDecl (id l) (fmap (deListType) type0)
deListDecl (SpliceDecl l exp) = SpliceDecl (id l) (deListExp exp)
deListDecl (TypeSig l name type0) = TypeSig (id l) (fmap (deListName) name) (deListType type0)
deListDecl (PatSynSig l name tyVarBind context1 context2 type0) = PatSynSig (id l) (deListName name) (fmap (fmap (deListTyVarBind)) tyVarBind) (fmap (deListContext) context1) (fmap (deListContext) context2) (deListType type0)
deListDecl (FunBind l match) = FunBind (id l) (fmap (deListMatch) match)
deListDecl (PatBind l pat rhs binds) = PatBind (id l) (deListPat pat) (deListRhs rhs) (fmap (deListBinds) binds)
deListDecl (PatSyn l pat1 pat2 patternSynDirection) = PatSyn (id l) (deListPat pat1) (deListPat pat2) (deListPatternSynDirection patternSynDirection)
deListDecl (ForImp l callConv safety string name type0) = ForImp (id l) (deListCallConv callConv) (fmap (deListSafety) safety) (fmap (id) string) (deListName name) (deListType type0)
deListDecl (ForExp l callConv string name type0) = ForExp (id l) (deListCallConv callConv) (fmap (id) string) (deListName name) (deListType type0)
deListDecl (RulePragmaDecl l rule) = RulePragmaDecl (id l) (fmap (deListRule) rule)
deListDecl (DeprPragmaDecl l name) = DeprPragmaDecl (id l) (fmap (((fmap (deListName)) *** (id))) name)
deListDecl (WarnPragmaDecl l name) = WarnPragmaDecl (id l) (fmap (((fmap (deListName)) *** (id))) name)
deListDecl (InlineSig l bool activation qName) = InlineSig (id l) (id bool) (fmap (deListActivation) activation) (deListQName qName)
deListDecl (InlineConlikeSig l activation qName) = InlineConlikeSig (id l) (fmap (deListActivation) activation) (deListQName qName)
deListDecl (SpecSig l activation qName type0) = SpecSig (id l) (fmap (deListActivation) activation) (deListQName qName) (fmap (deListType) type0)
deListDecl (SpecInlineSig l bool activation qName type0) = SpecInlineSig (id l) (id bool) (fmap (deListActivation) activation) (deListQName qName) (fmap (deListType) type0)
deListDecl (InstSig l instRule) = InstSig (id l) (deListInstRule instRule)
deListDecl (AnnPragma l annotation) = AnnPragma (id l) (deListAnnotation annotation)
deListDecl (MinimalPragma l booleanFormula) = MinimalPragma (id l) (fmap (deListBooleanFormula) booleanFormula)
deListDecl (RoleAnnotDecl l qName role) = RoleAnnotDecl (id l) (deListQName qName) (fmap (deListRole) role)
deListDeclHead :: DeclHead l -> DeclHead l
deListDeclHead (DHead l name) = DHead (id l) (deListName name)
deListDeclHead (DHInfix l tyVarBind name) = DHInfix (id l) (deListTyVarBind tyVarBind) (deListName name)
deListDeclHead (DHParen l declHead) = DHParen (id l) (deListDeclHead declHead)
deListDeclHead (DHApp l declHead tyVarBind) = DHApp (id l) (deListDeclHead declHead) (deListTyVarBind tyVarBind)
deListDeriving :: Deriving l -> Deriving l
deListDeriving (Deriving l instRule) = Deriving (id l) (fmap (deListInstRule) instRule)
deListExp :: Exp l -> Exp l
deListExp (Var l qName) = Var (id l) (deListQName qName)
deListExp (IPVar l iPName) = IPVar (id l) (deListIPName iPName)
deListExp (Con l qName) = Con (id l) (deListQName qName)
deListExp (Lit l literal) = Lit (id l) (deListLiteral literal)
deListExp (InfixApp l exp1 qOp exp2) = InfixApp (id l) (deListExp exp1) (deListQOp qOp) (deListExp exp2)
deListExp (App l exp1 exp2) = App (id l) (deListExp exp1) (deListExp exp2)
deListExp (NegApp l exp) = NegApp (id l) (deListExp exp)
deListExp (Lambda l pat exp) = Lambda (id l) (fmap (deListPat) pat) (deListExp exp)
deListExp (Let l binds exp) = Let (id l) (deListBinds binds) (deListExp exp)
deListExp (If l exp1 exp2 exp3) = If (id l) (deListExp exp1) (deListExp exp2) (deListExp exp3)
deListExp (MultiIf l guardedRhs) = MultiIf (id l) (fmap (deListGuardedRhs) guardedRhs)
deListExp (Case l exp alt) = Case (id l) (deListExp exp) (fmap (deListAlt) alt)
deListExp (Do l stmt) = Do (id l) (fmap (deListStmt) stmt)
deListExp (MDo l stmt) = MDo (id l) (fmap (deListStmt) stmt)
deListExp (Tuple l boxed exp) = Tuple (id l) (deListBoxed boxed) (fmap (deListExp) exp)
deListExp (TupleSection l boxed exp) = TupleSection (id l) (deListBoxed boxed) (fmap (fmap (deListExp)) exp)
deListExp (List l []) = List l []
deListExp (List l (e:es)) = deListExp (App l (App l (Con l (Special l (Cons l))) e) (List l es))
deListExp (ParArray l exp) = ParArray (id l) (fmap (deListExp) exp)
deListExp (Paren l exp) = Paren (id l) (deListExp exp)
deListExp (LeftSection l exp qOp) = LeftSection (id l) (deListExp exp) (deListQOp qOp)
deListExp (RightSection l qOp exp) = RightSection (id l) (deListQOp qOp) (deListExp exp)
deListExp (RecConstr l qName fieldUpdate) = RecConstr (id l) (deListQName qName) (fmap (deListFieldUpdate) fieldUpdate)
deListExp (RecUpdate l exp fieldUpdate) = RecUpdate (id l) (deListExp exp) (fmap (deListFieldUpdate) fieldUpdate)
deListExp (EnumFrom l exp) = EnumFrom (id l) (deListExp exp)
deListExp (EnumFromTo l exp1 exp2) = EnumFromTo (id l) (deListExp exp1) (deListExp exp2)
deListExp (EnumFromThen l exp1 exp2) = EnumFromThen (id l) (deListExp exp1) (deListExp exp2)
deListExp (EnumFromThenTo l exp1 exp2 exp3) = EnumFromThenTo (id l) (deListExp exp1) (deListExp exp2) (deListExp exp3)
deListExp (ParArrayFromTo l exp1 exp2) = ParArrayFromTo (id l) (deListExp exp1) (deListExp exp2)
deListExp (ParArrayFromThenTo l exp1 exp2 exp3) = ParArrayFromThenTo (id l) (deListExp exp1) (deListExp exp2) (deListExp exp3)
deListExp (ListComp l exp qualStmt) = ListComp (id l) (deListExp exp) (fmap (deListQualStmt) qualStmt)
deListExp (ParComp l exp qualStmt) = ParComp (id l) (deListExp exp) (fmap (fmap (deListQualStmt)) qualStmt)
deListExp (ParArrayComp l exp qualStmt) = ParArrayComp (id l) (deListExp exp) (fmap (fmap (deListQualStmt)) qualStmt)
deListExp (ExpTypeSig l exp type0) = ExpTypeSig (id l) (deListExp exp) (deListType type0)
deListExp (VarQuote l qName) = VarQuote (id l) (deListQName qName)
deListExp (TypQuote l qName) = TypQuote (id l) (deListQName qName)
deListExp (BracketExp l bracket) = BracketExp (id l) (deListBracket bracket)
deListExp (SpliceExp l splice) = SpliceExp (id l) (deListSplice splice)
deListExp (QuasiQuote l string1 string2) = QuasiQuote (id l) (id string1) (id string2)
deListExp (XTag l xName xAttr exp1 exp2) = XTag (id l) (deListXName xName) (fmap (deListXAttr) xAttr) (fmap (deListExp) exp1) (fmap (deListExp) exp2)
deListExp (XETag l xName xAttr exp) = XETag (id l) (deListXName xName) (fmap (deListXAttr) xAttr) (fmap (deListExp) exp)
deListExp (XPcdata l string) = XPcdata (id l) (id string)
deListExp (XExpTag l exp) = XExpTag (id l) (deListExp exp)
deListExp (XChildTag l exp) = XChildTag (id l) (fmap (deListExp) exp)
deListExp (CorePragma l string exp) = CorePragma (id l) (id string) (deListExp exp)
deListExp (SCCPragma l string exp) = SCCPragma (id l) (id string) (deListExp exp)
deListExp (GenPragma l string int1 int2 exp) = GenPragma (id l) (id string) (((id) *** (id)) int1) (((id) *** (id)) int2) (deListExp exp)
deListExp (Proc l pat exp) = Proc (id l) (deListPat pat) (deListExp exp)
deListExp (LeftArrApp l exp1 exp2) = LeftArrApp (id l) (deListExp exp1) (deListExp exp2)
deListExp (RightArrApp l exp1 exp2) = RightArrApp (id l) (deListExp exp1) (deListExp exp2)
deListExp (LeftArrHighApp l exp1 exp2) = LeftArrHighApp (id l) (deListExp exp1) (deListExp exp2)
deListExp (RightArrHighApp l exp1 exp2) = RightArrHighApp (id l) (deListExp exp1) (deListExp exp2)
deListExp (LCase l alt) = LCase (id l) (fmap (deListAlt) alt)
deListExp (ExprHole l) = ExprHole (id l)
deListExportSpec :: ExportSpec l -> ExportSpec l
deListExportSpec (EVar l qName) = EVar (id l) (deListQName qName)
deListExportSpec (EAbs l namespace qName) = EAbs (id l) (deListNamespace namespace) (deListQName qName)
deListExportSpec (EThingAll l qName) = EThingAll (id l) (deListQName qName)
deListExportSpec (EThingWith l qName cName) = EThingWith (id l) (deListQName qName) (fmap (deListCName) cName)
deListExportSpec (EModuleContents l moduleName) = EModuleContents (id l) (deListModuleName moduleName)
deListExportSpecList :: ExportSpecList l -> ExportSpecList l
deListExportSpecList (ExportSpecList l exportSpec) = ExportSpecList (id l) (fmap (deListExportSpec) exportSpec)
deListFieldDecl :: FieldDecl l -> FieldDecl l
deListFieldDecl (FieldDecl l name type0) = FieldDecl (id l) (fmap (deListName) name) (deListType type0)
deListFieldUpdate :: FieldUpdate l -> FieldUpdate l
deListFieldUpdate (FieldUpdate l qName exp) = FieldUpdate (id l) (deListQName qName) (deListExp exp)
deListFieldUpdate (FieldPun l qName) = FieldPun (id l) (deListQName qName)
deListFieldUpdate (FieldWildcard l) = FieldWildcard (id l)
deListFunDep :: FunDep l -> FunDep l
deListFunDep (FunDep l name1 name2) = FunDep (id l) (fmap (deListName) name1) (fmap (deListName) name2)
deListGadtDecl :: GadtDecl l -> GadtDecl l
deListGadtDecl (GadtDecl l name fieldDecl type0) = GadtDecl (id l) (deListName name) (fmap (fmap (deListFieldDecl)) fieldDecl) (deListType type0)
deListGuardedRhs :: GuardedRhs l -> GuardedRhs l
deListGuardedRhs (GuardedRhs l stmt exp) = GuardedRhs (id l) (fmap (deListStmt) stmt) (deListExp exp)
deListIPBind :: IPBind l -> IPBind l
deListIPBind (IPBind l iPName exp) = IPBind (id l) (deListIPName iPName) (deListExp exp)
deListIPName :: IPName l -> IPName l
deListIPName (IPDup l string) = IPDup (id l) (id string)
deListIPName (IPLin l string) = IPLin (id l) (id string)
deListImportDecl :: ImportDecl l -> ImportDecl l
deListImportDecl (ImportDecl importAnn importModule importQualified importSrc importSafe importPkg importAs importSpecs) = ImportDecl (id importAnn) (deListModuleName importModule) (id importQualified) (id importSrc) (id importSafe) (fmap (id) importPkg) (fmap (deListModuleName) importAs) (fmap (deListImportSpecList) importSpecs)
deListImportSpec :: ImportSpec l -> ImportSpec l
deListImportSpec (IVar l name) = IVar (id l) (deListName name)
deListImportSpec (IAbs l namespace name) = IAbs (id l) (deListNamespace namespace) (deListName name)
deListImportSpec (IThingAll l name) = IThingAll (id l) (deListName name)
deListImportSpec (IThingWith l name cName) = IThingWith (id l) (deListName name) (fmap (deListCName) cName)
deListImportSpecList :: ImportSpecList l -> ImportSpecList l
deListImportSpecList (ImportSpecList l bool importSpec) = ImportSpecList (id l) (id bool) (fmap (deListImportSpec) importSpec)
deListInstDecl :: InstDecl l -> InstDecl l
deListInstDecl (InsDecl l decl) = InsDecl (id l) (deListDecl decl)
deListInstDecl (InsType l type1 type2) = InsType (id l) (deListType type1) (deListType type2)
deListInstDecl (InsData l dataOrNew type0 qualConDecl deriving0) = InsData (id l) (deListDataOrNew dataOrNew) (deListType type0) (fmap (deListQualConDecl) qualConDecl) (fmap (deListDeriving) deriving0)
deListInstDecl (InsGData l dataOrNew type0 kind gadtDecl deriving0) = InsGData (id l) (deListDataOrNew dataOrNew) (deListType type0) (fmap (deListKind) kind) (fmap (deListGadtDecl) gadtDecl) (fmap (deListDeriving) deriving0)
deListInstHead :: InstHead l -> InstHead l
deListInstHead (IHCon l qName) = IHCon (id l) (deListQName qName)
deListInstHead (IHInfix l type0 qName) = IHInfix (id l) (deListType type0) (deListQName qName)
deListInstHead (IHParen l instHead) = IHParen (id l) (deListInstHead instHead)
deListInstHead (IHApp l instHead type0) = IHApp (id l) (deListInstHead instHead) (deListType type0)
deListInstRule :: InstRule l -> InstRule l
deListInstRule (IRule l tyVarBind context instHead) = IRule (id l) (fmap (fmap (deListTyVarBind)) tyVarBind) (fmap (deListContext) context) (deListInstHead instHead)
deListInstRule (IParen l instRule) = IParen (id l) (deListInstRule instRule)
deListKind :: Kind l -> Kind l
deListKind (KindStar l) = KindStar (id l)
deListKind (KindFn l kind1 kind2) = KindFn (id l) (deListKind kind1) (deListKind kind2)
deListKind (KindParen l kind) = KindParen (id l) (deListKind kind)
deListKind (KindVar l qName) = KindVar (id l) (deListQName qName)
deListKind (KindApp l kind1 kind2) = KindApp (id l) (deListKind kind1) (deListKind kind2)
deListKind (KindTuple l kind) = KindTuple (id l) (fmap (deListKind) kind)
deListKind (KindList l kind) = KindList (id l) (deListKind kind)
deListLiteral :: Literal l -> Literal l
deListLiteral (Char l char string) = Char (id l) (id char) (id string)
deListLiteral (String l string1 string2) = String (id l) (id string1) (id string2)
deListLiteral (Int l integer string) = Int (id l) (id integer) (id string)
deListLiteral (Frac l rational string) = Frac (id l) (id rational) (id string)
deListLiteral (PrimInt l integer string) = PrimInt (id l) (id integer) (id string)
deListLiteral (PrimWord l integer string) = PrimWord (id l) (id integer) (id string)
deListLiteral (PrimFloat l rational string) = PrimFloat (id l) (id rational) (id string)
deListLiteral (PrimDouble l rational string) = PrimDouble (id l) (id rational) (id string)
deListLiteral (PrimChar l char string) = PrimChar (id l) (id char) (id string)
deListLiteral (PrimString l string1 string2) = PrimString (id l) (id string1) (id string2)
deListMatch :: Match l -> Match l
deListMatch (Match l name pat rhs binds) = Match (id l) (deListName name) (fmap (deListPat) pat) (deListRhs rhs) (fmap (deListBinds) binds)
deListMatch (InfixMatch l pat1 name pat2 rhs binds) = InfixMatch (id l) (deListPat pat1) (deListName name) (fmap (deListPat) pat2) (deListRhs rhs) (fmap (deListBinds) binds)
deListModule :: Module l -> Module l
deListModule (Module l moduleHead modulePragma importDecl decl) = Module (id l) (fmap (deListModuleHead) moduleHead) (fmap (deListModulePragma) modulePragma) (fmap (deListImportDecl) importDecl) (fmap (deListDecl) decl)
deListModule (XmlPage l moduleName modulePragma xName xAttr exp1 exp2) = XmlPage (id l) (deListModuleName moduleName) (fmap (deListModulePragma) modulePragma) (deListXName xName) (fmap (deListXAttr) xAttr) (fmap (deListExp) exp1) (fmap (deListExp) exp2)
deListModule (XmlHybrid l moduleHead modulePragma importDecl decl xName xAttr exp1 exp2) = XmlHybrid (id l) (fmap (deListModuleHead) moduleHead) (fmap (deListModulePragma) modulePragma) (fmap (deListImportDecl) importDecl) (fmap (deListDecl) decl) (deListXName xName) (fmap (deListXAttr) xAttr) (fmap (deListExp) exp1) (fmap (deListExp) exp2)
deListModuleHead :: ModuleHead l -> ModuleHead l
deListModuleHead (ModuleHead l moduleName warningText exportSpecList) = ModuleHead (id l) (deListModuleName moduleName) (fmap (deListWarningText) warningText) (fmap (deListExportSpecList) exportSpecList)
deListModuleName :: ModuleName l -> ModuleName l
deListModuleName (ModuleName l string) = ModuleName (id l) (id string)
deListModulePragma :: ModulePragma l -> ModulePragma l
deListModulePragma (LanguagePragma l name) = LanguagePragma (id l) (fmap (deListName) name)
deListModulePragma (OptionsPragma l tool string) = OptionsPragma (id l) (fmap (deListTool) tool) (id string)
deListModulePragma (AnnModulePragma l annotation) = AnnModulePragma (id l) (deListAnnotation annotation)
deListName :: Name l -> Name l
deListName (Ident l string) = Ident (id l) (id string)
deListName (Symbol l string) = Symbol (id l) (id string)
deListNamespace :: Namespace l -> Namespace l
deListNamespace (NoNamespace l) = NoNamespace (id l)
deListNamespace (TypeNamespace l) = TypeNamespace (id l)
deListNamespace (PatternNamespace l) = PatternNamespace (id l)
deListOp :: Op l -> Op l
deListOp (VarOp l name) = VarOp (id l) (deListName name)
deListOp (ConOp l name) = ConOp (id l) (deListName name)
deListOverlap :: Overlap l -> Overlap l
deListOverlap (NoOverlap l) = NoOverlap (id l)
deListOverlap (Overlap l) = Overlap (id l)
deListOverlap (Incoherent l) = Incoherent (id l)
deListPXAttr :: PXAttr l -> PXAttr l
deListPXAttr (PXAttr l xName pat) = PXAttr (id l) (deListXName xName) (deListPat pat)
deListPat :: Pat l -> Pat l
deListPat (PVar l name) = PVar (id l) (deListName name)
deListPat (PLit l sign literal) = PLit (id l) (deListSign sign) (deListLiteral literal)
deListPat (PNPlusK l name integer) = PNPlusK (id l) (deListName name) (id integer)
deListPat (PInfixApp l pat1 qName pat2) = PInfixApp (id l) (deListPat pat1) (deListQName qName) (deListPat pat2)
deListPat (PApp l qName pat) = PApp (id l) (deListQName qName) (fmap (deListPat) pat)
deListPat (PTuple l boxed pat) = PTuple (id l) (deListBoxed boxed) (fmap (deListPat) pat)
deListPat (PList l pat) = PList (id l) (fmap (deListPat) pat)
deListPat (PParen l pat) = PParen (id l) (deListPat pat)
deListPat (PRec l qName patField) = PRec (id l) (deListQName qName) (fmap (deListPatField) patField)
deListPat (PAsPat l name pat) = PAsPat (id l) (deListName name) (deListPat pat)
deListPat (PWildCard l) = PWildCard (id l)
deListPat (PIrrPat l pat) = PIrrPat (id l) (deListPat pat)
deListPat (PatTypeSig l pat type0) = PatTypeSig (id l) (deListPat pat) (deListType type0)
deListPat (PViewPat l exp pat) = PViewPat (id l) (deListExp exp) (deListPat pat)
deListPat (PRPat l rPat) = PRPat (id l) (fmap (deListRPat) rPat)
deListPat (PXTag l xName pXAttr pat1 pat2) = PXTag (id l) (deListXName xName) (fmap (deListPXAttr) pXAttr) (fmap (deListPat) pat1) (fmap (deListPat) pat2)
deListPat (PXETag l xName pXAttr pat) = PXETag (id l) (deListXName xName) (fmap (deListPXAttr) pXAttr) (fmap (deListPat) pat)
deListPat (PXPcdata l string) = PXPcdata (id l) (id string)
deListPat (PXPatTag l pat) = PXPatTag (id l) (deListPat pat)
deListPat (PXRPats l rPat) = PXRPats (id l) (fmap (deListRPat) rPat)
deListPat (PQuasiQuote l string1 string2) = PQuasiQuote (id l) (id string1) (id string2)
deListPat (PBangPat l pat) = PBangPat (id l) (deListPat pat)
deListPatField :: PatField l -> PatField l
deListPatField (PFieldPat l qName pat) = PFieldPat (id l) (deListQName qName) (deListPat pat)
deListPatField (PFieldPun l qName) = PFieldPun (id l) (deListQName qName)
deListPatField (PFieldWildcard l) = PFieldWildcard (id l)
deListPatternSynDirection :: PatternSynDirection l -> PatternSynDirection l
deListPatternSynDirection (Unidirectional) = Unidirectional
deListPatternSynDirection (ImplicitBidirectional) = ImplicitBidirectional
deListPatternSynDirection (ExplicitBidirectional l decl) = ExplicitBidirectional (id l) (fmap (deListDecl) decl)
deListPromoted :: Promoted l -> Promoted l
deListPromoted (PromotedInteger l integer string) = PromotedInteger (id l) (id integer) (id string)
deListPromoted (PromotedString l string1 string2) = PromotedString (id l) (id string1) (id string2)
deListPromoted (PromotedCon l bool qName) = PromotedCon (id l) (id bool) (deListQName qName)
deListPromoted (PromotedList l bool type0) = PromotedList (id l) (id bool) (fmap (deListType) type0)
deListPromoted (PromotedTuple l type0) = PromotedTuple (id l) (fmap (deListType) type0)
deListPromoted (PromotedUnit l) = PromotedUnit (id l)
deListQName :: QName l -> QName l
deListQName (Qual l moduleName name) = Qual (id l) (deListModuleName moduleName) (deListName name)
deListQName (UnQual l name) = UnQual (id l) (deListName name)
deListQName (Special l specialCon) = Special (id l) (deListSpecialCon specialCon)
deListQOp :: QOp l -> QOp l
deListQOp (QVarOp l qName) = QVarOp (id l) (deListQName qName)
deListQOp (QConOp l qName) = QConOp (id l) (deListQName qName)
deListQualConDecl :: QualConDecl l -> QualConDecl l
deListQualConDecl (QualConDecl l tyVarBind context conDecl) = QualConDecl (id l) (fmap (fmap (deListTyVarBind)) tyVarBind) (fmap (deListContext) context) (deListConDecl conDecl)
deListQualStmt :: QualStmt l -> QualStmt l
deListQualStmt (QualStmt l stmt) = QualStmt (id l) (deListStmt stmt)
deListQualStmt (ThenTrans l exp) = ThenTrans (id l) (deListExp exp)
deListQualStmt (ThenBy l exp1 exp2) = ThenBy (id l) (deListExp exp1) (deListExp exp2)
deListQualStmt (GroupBy l exp) = GroupBy (id l) (deListExp exp)
deListQualStmt (GroupUsing l exp) = GroupUsing (id l) (deListExp exp)
deListQualStmt (GroupByUsing l exp1 exp2) = GroupByUsing (id l) (deListExp exp1) (deListExp exp2)
deListRPat :: RPat l -> RPat l
deListRPat (RPOp l rPat rPatOp) = RPOp (id l) (deListRPat rPat) (deListRPatOp rPatOp)
deListRPat (RPEither l rPat1 rPat2) = RPEither (id l) (deListRPat rPat1) (deListRPat rPat2)
deListRPat (RPSeq l rPat) = RPSeq (id l) (fmap (deListRPat) rPat)
deListRPat (RPGuard l pat stmt) = RPGuard (id l) (deListPat pat) (fmap (deListStmt) stmt)
deListRPat (RPCAs l name rPat) = RPCAs (id l) (deListName name) (deListRPat rPat)
deListRPat (RPAs l name rPat) = RPAs (id l) (deListName name) (deListRPat rPat)
deListRPat (RPParen l rPat) = RPParen (id l) (deListRPat rPat)
deListRPat (RPPat l pat) = RPPat (id l) (deListPat pat)
deListRPatOp :: RPatOp l -> RPatOp l
deListRPatOp (RPStar l) = RPStar (id l)
deListRPatOp (RPStarG l) = RPStarG (id l)
deListRPatOp (RPPlus l) = RPPlus (id l)
deListRPatOp (RPPlusG l) = RPPlusG (id l)
deListRPatOp (RPOpt l) = RPOpt (id l)
deListRPatOp (RPOptG l) = RPOptG (id l)
deListRhs :: Rhs l -> Rhs l
deListRhs (UnGuardedRhs l exp) = UnGuardedRhs (id l) (deListExp exp)
deListRhs (GuardedRhss l guardedRhs) = GuardedRhss (id l) (fmap (deListGuardedRhs) guardedRhs)
deListRole :: Role l -> Role l
deListRole (Nominal l) = Nominal (id l)
deListRole (Representational l) = Representational (id l)
deListRole (Phantom l) = Phantom (id l)
deListRole (RoleWildcard l) = RoleWildcard (id l)
deListRule :: Rule l -> Rule l
deListRule (Rule l string activation ruleVar exp1 exp2) = Rule (id l) (id string) (fmap (deListActivation) activation) (fmap (fmap (deListRuleVar)) ruleVar) (deListExp exp1) (deListExp exp2)
deListRuleVar :: RuleVar l -> RuleVar l
deListRuleVar (RuleVar l name) = RuleVar (id l) (deListName name)
deListRuleVar (TypedRuleVar l name type0) = TypedRuleVar (id l) (deListName name) (deListType type0)
deListSafety :: Safety l -> Safety l
deListSafety (PlayRisky l) = PlayRisky (id l)
deListSafety (PlaySafe l bool) = PlaySafe (id l) (id bool)
deListSafety (PlayInterruptible l) = PlayInterruptible (id l)
deListSign :: Sign l -> Sign l
deListSign (Signless l) = Signless (id l)
deListSign (Negative l) = Negative (id l)
deListSpecialCon :: SpecialCon l -> SpecialCon l
deListSpecialCon (UnitCon l) = UnitCon (id l)
deListSpecialCon (ListCon l) = ListCon (id l)
deListSpecialCon (FunCon l) = FunCon (id l)
deListSpecialCon (TupleCon l boxed int) = TupleCon (id l) (deListBoxed boxed) (id int)
deListSpecialCon (Cons l) = Cons (id l)
deListSpecialCon (UnboxedSingleCon l) = UnboxedSingleCon (id l)
deListSplice :: Splice l -> Splice l
deListSplice (IdSplice l string) = IdSplice (id l) (id string)
deListSplice (ParenSplice l exp) = ParenSplice (id l) (deListExp exp)
deListStmt :: Stmt l -> Stmt l
deListStmt (Generator l pat exp) = Generator (id l) (deListPat pat) (deListExp exp)
deListStmt (Qualifier l exp) = Qualifier (id l) (deListExp exp)
deListStmt (LetStmt l binds) = LetStmt (id l) (deListBinds binds)
deListStmt (RecStmt l stmt) = RecStmt (id l) (fmap (deListStmt) stmt)
deListTool :: Tool -> Tool
deListTool (GHC) = GHC
deListTool (HUGS) = HUGS
deListTool (NHC98) = NHC98
deListTool (YHC) = YHC
deListTool (HADDOCK) = HADDOCK
deListTool (UnknownTool string) = UnknownTool (id string)
deListTyVarBind :: TyVarBind l -> TyVarBind l
deListTyVarBind (KindedVar l name kind) = KindedVar (id l) (deListName name) (deListKind kind)
deListTyVarBind (UnkindedVar l name) = UnkindedVar (id l) (deListName name)
deListType :: Type l -> Type l
deListType (TyForall l tyVarBind context type0) = TyForall (id l) (fmap (fmap (deListTyVarBind)) tyVarBind) (fmap (deListContext) context) (deListType type0)
deListType (TyFun l type1 type2) = TyFun (id l) (deListType type1) (deListType type2)
deListType (TyTuple l boxed type0) = TyTuple (id l) (deListBoxed boxed) (fmap (deListType) type0)
deListType (TyList l type0) = TyList (id l) (deListType type0)
deListType (TyParArray l type0) = TyParArray (id l) (deListType type0)
deListType (TyApp l type1 type2) = TyApp (id l) (deListType type1) (deListType type2)
deListType (TyVar l name) = TyVar (id l) (deListName name)
deListType (TyCon l qName) = TyCon (id l) (deListQName qName)
deListType (TyParen l type0) = TyParen (id l) (deListType type0)
deListType (TyInfix l type1 qName type2) = TyInfix (id l) (deListType type1) (deListQName qName) (deListType type2)
deListType (TyKind l type0 kind) = TyKind (id l) (deListType type0) (deListKind kind)
deListType (TyPromoted l promoted) = TyPromoted (id l) (deListPromoted promoted)
deListType (TyEquals l type1 type2) = TyEquals (id l) (deListType type1) (deListType type2)
deListType (TySplice l splice) = TySplice (id l) (deListSplice splice)
deListType (TyBang l bangType type0) = TyBang (id l) (deListBangType bangType) (deListType type0)
deListType (TyWildCard l name) = TyWildCard (id l) (fmap (deListName) name)
deListTypeEqn :: TypeEqn l -> TypeEqn l
deListTypeEqn (TypeEqn l type1 type2) = TypeEqn (id l) (deListType type1) (deListType type2)
deListWarningText :: WarningText l -> WarningText l
deListWarningText (DeprText l string) = DeprText (id l) (id string)
deListWarningText (WarnText l string) = WarnText (id l) (id string)
deListXAttr :: XAttr l -> XAttr l
deListXAttr (XAttr l xName exp) = XAttr (id l) (deListXName xName) (deListExp exp)
deListXName :: XName l -> XName l
deListXName (XName l string) = XName (id l) (id string)
deListXName (XDomName l string1 string2) = XDomName (id l) (id string1) (id string2)

