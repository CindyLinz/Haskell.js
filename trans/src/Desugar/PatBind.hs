module Desugar.PatBind where
import Language.Haskell.Exts.Annotated.Syntax
import Control.Arrow ((***))

patVars :: Pat l -> [Name l]
patVars = go [] where
  infixl 0 `go`
  go acc (PVar _ name) = name : acc
  go acc (PLit _ _ _) = acc
  go acc (PNPlusK _ name _) = name : acc
  go acc (PInfixApp _ p1 _ p2) = acc `go` p1 `go` p2
  go acc (PApp _ _ ps) = foldl go acc ps
  go acc (PTuple _ _ ps) = foldl go acc ps
  go acc (PList _ ps) = foldl go acc ps
  go acc (PParen _ p) = acc `go` p
  go acc (PRec _ _ pfs) = foldl go2 acc pfs where
    go2 acc (PFieldPat _ _ p) = acc `go` p
    go2 acc (PFieldPun _ (UnQual _ name)) = name : acc
    go2 acc (PFieldPun _ (Qual _ _ name)) = name : acc
    go2 acc (PFieldPun _ (Special _ _)) = acc
    go2 acc (PFieldWildcard _) = acc
  go acc (PAsPat _ name p) = (name : acc) `go` p
  go acc (PWildCard _) = acc
  go acc (PIrrPat _ p) = acc `go` p
  go acc (PatTypeSig _ p _) = acc `go` p
  go acc (PViewPat _ _ p) = acc `go` p
  go acc (PRPat _ _) = error "PRPat (regular list pattern) is not supported"
  go acc (PXTag _ _ _ _ _) = error "PXTag (XML element pattern) is not supported"
  go acc (PXETag _ _ _ _) = error "PXETag (XML singleton element pattern) is not supported"
  go acc (PXPcdata _ _) = error "PXPcdata (XML PCDATA pattern) is not supported"
  go acc (PXPatTag _ _) = error "PXPatTag (XML embedded pattern) is not supported"
  go acc (PXRPats _ _) = error "PXRPats (XML regular list pattern) is not supported"
  go acc (PQuasiQuote _ _ _) = error "PQuasiQuote (quasi quote pattern) is not supported"
  go acc (PBangPat _ p) = acc `go` p

dePatBindActivation :: Activation l -> Activation l
dePatBindActivation (ActiveFrom l int) = ActiveFrom (id l) (id int)
dePatBindActivation (ActiveUntil l int) = ActiveUntil (id l) (id int)
dePatBindAlt :: Alt l -> Alt l
dePatBindAlt (Alt l pat rhs binds) = Alt (id l) (dePatBindPat pat) (dePatBindRhs rhs) (fmap (dePatBindBinds) binds)
dePatBindAnnotation :: Annotation l -> Annotation l
dePatBindAnnotation (Ann l name exp) = Ann (id l) (dePatBindName name) (dePatBindExp exp)
dePatBindAnnotation (TypeAnn l name exp) = TypeAnn (id l) (dePatBindName name) (dePatBindExp exp)
dePatBindAnnotation (ModuleAnn l exp) = ModuleAnn (id l) (dePatBindExp exp)
dePatBindAssoc :: Assoc l -> Assoc l
dePatBindAssoc (AssocNone l) = AssocNone (id l)
dePatBindAssoc (AssocLeft l) = AssocLeft (id l)
dePatBindAssoc (AssocRight l) = AssocRight (id l)
dePatBindAsst :: Asst l -> Asst l
dePatBindAsst (ClassA l qName type0) = ClassA (id l) (dePatBindQName qName) (fmap (dePatBindType) type0)
dePatBindAsst (AppA l name type0) = AppA (id l) (dePatBindName name) (fmap (dePatBindType) type0)
dePatBindAsst (InfixA l type1 qName type2) = InfixA (id l) (dePatBindType type1) (dePatBindQName qName) (dePatBindType type2)
dePatBindAsst (IParam l iPName type0) = IParam (id l) (dePatBindIPName iPName) (dePatBindType type0)
dePatBindAsst (EqualP l type1 type2) = EqualP (id l) (dePatBindType type1) (dePatBindType type2)
dePatBindAsst (ParenA l asst) = ParenA (id l) (dePatBindAsst asst)
dePatBindAsst (WildCardA l name) = WildCardA (id l) (fmap (dePatBindName) name)
dePatBindBangType :: BangType l -> BangType l
dePatBindBangType (BangedTy l) = BangedTy (id l)
dePatBindBangType (UnpackedTy l) = UnpackedTy (id l)
dePatBindBinds :: Binds l -> Binds l
dePatBindBinds (BDecls l decl) = BDecls (id l) (fmap (dePatBindDecl) decl)
dePatBindBinds (IPBinds l iPBind) = IPBinds (id l) (fmap (dePatBindIPBind) iPBind)
dePatBindBooleanFormula :: BooleanFormula l -> BooleanFormula l
dePatBindBooleanFormula (VarFormula l name) = VarFormula (id l) (dePatBindName name)
dePatBindBooleanFormula (AndFormula l booleanFormula) = AndFormula (id l) (fmap (dePatBindBooleanFormula) booleanFormula)
dePatBindBooleanFormula (OrFormula l booleanFormula) = OrFormula (id l) (fmap (dePatBindBooleanFormula) booleanFormula)
dePatBindBooleanFormula (ParenFormula l booleanFormula) = ParenFormula (id l) (dePatBindBooleanFormula booleanFormula)
dePatBindBoxed :: Boxed -> Boxed
dePatBindBoxed (Boxed) = Boxed
dePatBindBoxed (Unboxed) = Unboxed
dePatBindBracket :: Bracket l -> Bracket l
dePatBindBracket (ExpBracket l exp) = ExpBracket (id l) (dePatBindExp exp)
dePatBindBracket (PatBracket l pat) = PatBracket (id l) (dePatBindPat pat)
dePatBindBracket (TypeBracket l type0) = TypeBracket (id l) (dePatBindType type0)
dePatBindBracket (DeclBracket l decl) = DeclBracket (id l) (fmap (dePatBindDecl) decl)
dePatBindCName :: CName l -> CName l
dePatBindCName (VarName l name) = VarName (id l) (dePatBindName name)
dePatBindCName (ConName l name) = ConName (id l) (dePatBindName name)
dePatBindCallConv :: CallConv l -> CallConv l
dePatBindCallConv (StdCall l) = StdCall (id l)
dePatBindCallConv (CCall l) = CCall (id l)
dePatBindCallConv (CPlusPlus l) = CPlusPlus (id l)
dePatBindCallConv (DotNet l) = DotNet (id l)
dePatBindCallConv (Jvm l) = Jvm (id l)
dePatBindCallConv (Js l) = Js (id l)
dePatBindCallConv (JavaScript l) = JavaScript (id l)
dePatBindCallConv (CApi l) = CApi (id l)
dePatBindClassDecl :: ClassDecl l -> ClassDecl l
dePatBindClassDecl (ClsDecl l decl) = ClsDecl (id l) (dePatBindDecl decl)
dePatBindClassDecl (ClsDataFam l context declHead kind) = ClsDataFam (id l) (fmap (dePatBindContext) context) (dePatBindDeclHead declHead) (fmap (dePatBindKind) kind)
dePatBindClassDecl (ClsTyFam l declHead kind) = ClsTyFam (id l) (dePatBindDeclHead declHead) (fmap (dePatBindKind) kind)
dePatBindClassDecl (ClsTyDef l type1 type2) = ClsTyDef (id l) (dePatBindType type1) (dePatBindType type2)
dePatBindClassDecl (ClsDefSig l name type0) = ClsDefSig (id l) (dePatBindName name) (dePatBindType type0)
dePatBindConDecl :: ConDecl l -> ConDecl l
dePatBindConDecl (ConDecl l name type0) = ConDecl (id l) (dePatBindName name) (fmap (dePatBindType) type0)
dePatBindConDecl (InfixConDecl l type1 name type2) = InfixConDecl (id l) (dePatBindType type1) (dePatBindName name) (dePatBindType type2)
dePatBindConDecl (RecDecl l name fieldDecl) = RecDecl (id l) (dePatBindName name) (fmap (dePatBindFieldDecl) fieldDecl)
dePatBindContext :: Context l -> Context l
dePatBindContext (CxSingle l asst) = CxSingle (id l) (dePatBindAsst asst)
dePatBindContext (CxTuple l asst) = CxTuple (id l) (fmap (dePatBindAsst) asst)
dePatBindContext (CxEmpty l) = CxEmpty (id l)
dePatBindDataOrNew :: DataOrNew l -> DataOrNew l
dePatBindDataOrNew (DataType l) = DataType (id l)
dePatBindDataOrNew (NewType l) = NewType (id l)
dePatBindDecl :: Decl l -> Decl l
dePatBindDecl (TypeDecl l declHead type0) = TypeDecl (id l) (dePatBindDeclHead declHead) (dePatBindType type0)
dePatBindDecl (TypeFamDecl l declHead kind) = TypeFamDecl (id l) (dePatBindDeclHead declHead) (fmap (dePatBindKind) kind)
dePatBindDecl (ClosedTypeFamDecl l declHead kind typeEqn) = ClosedTypeFamDecl (id l) (dePatBindDeclHead declHead) (fmap (dePatBindKind) kind) (fmap (dePatBindTypeEqn) typeEqn)
dePatBindDecl (DataDecl l dataOrNew context declHead qualConDecl deriving0) = DataDecl (id l) (dePatBindDataOrNew dataOrNew) (fmap (dePatBindContext) context) (dePatBindDeclHead declHead) (fmap (dePatBindQualConDecl) qualConDecl) (fmap (dePatBindDeriving) deriving0)
dePatBindDecl (GDataDecl l dataOrNew context declHead kind gadtDecl deriving0) = GDataDecl (id l) (dePatBindDataOrNew dataOrNew) (fmap (dePatBindContext) context) (dePatBindDeclHead declHead) (fmap (dePatBindKind) kind) (fmap (dePatBindGadtDecl) gadtDecl) (fmap (dePatBindDeriving) deriving0)
dePatBindDecl (DataFamDecl l context declHead kind) = DataFamDecl (id l) (fmap (dePatBindContext) context) (dePatBindDeclHead declHead) (fmap (dePatBindKind) kind)
dePatBindDecl (TypeInsDecl l type1 type2) = TypeInsDecl (id l) (dePatBindType type1) (dePatBindType type2)
dePatBindDecl (DataInsDecl l dataOrNew type0 qualConDecl deriving0) = DataInsDecl (id l) (dePatBindDataOrNew dataOrNew) (dePatBindType type0) (fmap (dePatBindQualConDecl) qualConDecl) (fmap (dePatBindDeriving) deriving0)
dePatBindDecl (GDataInsDecl l dataOrNew type0 kind gadtDecl deriving0) = GDataInsDecl (id l) (dePatBindDataOrNew dataOrNew) (dePatBindType type0) (fmap (dePatBindKind) kind) (fmap (dePatBindGadtDecl) gadtDecl) (fmap (dePatBindDeriving) deriving0)
dePatBindDecl (ClassDecl l context declHead funDep classDecl) = ClassDecl (id l) (fmap (dePatBindContext) context) (dePatBindDeclHead declHead) (fmap (dePatBindFunDep) funDep) (fmap (fmap (dePatBindClassDecl)) classDecl)
dePatBindDecl (InstDecl l overlap instRule instDecl) = InstDecl (id l) (fmap (dePatBindOverlap) overlap) (dePatBindInstRule instRule) (fmap (fmap (dePatBindInstDecl)) instDecl)
dePatBindDecl (DerivDecl l overlap instRule) = DerivDecl (id l) (fmap (dePatBindOverlap) overlap) (dePatBindInstRule instRule)
dePatBindDecl (InfixDecl l assoc int op) = InfixDecl (id l) (dePatBindAssoc assoc) (fmap (id) int) (fmap (dePatBindOp) op)
dePatBindDecl (DefaultDecl l type0) = DefaultDecl (id l) (fmap (dePatBindType) type0)
dePatBindDecl (SpliceDecl l exp) = SpliceDecl (id l) (dePatBindExp exp)
dePatBindDecl (TypeSig l name type0) = TypeSig (id l) (fmap (dePatBindName) name) (dePatBindType type0)
dePatBindDecl (PatSynSig l name tyVarBind context1 context2 type0) = PatSynSig (id l) (dePatBindName name) (fmap (fmap (dePatBindTyVarBind)) tyVarBind) (fmap (dePatBindContext) context1) (fmap (dePatBindContext) context2) (dePatBindType type0)
dePatBindDecl (FunBind l match) = FunBind (id l) (fmap (dePatBindMatch) match)
dePatBindDecl (PatBind l pat rhs binds) =
  case pat of
    PParen _ p -> dePatBindDecl (PatBind l p rhs binds)
    PApp _ _ _ -> process
    _ -> pass
  where
    process =
      let
        vars = patVars pat
      in
        if null vars then
          pass
        else
    pass = PatBind (id l) (dePatBindPat pat) (dePatBindRhs rhs) (fmap (dePatBindBinds) binds)
dePatBindDecl (PatSyn l pat1 pat2 patternSynDirection) = PatSyn (id l) (dePatBindPat pat1) (dePatBindPat pat2) (dePatBindPatternSynDirection patternSynDirection)
dePatBindDecl (ForImp l callConv safety string name type0) = ForImp (id l) (dePatBindCallConv callConv) (fmap (dePatBindSafety) safety) (fmap (id) string) (dePatBindName name) (dePatBindType type0)
dePatBindDecl (ForExp l callConv string name type0) = ForExp (id l) (dePatBindCallConv callConv) (fmap (id) string) (dePatBindName name) (dePatBindType type0)
dePatBindDecl (RulePragmaDecl l rule) = RulePragmaDecl (id l) (fmap (dePatBindRule) rule)
dePatBindDecl (DeprPragmaDecl l name) = DeprPragmaDecl (id l) (fmap (((fmap (dePatBindName)) *** (id))) name)
dePatBindDecl (WarnPragmaDecl l name) = WarnPragmaDecl (id l) (fmap (((fmap (dePatBindName)) *** (id))) name)
dePatBindDecl (InlineSig l bool activation qName) = InlineSig (id l) (id bool) (fmap (dePatBindActivation) activation) (dePatBindQName qName)
dePatBindDecl (InlineConlikeSig l activation qName) = InlineConlikeSig (id l) (fmap (dePatBindActivation) activation) (dePatBindQName qName)
dePatBindDecl (SpecSig l activation qName type0) = SpecSig (id l) (fmap (dePatBindActivation) activation) (dePatBindQName qName) (fmap (dePatBindType) type0)
dePatBindDecl (SpecInlineSig l bool activation qName type0) = SpecInlineSig (id l) (id bool) (fmap (dePatBindActivation) activation) (dePatBindQName qName) (fmap (dePatBindType) type0)
dePatBindDecl (InstSig l instRule) = InstSig (id l) (dePatBindInstRule instRule)
dePatBindDecl (AnnPragma l annotation) = AnnPragma (id l) (dePatBindAnnotation annotation)
dePatBindDecl (MinimalPragma l booleanFormula) = MinimalPragma (id l) (fmap (dePatBindBooleanFormula) booleanFormula)
dePatBindDecl (RoleAnnotDecl l qName role) = RoleAnnotDecl (id l) (dePatBindQName qName) (fmap (dePatBindRole) role)
dePatBindDeclHead :: DeclHead l -> DeclHead l
dePatBindDeclHead (DHead l name) = DHead (id l) (dePatBindName name)
dePatBindDeclHead (DHInfix l tyVarBind name) = DHInfix (id l) (dePatBindTyVarBind tyVarBind) (dePatBindName name)
dePatBindDeclHead (DHParen l declHead) = DHParen (id l) (dePatBindDeclHead declHead)
dePatBindDeclHead (DHApp l declHead tyVarBind) = DHApp (id l) (dePatBindDeclHead declHead) (dePatBindTyVarBind tyVarBind)
dePatBindDeriving :: Deriving l -> Deriving l
dePatBindDeriving (Deriving l instRule) = Deriving (id l) (fmap (dePatBindInstRule) instRule)
dePatBindExp :: Exp l -> Exp l
dePatBindExp (Var l qName) = Var (id l) (dePatBindQName qName)
dePatBindExp (IPVar l iPName) = IPVar (id l) (dePatBindIPName iPName)
dePatBindExp (Con l qName) = Con (id l) (dePatBindQName qName)
dePatBindExp (Lit l literal) = Lit (id l) (dePatBindLiteral literal)
dePatBindExp (InfixApp l exp1 qOp exp2) = InfixApp (id l) (dePatBindExp exp1) (dePatBindQOp qOp) (dePatBindExp exp2)
dePatBindExp (App l exp1 exp2) = App (id l) (dePatBindExp exp1) (dePatBindExp exp2)
dePatBindExp (NegApp l exp) = NegApp (id l) (dePatBindExp exp)
dePatBindExp (Lambda l pat exp) = Lambda (id l) (fmap (dePatBindPat) pat) (dePatBindExp exp)
dePatBindExp (Let l binds exp) = Let (id l) (dePatBindBinds binds) (dePatBindExp exp)
dePatBindExp (If l exp1 exp2 exp3) = If (id l) (dePatBindExp exp1) (dePatBindExp exp2) (dePatBindExp exp3)
dePatBindExp (MultiIf l guardedRhs) = MultiIf (id l) (fmap (dePatBindGuardedRhs) guardedRhs)
dePatBindExp (Case l exp alt) = Case (id l) (dePatBindExp exp) (fmap (dePatBindAlt) alt)
dePatBindExp (Do l stmt) = Do (id l) (fmap (dePatBindStmt) stmt)
dePatBindExp (MDo l stmt) = MDo (id l) (fmap (dePatBindStmt) stmt)
dePatBindExp (Tuple l boxed exp) = Tuple (id l) (dePatBindBoxed boxed) (fmap (dePatBindExp) exp)
dePatBindExp (TupleSection l boxed exp) = TupleSection (id l) (dePatBindBoxed boxed) (fmap (fmap (dePatBindExp)) exp)
dePatBindExp (List l exp) = List (id l) (fmap (dePatBindExp) exp)
dePatBindExp (ParArray l exp) = ParArray (id l) (fmap (dePatBindExp) exp)
dePatBindExp (Paren l exp) = Paren (id l) (dePatBindExp exp)
dePatBindExp (LeftSection l exp qOp) = LeftSection (id l) (dePatBindExp exp) (dePatBindQOp qOp)
dePatBindExp (RightSection l qOp exp) = RightSection (id l) (dePatBindQOp qOp) (dePatBindExp exp)
dePatBindExp (RecConstr l qName fieldUpdate) = RecConstr (id l) (dePatBindQName qName) (fmap (dePatBindFieldUpdate) fieldUpdate)
dePatBindExp (RecUpdate l exp fieldUpdate) = RecUpdate (id l) (dePatBindExp exp) (fmap (dePatBindFieldUpdate) fieldUpdate)
dePatBindExp (EnumFrom l exp) = EnumFrom (id l) (dePatBindExp exp)
dePatBindExp (EnumFromTo l exp1 exp2) = EnumFromTo (id l) (dePatBindExp exp1) (dePatBindExp exp2)
dePatBindExp (EnumFromThen l exp1 exp2) = EnumFromThen (id l) (dePatBindExp exp1) (dePatBindExp exp2)
dePatBindExp (EnumFromThenTo l exp1 exp2 exp3) = EnumFromThenTo (id l) (dePatBindExp exp1) (dePatBindExp exp2) (dePatBindExp exp3)
dePatBindExp (ParArrayFromTo l exp1 exp2) = ParArrayFromTo (id l) (dePatBindExp exp1) (dePatBindExp exp2)
dePatBindExp (ParArrayFromThenTo l exp1 exp2 exp3) = ParArrayFromThenTo (id l) (dePatBindExp exp1) (dePatBindExp exp2) (dePatBindExp exp3)
dePatBindExp (ListComp l exp qualStmt) = ListComp (id l) (dePatBindExp exp) (fmap (dePatBindQualStmt) qualStmt)
dePatBindExp (ParComp l exp qualStmt) = ParComp (id l) (dePatBindExp exp) (fmap (fmap (dePatBindQualStmt)) qualStmt)
dePatBindExp (ParArrayComp l exp qualStmt) = ParArrayComp (id l) (dePatBindExp exp) (fmap (fmap (dePatBindQualStmt)) qualStmt)
dePatBindExp (ExpTypeSig l exp type0) = ExpTypeSig (id l) (dePatBindExp exp) (dePatBindType type0)
dePatBindExp (VarQuote l qName) = VarQuote (id l) (dePatBindQName qName)
dePatBindExp (TypQuote l qName) = TypQuote (id l) (dePatBindQName qName)
dePatBindExp (BracketExp l bracket) = BracketExp (id l) (dePatBindBracket bracket)
dePatBindExp (SpliceExp l splice) = SpliceExp (id l) (dePatBindSplice splice)
dePatBindExp (QuasiQuote l string1 string2) = QuasiQuote (id l) (id string1) (id string2)
dePatBindExp (XTag l xName xAttr exp1 exp2) = XTag (id l) (dePatBindXName xName) (fmap (dePatBindXAttr) xAttr) (fmap (dePatBindExp) exp1) (fmap (dePatBindExp) exp2)
dePatBindExp (XETag l xName xAttr exp) = XETag (id l) (dePatBindXName xName) (fmap (dePatBindXAttr) xAttr) (fmap (dePatBindExp) exp)
dePatBindExp (XPcdata l string) = XPcdata (id l) (id string)
dePatBindExp (XExpTag l exp) = XExpTag (id l) (dePatBindExp exp)
dePatBindExp (XChildTag l exp) = XChildTag (id l) (fmap (dePatBindExp) exp)
dePatBindExp (CorePragma l string exp) = CorePragma (id l) (id string) (dePatBindExp exp)
dePatBindExp (SCCPragma l string exp) = SCCPragma (id l) (id string) (dePatBindExp exp)
dePatBindExp (GenPragma l string int1 int2 exp) = GenPragma (id l) (id string) (((id) *** (id)) int1) (((id) *** (id)) int2) (dePatBindExp exp)
dePatBindExp (Proc l pat exp) = Proc (id l) (dePatBindPat pat) (dePatBindExp exp)
dePatBindExp (LeftArrApp l exp1 exp2) = LeftArrApp (id l) (dePatBindExp exp1) (dePatBindExp exp2)
dePatBindExp (RightArrApp l exp1 exp2) = RightArrApp (id l) (dePatBindExp exp1) (dePatBindExp exp2)
dePatBindExp (LeftArrHighApp l exp1 exp2) = LeftArrHighApp (id l) (dePatBindExp exp1) (dePatBindExp exp2)
dePatBindExp (RightArrHighApp l exp1 exp2) = RightArrHighApp (id l) (dePatBindExp exp1) (dePatBindExp exp2)
dePatBindExp (LCase l alt) = LCase (id l) (fmap (dePatBindAlt) alt)
dePatBindExp (ExprHole l) = ExprHole (id l)
dePatBindExportSpec :: ExportSpec l -> ExportSpec l
dePatBindExportSpec (EVar l qName) = EVar (id l) (dePatBindQName qName)
dePatBindExportSpec (EAbs l namespace qName) = EAbs (id l) (dePatBindNamespace namespace) (dePatBindQName qName)
dePatBindExportSpec (EThingAll l qName) = EThingAll (id l) (dePatBindQName qName)
dePatBindExportSpec (EThingWith l qName cName) = EThingWith (id l) (dePatBindQName qName) (fmap (dePatBindCName) cName)
dePatBindExportSpec (EModuleContents l moduleName) = EModuleContents (id l) (dePatBindModuleName moduleName)
dePatBindExportSpecList :: ExportSpecList l -> ExportSpecList l
dePatBindExportSpecList (ExportSpecList l exportSpec) = ExportSpecList (id l) (fmap (dePatBindExportSpec) exportSpec)
dePatBindFieldDecl :: FieldDecl l -> FieldDecl l
dePatBindFieldDecl (FieldDecl l name type0) = FieldDecl (id l) (fmap (dePatBindName) name) (dePatBindType type0)
dePatBindFieldUpdate :: FieldUpdate l -> FieldUpdate l
dePatBindFieldUpdate (FieldUpdate l qName exp) = FieldUpdate (id l) (dePatBindQName qName) (dePatBindExp exp)
dePatBindFieldUpdate (FieldPun l qName) = FieldPun (id l) (dePatBindQName qName)
dePatBindFieldUpdate (FieldWildcard l) = FieldWildcard (id l)
dePatBindFunDep :: FunDep l -> FunDep l
dePatBindFunDep (FunDep l name1 name2) = FunDep (id l) (fmap (dePatBindName) name1) (fmap (dePatBindName) name2)
dePatBindGadtDecl :: GadtDecl l -> GadtDecl l
dePatBindGadtDecl (GadtDecl l name fieldDecl type0) = GadtDecl (id l) (dePatBindName name) (fmap (fmap (dePatBindFieldDecl)) fieldDecl) (dePatBindType type0)
dePatBindGuardedRhs :: GuardedRhs l -> GuardedRhs l
dePatBindGuardedRhs (GuardedRhs l stmt exp) = GuardedRhs (id l) (fmap (dePatBindStmt) stmt) (dePatBindExp exp)
dePatBindIPBind :: IPBind l -> IPBind l
dePatBindIPBind (IPBind l iPName exp) = IPBind (id l) (dePatBindIPName iPName) (dePatBindExp exp)
dePatBindIPName :: IPName l -> IPName l
dePatBindIPName (IPDup l string) = IPDup (id l) (id string)
dePatBindIPName (IPLin l string) = IPLin (id l) (id string)
dePatBindImportDecl :: ImportDecl l -> ImportDecl l
dePatBindImportDecl (ImportDecl importAnn importModule importQualified importSrc importSafe importPkg importAs importSpecs) = ImportDecl (id importAnn) (dePatBindModuleName importModule) (id importQualified) (id importSrc) (id importSafe) (fmap (id) importPkg) (fmap (dePatBindModuleName) importAs) (fmap (dePatBindImportSpecList) importSpecs)
dePatBindImportSpec :: ImportSpec l -> ImportSpec l
dePatBindImportSpec (IVar l name) = IVar (id l) (dePatBindName name)
dePatBindImportSpec (IAbs l namespace name) = IAbs (id l) (dePatBindNamespace namespace) (dePatBindName name)
dePatBindImportSpec (IThingAll l name) = IThingAll (id l) (dePatBindName name)
dePatBindImportSpec (IThingWith l name cName) = IThingWith (id l) (dePatBindName name) (fmap (dePatBindCName) cName)
dePatBindImportSpecList :: ImportSpecList l -> ImportSpecList l
dePatBindImportSpecList (ImportSpecList l bool importSpec) = ImportSpecList (id l) (id bool) (fmap (dePatBindImportSpec) importSpec)
dePatBindInstDecl :: InstDecl l -> InstDecl l
dePatBindInstDecl (InsDecl l decl) = InsDecl (id l) (dePatBindDecl decl)
dePatBindInstDecl (InsType l type1 type2) = InsType (id l) (dePatBindType type1) (dePatBindType type2)
dePatBindInstDecl (InsData l dataOrNew type0 qualConDecl deriving0) = InsData (id l) (dePatBindDataOrNew dataOrNew) (dePatBindType type0) (fmap (dePatBindQualConDecl) qualConDecl) (fmap (dePatBindDeriving) deriving0)
dePatBindInstDecl (InsGData l dataOrNew type0 kind gadtDecl deriving0) = InsGData (id l) (dePatBindDataOrNew dataOrNew) (dePatBindType type0) (fmap (dePatBindKind) kind) (fmap (dePatBindGadtDecl) gadtDecl) (fmap (dePatBindDeriving) deriving0)
dePatBindInstHead :: InstHead l -> InstHead l
dePatBindInstHead (IHCon l qName) = IHCon (id l) (dePatBindQName qName)
dePatBindInstHead (IHInfix l type0 qName) = IHInfix (id l) (dePatBindType type0) (dePatBindQName qName)
dePatBindInstHead (IHParen l instHead) = IHParen (id l) (dePatBindInstHead instHead)
dePatBindInstHead (IHApp l instHead type0) = IHApp (id l) (dePatBindInstHead instHead) (dePatBindType type0)
dePatBindInstRule :: InstRule l -> InstRule l
dePatBindInstRule (IRule l tyVarBind context instHead) = IRule (id l) (fmap (fmap (dePatBindTyVarBind)) tyVarBind) (fmap (dePatBindContext) context) (dePatBindInstHead instHead)
dePatBindInstRule (IParen l instRule) = IParen (id l) (dePatBindInstRule instRule)
dePatBindKind :: Kind l -> Kind l
dePatBindKind (KindStar l) = KindStar (id l)
dePatBindKind (KindFn l kind1 kind2) = KindFn (id l) (dePatBindKind kind1) (dePatBindKind kind2)
dePatBindKind (KindParen l kind) = KindParen (id l) (dePatBindKind kind)
dePatBindKind (KindVar l qName) = KindVar (id l) (dePatBindQName qName)
dePatBindKind (KindApp l kind1 kind2) = KindApp (id l) (dePatBindKind kind1) (dePatBindKind kind2)
dePatBindKind (KindTuple l kind) = KindTuple (id l) (fmap (dePatBindKind) kind)
dePatBindKind (KindList l kind) = KindList (id l) (dePatBindKind kind)
dePatBindLiteral :: Literal l -> Literal l
dePatBindLiteral (Char l char string) = Char (id l) (id char) (id string)
dePatBindLiteral (String l string1 string2) = String (id l) (id string1) (id string2)
dePatBindLiteral (Int l integer string) = Int (id l) (id integer) (id string)
dePatBindLiteral (Frac l rational string) = Frac (id l) (id rational) (id string)
dePatBindLiteral (PrimInt l integer string) = PrimInt (id l) (id integer) (id string)
dePatBindLiteral (PrimWord l integer string) = PrimWord (id l) (id integer) (id string)
dePatBindLiteral (PrimFloat l rational string) = PrimFloat (id l) (id rational) (id string)
dePatBindLiteral (PrimDouble l rational string) = PrimDouble (id l) (id rational) (id string)
dePatBindLiteral (PrimChar l char string) = PrimChar (id l) (id char) (id string)
dePatBindLiteral (PrimString l string1 string2) = PrimString (id l) (id string1) (id string2)
dePatBindMatch :: Match l -> Match l
dePatBindMatch (Match l name pat rhs binds) = Match (id l) (dePatBindName name) (fmap (dePatBindPat) pat) (dePatBindRhs rhs) (fmap (dePatBindBinds) binds)
dePatBindMatch (InfixMatch l pat1 name pat2 rhs binds) = InfixMatch (id l) (dePatBindPat pat1) (dePatBindName name) (fmap (dePatBindPat) pat2) (dePatBindRhs rhs) (fmap (dePatBindBinds) binds)
dePatBindModule :: Module l -> Module l
dePatBindModule (Module l moduleHead modulePragma importDecl decl) = Module (id l) (fmap (dePatBindModuleHead) moduleHead) (fmap (dePatBindModulePragma) modulePragma) (fmap (dePatBindImportDecl) importDecl) (fmap (dePatBindDecl) decl)
dePatBindModule (XmlPage l moduleName modulePragma xName xAttr exp1 exp2) = XmlPage (id l) (dePatBindModuleName moduleName) (fmap (dePatBindModulePragma) modulePragma) (dePatBindXName xName) (fmap (dePatBindXAttr) xAttr) (fmap (dePatBindExp) exp1) (fmap (dePatBindExp) exp2)
dePatBindModule (XmlHybrid l moduleHead modulePragma importDecl decl xName xAttr exp1 exp2) = XmlHybrid (id l) (fmap (dePatBindModuleHead) moduleHead) (fmap (dePatBindModulePragma) modulePragma) (fmap (dePatBindImportDecl) importDecl) (fmap (dePatBindDecl) decl) (dePatBindXName xName) (fmap (dePatBindXAttr) xAttr) (fmap (dePatBindExp) exp1) (fmap (dePatBindExp) exp2)
dePatBindModuleHead :: ModuleHead l -> ModuleHead l
dePatBindModuleHead (ModuleHead l moduleName warningText exportSpecList) = ModuleHead (id l) (dePatBindModuleName moduleName) (fmap (dePatBindWarningText) warningText) (fmap (dePatBindExportSpecList) exportSpecList)
dePatBindModuleName :: ModuleName l -> ModuleName l
dePatBindModuleName (ModuleName l string) = ModuleName (id l) (id string)
dePatBindModulePragma :: ModulePragma l -> ModulePragma l
dePatBindModulePragma (LanguagePragma l name) = LanguagePragma (id l) (fmap (dePatBindName) name)
dePatBindModulePragma (OptionsPragma l tool string) = OptionsPragma (id l) (fmap (dePatBindTool) tool) (id string)
dePatBindModulePragma (AnnModulePragma l annotation) = AnnModulePragma (id l) (dePatBindAnnotation annotation)
dePatBindName :: Name l -> Name l
dePatBindName (Ident l string) = Ident (id l) (id string)
dePatBindName (Symbol l string) = Symbol (id l) (id string)
dePatBindNamespace :: Namespace l -> Namespace l
dePatBindNamespace (NoNamespace l) = NoNamespace (id l)
dePatBindNamespace (TypeNamespace l) = TypeNamespace (id l)
dePatBindNamespace (PatternNamespace l) = PatternNamespace (id l)
dePatBindOp :: Op l -> Op l
dePatBindOp (VarOp l name) = VarOp (id l) (dePatBindName name)
dePatBindOp (ConOp l name) = ConOp (id l) (dePatBindName name)
dePatBindOverlap :: Overlap l -> Overlap l
dePatBindOverlap (NoOverlap l) = NoOverlap (id l)
dePatBindOverlap (Overlap l) = Overlap (id l)
dePatBindOverlap (Incoherent l) = Incoherent (id l)
dePatBindPXAttr :: PXAttr l -> PXAttr l
dePatBindPXAttr (PXAttr l xName pat) = PXAttr (id l) (dePatBindXName xName) (dePatBindPat pat)
dePatBindPat :: Pat l -> Pat l
dePatBindPat (PVar l name) = PVar (id l) (dePatBindName name)
dePatBindPat (PLit l sign literal) = PLit (id l) (dePatBindSign sign) (dePatBindLiteral literal)
dePatBindPat (PNPlusK l name integer) = PNPlusK (id l) (dePatBindName name) (id integer)
dePatBindPat (PInfixApp l pat1 qName pat2) = PInfixApp (id l) (dePatBindPat pat1) (dePatBindQName qName) (dePatBindPat pat2)
dePatBindPat (PApp l qName pat) = PApp (id l) (dePatBindQName qName) (fmap (dePatBindPat) pat)
dePatBindPat (PTuple l boxed pat) = PTuple (id l) (dePatBindBoxed boxed) (fmap (dePatBindPat) pat)
dePatBindPat (PList l pat) = PList (id l) (fmap (dePatBindPat) pat)
dePatBindPat (PParen l pat) = PParen (id l) (dePatBindPat pat)
dePatBindPat (PRec l qName patField) = PRec (id l) (dePatBindQName qName) (fmap (dePatBindPatField) patField)
dePatBindPat (PAsPat l name pat) = PAsPat (id l) (dePatBindName name) (dePatBindPat pat)
dePatBindPat (PWildCard l) = PWildCard (id l)
dePatBindPat (PIrrPat l pat) = PIrrPat (id l) (dePatBindPat pat)
dePatBindPat (PatTypeSig l pat type0) = PatTypeSig (id l) (dePatBindPat pat) (dePatBindType type0)
dePatBindPat (PViewPat l exp pat) = PViewPat (id l) (dePatBindExp exp) (dePatBindPat pat)
dePatBindPat (PRPat l rPat) = PRPat (id l) (fmap (dePatBindRPat) rPat)
dePatBindPat (PXTag l xName pXAttr pat1 pat2) = PXTag (id l) (dePatBindXName xName) (fmap (dePatBindPXAttr) pXAttr) (fmap (dePatBindPat) pat1) (fmap (dePatBindPat) pat2)
dePatBindPat (PXETag l xName pXAttr pat) = PXETag (id l) (dePatBindXName xName) (fmap (dePatBindPXAttr) pXAttr) (fmap (dePatBindPat) pat)
dePatBindPat (PXPcdata l string) = PXPcdata (id l) (id string)
dePatBindPat (PXPatTag l pat) = PXPatTag (id l) (dePatBindPat pat)
dePatBindPat (PXRPats l rPat) = PXRPats (id l) (fmap (dePatBindRPat) rPat)
dePatBindPat (PQuasiQuote l string1 string2) = PQuasiQuote (id l) (id string1) (id string2)
dePatBindPat (PBangPat l pat) = PBangPat (id l) (dePatBindPat pat)
dePatBindPatField :: PatField l -> PatField l
dePatBindPatField (PFieldPat l qName pat) = PFieldPat (id l) (dePatBindQName qName) (dePatBindPat pat)
dePatBindPatField (PFieldPun l qName) = PFieldPun (id l) (dePatBindQName qName)
dePatBindPatField (PFieldWildcard l) = PFieldWildcard (id l)
dePatBindPatternSynDirection :: PatternSynDirection l -> PatternSynDirection l
dePatBindPatternSynDirection (Unidirectional) = Unidirectional
dePatBindPatternSynDirection (ImplicitBidirectional) = ImplicitBidirectional
dePatBindPatternSynDirection (ExplicitBidirectional l decl) = ExplicitBidirectional (id l) (fmap (dePatBindDecl) decl)
dePatBindPromoted :: Promoted l -> Promoted l
dePatBindPromoted (PromotedInteger l integer string) = PromotedInteger (id l) (id integer) (id string)
dePatBindPromoted (PromotedString l string1 string2) = PromotedString (id l) (id string1) (id string2)
dePatBindPromoted (PromotedCon l bool qName) = PromotedCon (id l) (id bool) (dePatBindQName qName)
dePatBindPromoted (PromotedList l bool type0) = PromotedList (id l) (id bool) (fmap (dePatBindType) type0)
dePatBindPromoted (PromotedTuple l type0) = PromotedTuple (id l) (fmap (dePatBindType) type0)
dePatBindPromoted (PromotedUnit l) = PromotedUnit (id l)
dePatBindQName :: QName l -> QName l
dePatBindQName (Qual l moduleName name) = Qual (id l) (dePatBindModuleName moduleName) (dePatBindName name)
dePatBindQName (UnQual l name) = UnQual (id l) (dePatBindName name)
dePatBindQName (Special l specialCon) = Special (id l) (dePatBindSpecialCon specialCon)
dePatBindQOp :: QOp l -> QOp l
dePatBindQOp (QVarOp l qName) = QVarOp (id l) (dePatBindQName qName)
dePatBindQOp (QConOp l qName) = QConOp (id l) (dePatBindQName qName)
dePatBindQualConDecl :: QualConDecl l -> QualConDecl l
dePatBindQualConDecl (QualConDecl l tyVarBind context conDecl) = QualConDecl (id l) (fmap (fmap (dePatBindTyVarBind)) tyVarBind) (fmap (dePatBindContext) context) (dePatBindConDecl conDecl)
dePatBindQualStmt :: QualStmt l -> QualStmt l
dePatBindQualStmt (QualStmt l stmt) = QualStmt (id l) (dePatBindStmt stmt)
dePatBindQualStmt (ThenTrans l exp) = ThenTrans (id l) (dePatBindExp exp)
dePatBindQualStmt (ThenBy l exp1 exp2) = ThenBy (id l) (dePatBindExp exp1) (dePatBindExp exp2)
dePatBindQualStmt (GroupBy l exp) = GroupBy (id l) (dePatBindExp exp)
dePatBindQualStmt (GroupUsing l exp) = GroupUsing (id l) (dePatBindExp exp)
dePatBindQualStmt (GroupByUsing l exp1 exp2) = GroupByUsing (id l) (dePatBindExp exp1) (dePatBindExp exp2)
dePatBindRPat :: RPat l -> RPat l
dePatBindRPat (RPOp l rPat rPatOp) = RPOp (id l) (dePatBindRPat rPat) (dePatBindRPatOp rPatOp)
dePatBindRPat (RPEither l rPat1 rPat2) = RPEither (id l) (dePatBindRPat rPat1) (dePatBindRPat rPat2)
dePatBindRPat (RPSeq l rPat) = RPSeq (id l) (fmap (dePatBindRPat) rPat)
dePatBindRPat (RPGuard l pat stmt) = RPGuard (id l) (dePatBindPat pat) (fmap (dePatBindStmt) stmt)
dePatBindRPat (RPCAs l name rPat) = RPCAs (id l) (dePatBindName name) (dePatBindRPat rPat)
dePatBindRPat (RPAs l name rPat) = RPAs (id l) (dePatBindName name) (dePatBindRPat rPat)
dePatBindRPat (RPParen l rPat) = RPParen (id l) (dePatBindRPat rPat)
dePatBindRPat (RPPat l pat) = RPPat (id l) (dePatBindPat pat)
dePatBindRPatOp :: RPatOp l -> RPatOp l
dePatBindRPatOp (RPStar l) = RPStar (id l)
dePatBindRPatOp (RPStarG l) = RPStarG (id l)
dePatBindRPatOp (RPPlus l) = RPPlus (id l)
dePatBindRPatOp (RPPlusG l) = RPPlusG (id l)
dePatBindRPatOp (RPOpt l) = RPOpt (id l)
dePatBindRPatOp (RPOptG l) = RPOptG (id l)
dePatBindRhs :: Rhs l -> Rhs l
dePatBindRhs (UnGuardedRhs l exp) = UnGuardedRhs (id l) (dePatBindExp exp)
dePatBindRhs (GuardedRhss l guardedRhs) = GuardedRhss (id l) (fmap (dePatBindGuardedRhs) guardedRhs)
dePatBindRole :: Role l -> Role l
dePatBindRole (Nominal l) = Nominal (id l)
dePatBindRole (Representational l) = Representational (id l)
dePatBindRole (Phantom l) = Phantom (id l)
dePatBindRole (RoleWildcard l) = RoleWildcard (id l)
dePatBindRule :: Rule l -> Rule l
dePatBindRule (Rule l string activation ruleVar exp1 exp2) = Rule (id l) (id string) (fmap (dePatBindActivation) activation) (fmap (fmap (dePatBindRuleVar)) ruleVar) (dePatBindExp exp1) (dePatBindExp exp2)
dePatBindRuleVar :: RuleVar l -> RuleVar l
dePatBindRuleVar (RuleVar l name) = RuleVar (id l) (dePatBindName name)
dePatBindRuleVar (TypedRuleVar l name type0) = TypedRuleVar (id l) (dePatBindName name) (dePatBindType type0)
dePatBindSafety :: Safety l -> Safety l
dePatBindSafety (PlayRisky l) = PlayRisky (id l)
dePatBindSafety (PlaySafe l bool) = PlaySafe (id l) (id bool)
dePatBindSafety (PlayInterruptible l) = PlayInterruptible (id l)
dePatBindSign :: Sign l -> Sign l
dePatBindSign (Signless l) = Signless (id l)
dePatBindSign (Negative l) = Negative (id l)
dePatBindSpecialCon :: SpecialCon l -> SpecialCon l
dePatBindSpecialCon (UnitCon l) = UnitCon (id l)
dePatBindSpecialCon (ListCon l) = ListCon (id l)
dePatBindSpecialCon (FunCon l) = FunCon (id l)
dePatBindSpecialCon (TupleCon l boxed int) = TupleCon (id l) (dePatBindBoxed boxed) (id int)
dePatBindSpecialCon (Cons l) = Cons (id l)
dePatBindSpecialCon (UnboxedSingleCon l) = UnboxedSingleCon (id l)
dePatBindSplice :: Splice l -> Splice l
dePatBindSplice (IdSplice l string) = IdSplice (id l) (id string)
dePatBindSplice (ParenSplice l exp) = ParenSplice (id l) (dePatBindExp exp)
dePatBindStmt :: Stmt l -> Stmt l
dePatBindStmt (Generator l pat exp) = Generator (id l) (dePatBindPat pat) (dePatBindExp exp)
dePatBindStmt (Qualifier l exp) = Qualifier (id l) (dePatBindExp exp)
dePatBindStmt (LetStmt l binds) = LetStmt (id l) (dePatBindBinds binds)
dePatBindStmt (RecStmt l stmt) = RecStmt (id l) (fmap (dePatBindStmt) stmt)
dePatBindTool :: Tool -> Tool
dePatBindTool (GHC) = GHC
dePatBindTool (HUGS) = HUGS
dePatBindTool (NHC98) = NHC98
dePatBindTool (YHC) = YHC
dePatBindTool (HADDOCK) = HADDOCK
dePatBindTool (UnknownTool string) = UnknownTool (id string)
dePatBindTyVarBind :: TyVarBind l -> TyVarBind l
dePatBindTyVarBind (KindedVar l name kind) = KindedVar (id l) (dePatBindName name) (dePatBindKind kind)
dePatBindTyVarBind (UnkindedVar l name) = UnkindedVar (id l) (dePatBindName name)
dePatBindType :: Type l -> Type l
dePatBindType (TyForall l tyVarBind context type0) = TyForall (id l) (fmap (fmap (dePatBindTyVarBind)) tyVarBind) (fmap (dePatBindContext) context) (dePatBindType type0)
dePatBindType (TyFun l type1 type2) = TyFun (id l) (dePatBindType type1) (dePatBindType type2)
dePatBindType (TyTuple l boxed type0) = TyTuple (id l) (dePatBindBoxed boxed) (fmap (dePatBindType) type0)
dePatBindType (TyList l type0) = TyList (id l) (dePatBindType type0)
dePatBindType (TyParArray l type0) = TyParArray (id l) (dePatBindType type0)
dePatBindType (TyApp l type1 type2) = TyApp (id l) (dePatBindType type1) (dePatBindType type2)
dePatBindType (TyVar l name) = TyVar (id l) (dePatBindName name)
dePatBindType (TyCon l qName) = TyCon (id l) (dePatBindQName qName)
dePatBindType (TyParen l type0) = TyParen (id l) (dePatBindType type0)
dePatBindType (TyInfix l type1 qName type2) = TyInfix (id l) (dePatBindType type1) (dePatBindQName qName) (dePatBindType type2)
dePatBindType (TyKind l type0 kind) = TyKind (id l) (dePatBindType type0) (dePatBindKind kind)
dePatBindType (TyPromoted l promoted) = TyPromoted (id l) (dePatBindPromoted promoted)
dePatBindType (TyEquals l type1 type2) = TyEquals (id l) (dePatBindType type1) (dePatBindType type2)
dePatBindType (TySplice l splice) = TySplice (id l) (dePatBindSplice splice)
dePatBindType (TyBang l bangType type0) = TyBang (id l) (dePatBindBangType bangType) (dePatBindType type0)
dePatBindType (TyWildCard l name) = TyWildCard (id l) (fmap (dePatBindName) name)
dePatBindTypeEqn :: TypeEqn l -> TypeEqn l
dePatBindTypeEqn (TypeEqn l type1 type2) = TypeEqn (id l) (dePatBindType type1) (dePatBindType type2)
dePatBindWarningText :: WarningText l -> WarningText l
dePatBindWarningText (DeprText l string) = DeprText (id l) (id string)
dePatBindWarningText (WarnText l string) = WarnText (id l) (id string)
dePatBindXAttr :: XAttr l -> XAttr l
dePatBindXAttr (XAttr l xName exp) = XAttr (id l) (dePatBindXName xName) (dePatBindExp exp)
dePatBindXName :: XName l -> XName l
dePatBindXName (XName l string) = XName (id l) (id string)
dePatBindXName (XDomName l string1 string2) = XDomName (id l) (id string1) (id string2)

