module Desugar
  (
  ) where

import Language.Haskell.Exts.Annotated.Syntax
import Control.Arrow ((***))
import Control.Applicative
import Data.Traversable

import DesugarClass

instance Monad m => Desugar m (Activation l) where
  desugar (ActiveFrom l int) = return $ ActiveFrom (id l) (id int)
  desugar (ActiveUntil l int) = return $ ActiveUntil (id l) (id int)

instance Monad m => Desugar m (Alt l) where
  desugar (Alt l pat rhs binds) = Alt (id l) <$> (desugar pat) <*> (desugar rhs) <*> sequence (fmap (desugar) binds)

instance Monad m => Desugar m (Annotation l) where
  desugar (Ann l name exp) = Ann (id l) <$> (desugar name) <*> (desugar exp)
  desugar (TypeAnn l name exp) = TypeAnn (id l) <$> (desugar name) <*> (desugar exp)
  desugar (ModuleAnn l exp) = ModuleAnn (id l) <$> (desugar exp)

instance Monad m => Desugar m (Assoc l) where
  desugar (AssocNone l) = return $ AssocNone (id l)
  desugar (AssocLeft l) = return $ AssocLeft (id l)
  desugar (AssocRight l) = return $ AssocRight (id l)

instance Monad m => Desugar m (Asst l) where
  desugar (ClassA l qName type0) = ClassA (id l) <$> (desugar qName) <*> sequence (fmap (desugar) type0)
  desugar (AppA l name type0) = AppA (id l) <$> (desugar name) <*> sequence (fmap (desugar) type0)
  desugar (InfixA l type1 qName type2) = InfixA (id l) <$> (desugar type1) <*> (desugar qName) <*> (desugar type2)
  desugar (IParam l iPName type0) = IParam (id l) <$> (desugar iPName) <*> (desugar type0)
  desugar (EqualP l type1 type2) = EqualP (id l) <$> (desugar type1) <*> (desugar type2)
  desugar (ParenA l asst) = ParenA (id l) <$> (desugar asst)
  desugar (WildCardA l name) = WildCardA (id l) <$> sequence (fmap (desugar) name)

instance Monad m => Desugar m (BangType l) where
  desugar (BangedTy l) = return $ BangedTy (id l)
  desugar (UnpackedTy l) = return $ UnpackedTy (id l)

instance Monad m => Desugar m (Binds l) where
  desugar (BDecls l decl) = BDecls (id l) <$> sequence (fmap (desugar) decl)
  desugar (IPBinds l iPBind) = IPBinds (id l) <$> sequence (fmap (desugar) iPBind)

instance Monad m => Desugar m (BooleanFormula l) where
  desugar (VarFormula l name) = VarFormula (id l) <$> (desugar name)
  desugar (AndFormula l booleanFormula) = AndFormula (id l) <$> sequence (fmap (desugar) booleanFormula)
  desugar (OrFormula l booleanFormula) = OrFormula (id l) <$> sequence (fmap (desugar) booleanFormula)
  desugar (ParenFormula l booleanFormula) = ParenFormula (id l) <$> (desugar booleanFormula)

instance Monad m => Desugar m (Boxed) where
  desugar (Boxed) = return $ Boxed
  desugar (Unboxed) = return $ Unboxed

instance Monad m => Desugar m (Bracket l) where
  desugar (ExpBracket l exp) = ExpBracket (id l) <$> (desugar exp)
  desugar (PatBracket l pat) = PatBracket (id l) <$> (desugar pat)
  desugar (TypeBracket l type0) = TypeBracket (id l) <$> (desugar type0)
  desugar (DeclBracket l decl) = DeclBracket (id l) <$> sequence (fmap (desugar) decl)

instance Monad m => Desugar m (CName l) where
  desugar (VarName l name) = VarName (id l) <$> (desugar name)
  desugar (ConName l name) = ConName (id l) <$> (desugar name)

instance Monad m => Desugar m (CallConv l) where
  desugar (StdCall l) = return $ StdCall (id l)
  desugar (CCall l) = return $ CCall (id l)
  desugar (CPlusPlus l) = return $ CPlusPlus (id l)
  desugar (DotNet l) = return $ DotNet (id l)
  desugar (Jvm l) = return $ Jvm (id l)
  desugar (Js l) = return $ Js (id l)
  desugar (JavaScript l) = return $ JavaScript (id l)
  desugar (CApi l) = return $ CApi (id l)

instance Monad m => Desugar m (ClassDecl l) where
  desugar (ClsDecl l decl) = ClsDecl (id l) <$> (desugar decl)
  desugar (ClsDataFam l context declHead kind) = ClsDataFam (id l) <$> sequence (fmap (desugar) context) <*> (desugar declHead) <*> sequence (fmap (desugar) kind)
  desugar (ClsTyFam l declHead kind) = ClsTyFam (id l) <$> (desugar declHead) <*> sequence (fmap (desugar) kind)
  desugar (ClsTyDef l type1 type2) = ClsTyDef (id l) <$> (desugar type1) <*> (desugar type2)
  desugar (ClsDefSig l name type0) = ClsDefSig (id l) <$> (desugar name) <*> (desugar type0)

instance Monad m => Desugar m (ConDecl l) where
  desugar (ConDecl l name type0) = ConDecl (id l) <$> (desugar name) <*> sequence (fmap (desugar) type0)
  desugar (InfixConDecl l type1 name type2) = InfixConDecl (id l) <$> (desugar type1) <*> (desugar name) <*> (desugar type2)
  desugar (RecDecl l name fieldDecl) = RecDecl (id l) <$> (desugar name) <*> sequence (fmap (desugar) fieldDecl)

instance Monad m => Desugar m (Context l) where
  desugar (CxSingle l asst) = CxSingle (id l) <$> (desugar asst)
  desugar (CxTuple l asst) = CxTuple (id l) <$> sequence (fmap (desugar) asst)
  desugar (CxEmpty l) = return $ CxEmpty (id l)

instance Monad m => Desugar m (DataOrNew l) where
  desugar (DataType l) = return $ DataType (id l)
  desugar (NewType l) = return $ NewType (id l)

instance Monad m => Desugar m (Decl l) where
  desugar (TypeDecl l declHead type0) = TypeDecl (id l) <$> (desugar declHead) <*> (desugar type0)
  desugar (TypeFamDecl l declHead kind) = TypeFamDecl (id l) <$> (desugar declHead) <*> sequence (fmap (desugar) kind)
  desugar (ClosedTypeFamDecl l declHead kind typeEqn) = ClosedTypeFamDecl (id l) <$> (desugar declHead) <*> sequence (fmap (desugar) kind) <*> sequence (fmap (desugar) typeEqn)
  desugar (DataDecl l dataOrNew context declHead qualConDecl deriving0) = DataDecl (id l) <$> (desugar dataOrNew) <*> sequence (fmap (desugar) context) <*> (desugar declHead) <*> sequence (fmap (desugar) qualConDecl) <*> sequence (fmap (desugar) deriving0)
  desugar (GDataDecl l dataOrNew context declHead kind gadtDecl deriving0) = GDataDecl (id l) <$> (desugar dataOrNew) <*> sequence (fmap (desugar) context) <*> (desugar declHead) <*> sequence (fmap (desugar) kind) <*> sequence (fmap (desugar) gadtDecl) <*> sequence (fmap (desugar) deriving0)
  desugar (DataFamDecl l context declHead kind) = DataFamDecl (id l) <$> sequence (fmap (desugar) context) <*> (desugar declHead) <*> sequence (fmap (desugar) kind)
  desugar (TypeInsDecl l type1 type2) = TypeInsDecl (id l) <$> (desugar type1) <*> (desugar type2)
  desugar (DataInsDecl l dataOrNew type0 qualConDecl deriving0) = DataInsDecl (id l) <$> (desugar dataOrNew) <*> (desugar type0) <*> sequence (fmap (desugar) qualConDecl) <*> sequence (fmap (desugar) deriving0)
  desugar (GDataInsDecl l dataOrNew type0 kind gadtDecl deriving0) = GDataInsDecl (id l) <$> (desugar dataOrNew) <*> (desugar type0) <*> sequence (fmap (desugar) kind) <*> sequence (fmap (desugar) gadtDecl) <*> sequence (fmap (desugar) deriving0)
  desugar (ClassDecl l context declHead funDep classDecl) = ClassDecl (id l) <$> sequence (fmap (desugar) context) <*> (desugar declHead) <*> sequence (fmap (desugar) funDep) <*> sequence (fmap (sequence . fmap (desugar)) classDecl)
  desugar (InstDecl l overlap instRule instDecl) = InstDecl (id l) <$> sequence (fmap (desugar) overlap) <*> (desugar instRule) <*> sequence (fmap (sequence . fmap (desugar)) instDecl)
  desugar (DerivDecl l overlap instRule) = DerivDecl (id l) <$> sequence (fmap (desugar) overlap) <*> (desugar instRule)
  desugar (InfixDecl l assoc int op) = InfixDecl (id l) <$> (desugar assoc) <*> return (fmap (id) int) <*> sequence (fmap (desugar) op)
  desugar (DefaultDecl l type0) = DefaultDecl (id l) <$> sequence (fmap (desugar) type0)
  desugar (SpliceDecl l exp) = SpliceDecl (id l) <$> (desugar exp)
  desugar (TypeSig l name type0) = TypeSig (id l) <$> sequence (fmap (desugar) name) <*> (desugar type0)
  desugar (PatSynSig l name tyVarBind context1 context2 type0) = PatSynSig (id l) <$> (desugar name) <*> sequence (fmap (sequence . fmap (desugar)) tyVarBind) <*> sequence (fmap (desugar) context1) <*> sequence (fmap (desugar) context2) <*> (desugar type0)
  desugar (FunBind l match) = FunBind (id l) <$> sequence (fmap (desugar) match)
  desugar (PatBind l pat rhs binds) = PatBind (id l) <$> (desugar pat) <*> (desugar rhs) <*> sequence (fmap (desugar) binds)
  desugar (PatSyn l pat1 pat2 patternSynDirection) = PatSyn (id l) <$> (desugar pat1) <*> (desugar pat2) <*> (desugar patternSynDirection)
  desugar (ForImp l callConv safety string name type0) = ForImp (id l) <$> (desugar callConv) <*> sequence (fmap (desugar) safety) <*> return (fmap (id) string) <*> (desugar name) <*> (desugar type0)
  desugar (ForExp l callConv string name type0) = ForExp (id l) <$> (desugar callConv) <*> return (fmap (id) string) <*> (desugar name) <*> (desugar type0)
  desugar (RulePragmaDecl l rule) = RulePragmaDecl (id l) <$> sequence (fmap (desugar) rule)
  desugar (DeprPragmaDecl l name) = DeprPragmaDecl (id l) <$> sequence (fmap (uncurry (flip (fmap . flip (,))) . ((sequence . fmap (desugar)) *** (id))) name)
  desugar (WarnPragmaDecl l name) = WarnPragmaDecl (id l) <$> sequence (fmap (uncurry (flip (fmap . flip (,))) . ((sequence . fmap (desugar)) *** (id))) name)
  desugar (InlineSig l bool activation qName) = InlineSig (id l) (id bool) <$> sequence (fmap (desugar) activation) <*> (desugar qName)
  desugar (InlineConlikeSig l activation qName) = InlineConlikeSig (id l) <$> sequence (fmap (desugar) activation) <*> (desugar qName)
  desugar (SpecSig l activation qName type0) = SpecSig (id l) <$> sequence (fmap (desugar) activation) <*> (desugar qName) <*> sequence (fmap (desugar) type0)
  desugar (SpecInlineSig l bool activation qName type0) = SpecInlineSig (id l) (id bool) <$> sequence (fmap (desugar) activation) <*> (desugar qName) <*> sequence (fmap (desugar) type0)
  desugar (InstSig l instRule) = InstSig (id l) <$> (desugar instRule)
  desugar (AnnPragma l annotation) = AnnPragma (id l) <$> (desugar annotation)
  desugar (MinimalPragma l booleanFormula) = MinimalPragma (id l) <$> sequence (fmap (desugar) booleanFormula)
  desugar (RoleAnnotDecl l qName role) = RoleAnnotDecl (id l) <$> (desugar qName) <*> sequence (fmap (desugar) role)

instance Monad m => Desugar m (DeclHead l) where
  desugar (DHead l name) = DHead (id l) <$> (desugar name)
  desugar (DHInfix l tyVarBind name) = DHInfix (id l) <$> (desugar tyVarBind) <*> (desugar name)
  desugar (DHParen l declHead) = DHParen (id l) <$> (desugar declHead)
  desugar (DHApp l declHead tyVarBind) = DHApp (id l) <$> (desugar declHead) <*> (desugar tyVarBind)

instance Monad m => Desugar m (Deriving l) where
  desugar (Deriving l instRule) = Deriving (id l) <$> sequence (fmap (desugar) instRule)

instance Monad m => Desugar m (Exp l) where
  desugar (Var l qName) = Var (id l) <$> (desugar qName)
  desugar (IPVar l iPName) = IPVar (id l) <$> (desugar iPName)
  desugar (Con l qName) = Con (id l) <$> (desugar qName)
  desugar (Lit l literal) = Lit (id l) <$> (desugar literal)
  desugar (InfixApp l exp1 qOp exp2) = InfixApp (id l) <$> (desugar exp1) <*> (desugar qOp) <*> (desugar exp2)
  desugar (App l exp1 exp2) = App (id l) <$> (desugar exp1) <*> (desugar exp2)
  desugar (NegApp l exp) = NegApp (id l) <$> (desugar exp)
  desugar (Lambda l pat exp) = Lambda (id l) <$> sequence (fmap (desugar) pat) <*> (desugar exp)
  desugar (Let l binds exp) = Let (id l) <$> (desugar binds) <*> (desugar exp)
  desugar (If l exp1 exp2 exp3) = If (id l) <$> (desugar exp1) <*> (desugar exp2) <*> (desugar exp3)
  desugar (MultiIf l guardedRhs) = MultiIf (id l) <$> sequence (fmap (desugar) guardedRhs)
  desugar (Case l exp alt) = Case (id l) <$> (desugar exp) <*> sequence (fmap (desugar) alt)
  desugar (Do l stmt) = Do (id l) <$> sequence (fmap (desugar) stmt)
  desugar (MDo l stmt) = MDo (id l) <$> sequence (fmap (desugar) stmt)
  desugar (Tuple l boxed exp) = Tuple (id l) <$> (desugar boxed) <*> sequence (fmap (desugar) exp)
  desugar (TupleSection l boxed exp) = TupleSection (id l) <$> (desugar boxed) <*> sequence (fmap (sequence . fmap (desugar)) exp)
  desugar (List l exp) = List (id l) <$> sequence (fmap (desugar) exp)
  desugar (ParArray l exp) = ParArray (id l) <$> sequence (fmap (desugar) exp)
  desugar (Paren l exp) = Paren (id l) <$> (desugar exp)
  desugar (LeftSection l exp qOp) = LeftSection (id l) <$> (desugar exp) <*> (desugar qOp)
  desugar (RightSection l qOp exp) = RightSection (id l) <$> (desugar qOp) <*> (desugar exp)
  desugar (RecConstr l qName fieldUpdate) = RecConstr (id l) <$> (desugar qName) <*> sequence (fmap (desugar) fieldUpdate)
  desugar (RecUpdate l exp fieldUpdate) = RecUpdate (id l) <$> (desugar exp) <*> sequence (fmap (desugar) fieldUpdate)
  desugar (EnumFrom l exp) = EnumFrom (id l) <$> (desugar exp)
  desugar (EnumFromTo l exp1 exp2) = EnumFromTo (id l) <$> (desugar exp1) <*> (desugar exp2)
  desugar (EnumFromThen l exp1 exp2) = EnumFromThen (id l) <$> (desugar exp1) <*> (desugar exp2)
  desugar (EnumFromThenTo l exp1 exp2 exp3) = EnumFromThenTo (id l) <$> (desugar exp1) <*> (desugar exp2) <*> (desugar exp3)
  desugar (ParArrayFromTo l exp1 exp2) = ParArrayFromTo (id l) <$> (desugar exp1) <*> (desugar exp2)
  desugar (ParArrayFromThenTo l exp1 exp2 exp3) = ParArrayFromThenTo (id l) <$> (desugar exp1) <*> (desugar exp2) <*> (desugar exp3)
  desugar (ListComp l exp qualStmt) = ListComp (id l) <$> (desugar exp) <*> sequence (fmap (desugar) qualStmt)
  desugar (ParComp l exp qualStmt) = ParComp (id l) <$> (desugar exp) <*> sequence (fmap (sequence . fmap (desugar)) qualStmt)
  desugar (ParArrayComp l exp qualStmt) = ParArrayComp (id l) <$> (desugar exp) <*> sequence (fmap (sequence . fmap (desugar)) qualStmt)
  desugar (ExpTypeSig l exp type0) = ExpTypeSig (id l) <$> (desugar exp) <*> (desugar type0)
  desugar (VarQuote l qName) = VarQuote (id l) <$> (desugar qName)
  desugar (TypQuote l qName) = TypQuote (id l) <$> (desugar qName)
  desugar (BracketExp l bracket) = BracketExp (id l) <$> (desugar bracket)
  desugar (SpliceExp l splice) = SpliceExp (id l) <$> (desugar splice)
  desugar (QuasiQuote l string1 string2) = return $ QuasiQuote (id l) (id string1) (id string2)
  desugar (XTag l xName xAttr exp1 exp2) = XTag (id l) <$> (desugar xName) <*> sequence (fmap (desugar) xAttr) <*> sequence (fmap (desugar) exp1) <*> sequence (fmap (desugar) exp2)
  desugar (XETag l xName xAttr exp) = XETag (id l) <$> (desugar xName) <*> sequence (fmap (desugar) xAttr) <*> sequence (fmap (desugar) exp)
  desugar (XPcdata l string) = return $ XPcdata (id l) (id string)
  desugar (XExpTag l exp) = XExpTag (id l) <$> (desugar exp)
  desugar (XChildTag l exp) = XChildTag (id l) <$> sequence (fmap (desugar) exp)
  desugar (CorePragma l string exp) = CorePragma (id l) (id string) <$> (desugar exp)
  desugar (SCCPragma l string exp) = SCCPragma (id l) (id string) <$> (desugar exp)
  desugar (GenPragma l string int1 int2 exp) = GenPragma (id l) (id string) (((id) *** (id)) int1) (((id) *** (id)) int2) <$> (desugar exp)
  desugar (Proc l pat exp) = Proc (id l) <$> (desugar pat) <*> (desugar exp)
  desugar (LeftArrApp l exp1 exp2) = LeftArrApp (id l) <$> (desugar exp1) <*> (desugar exp2)
  desugar (RightArrApp l exp1 exp2) = RightArrApp (id l) <$> (desugar exp1) <*> (desugar exp2)
  desugar (LeftArrHighApp l exp1 exp2) = LeftArrHighApp (id l) <$> (desugar exp1) <*> (desugar exp2)
  desugar (RightArrHighApp l exp1 exp2) = RightArrHighApp (id l) <$> (desugar exp1) <*> (desugar exp2)
  desugar (LCase l alt) = LCase (id l) <$> sequence (fmap (desugar) alt)
  desugar (ExprHole l) = return $ ExprHole (id l)

instance Monad m => Desugar m (ExportSpec l) where
  desugar (EVar l qName) = EVar (id l) <$> (desugar qName)
  desugar (EAbs l namespace qName) = EAbs (id l) <$> (desugar namespace) <*> (desugar qName)
  desugar (EThingAll l qName) = EThingAll (id l) <$> (desugar qName)
  desugar (EThingWith l qName cName) = EThingWith (id l) <$> (desugar qName) <*> sequence (fmap (desugar) cName)
  desugar (EModuleContents l moduleName) = EModuleContents (id l) <$> (desugar moduleName)

instance Monad m => Desugar m (ExportSpecList l) where
  desugar (ExportSpecList l exportSpec) = ExportSpecList (id l) <$> sequence (fmap (desugar) exportSpec)

instance Monad m => Desugar m (FieldDecl l) where
  desugar (FieldDecl l name type0) = FieldDecl (id l) <$> sequence (fmap (desugar) name) <*> (desugar type0)

instance Monad m => Desugar m (FieldUpdate l) where
  desugar (FieldUpdate l qName exp) = FieldUpdate (id l) <$> (desugar qName) <*> (desugar exp)
  desugar (FieldPun l qName) = FieldPun (id l) <$> (desugar qName)
  desugar (FieldWildcard l) = return $ FieldWildcard (id l)

instance Monad m => Desugar m (FunDep l) where
  desugar (FunDep l name1 name2) = FunDep (id l) <$> sequence (fmap (desugar) name1) <*> sequence (fmap (desugar) name2)

instance Monad m => Desugar m (GadtDecl l) where
  desugar (GadtDecl l name fieldDecl type0) = GadtDecl (id l) <$> (desugar name) <*> sequence (fmap (sequence . fmap (desugar)) fieldDecl) <*> (desugar type0)

instance Monad m => Desugar m (GuardedRhs l) where
  desugar (GuardedRhs l stmt exp) = GuardedRhs (id l) <$> sequence (fmap (desugar) stmt) <*> (desugar exp)

instance Monad m => Desugar m (IPBind l) where
  desugar (IPBind l iPName exp) = IPBind (id l) <$> (desugar iPName) <*> (desugar exp)

instance Monad m => Desugar m (IPName l) where
  desugar (IPDup l string) = return $ IPDup (id l) (id string)
  desugar (IPLin l string) = return $ IPLin (id l) (id string)

instance Monad m => Desugar m (ImportDecl l) where
  desugar (ImportDecl importAnn importModule importQualified importSrc importSafe importPkg importAs importSpecs) = ImportDecl (id importAnn) <$> (desugar importModule) <*> return (id importQualified) <*> return (id importSrc) <*> return (id importSafe) <*> return (fmap (id) importPkg) <*> sequence (fmap (desugar) importAs) <*> sequence (fmap (desugar) importSpecs)

instance Monad m => Desugar m (ImportSpec l) where
  desugar (IVar l name) = IVar (id l) <$> (desugar name)
  desugar (IAbs l namespace name) = IAbs (id l) <$> (desugar namespace) <*> (desugar name)
  desugar (IThingAll l name) = IThingAll (id l) <$> (desugar name)
  desugar (IThingWith l name cName) = IThingWith (id l) <$> (desugar name) <*> sequence (fmap (desugar) cName)

instance Monad m => Desugar m (ImportSpecList l) where
  desugar (ImportSpecList l bool importSpec) = ImportSpecList (id l) (id bool) <$> sequence (fmap (desugar) importSpec)

instance Monad m => Desugar m (InstDecl l) where
  desugar (InsDecl l decl) = InsDecl (id l) <$> (desugar decl)
  desugar (InsType l type1 type2) = InsType (id l) <$> (desugar type1) <*> (desugar type2)
  desugar (InsData l dataOrNew type0 qualConDecl deriving0) = InsData (id l) <$> (desugar dataOrNew) <*> (desugar type0) <*> sequence (fmap (desugar) qualConDecl) <*> sequence (fmap (desugar) deriving0)
  desugar (InsGData l dataOrNew type0 kind gadtDecl deriving0) = InsGData (id l) <$> (desugar dataOrNew) <*> (desugar type0) <*> sequence (fmap (desugar) kind) <*> sequence (fmap (desugar) gadtDecl) <*> sequence (fmap (desugar) deriving0)

instance Monad m => Desugar m (InstHead l) where
  desugar (IHCon l qName) = IHCon (id l) <$> (desugar qName)
  desugar (IHInfix l type0 qName) = IHInfix (id l) <$> (desugar type0) <*> (desugar qName)
  desugar (IHParen l instHead) = IHParen (id l) <$> (desugar instHead)
  desugar (IHApp l instHead type0) = IHApp (id l) <$> (desugar instHead) <*> (desugar type0)

instance Monad m => Desugar m (InstRule l) where
  desugar (IRule l tyVarBind context instHead) = IRule (id l) <$> sequence (fmap (sequence . fmap (desugar)) tyVarBind) <*> sequence (fmap (desugar) context) <*> (desugar instHead)
  desugar (IParen l instRule) = IParen (id l) <$> (desugar instRule)

instance Monad m => Desugar m (Kind l) where
  desugar (KindStar l) = return $ KindStar (id l)
  desugar (KindFn l kind1 kind2) = KindFn (id l) <$> (desugar kind1) <*> (desugar kind2)
  desugar (KindParen l kind) = KindParen (id l) <$> (desugar kind)
  desugar (KindVar l qName) = KindVar (id l) <$> (desugar qName)
  desugar (KindApp l kind1 kind2) = KindApp (id l) <$> (desugar kind1) <*> (desugar kind2)
  desugar (KindTuple l kind) = KindTuple (id l) <$> sequence (fmap (desugar) kind)
  desugar (KindList l kind) = KindList (id l) <$> (desugar kind)

instance Monad m => Desugar m (Literal l) where
  desugar (Char l char string) = return $ Char (id l) (id char) (id string)
  desugar (String l string1 string2) = return $ String (id l) (id string1) (id string2)
  desugar (Int l integer string) = return $ Int (id l) (id integer) (id string)
  desugar (Frac l rational string) = return $ Frac (id l) (id rational) (id string)
  desugar (PrimInt l integer string) = return $ PrimInt (id l) (id integer) (id string)
  desugar (PrimWord l integer string) = return $ PrimWord (id l) (id integer) (id string)
  desugar (PrimFloat l rational string) = return $ PrimFloat (id l) (id rational) (id string)
  desugar (PrimDouble l rational string) = return $ PrimDouble (id l) (id rational) (id string)
  desugar (PrimChar l char string) = return $ PrimChar (id l) (id char) (id string)
  desugar (PrimString l string1 string2) = return $ PrimString (id l) (id string1) (id string2)

instance Monad m => Desugar m (Match l) where
  desugar (Match l name pat rhs binds) = Match (id l) <$> (desugar name) <*> sequence (fmap (desugar) pat) <*> (desugar rhs) <*> sequence (fmap (desugar) binds)
  desugar (InfixMatch l pat1 name pat2 rhs binds) = InfixMatch (id l) <$> (desugar pat1) <*> (desugar name) <*> sequence (fmap (desugar) pat2) <*> (desugar rhs) <*> sequence (fmap (desugar) binds)

instance Monad m => Desugar m (Module l) where
  desugar (Module l moduleHead modulePragma importDecl decl) = Module (id l) <$> sequence (fmap (desugar) moduleHead) <*> sequence (fmap (desugar) modulePragma) <*> sequence (fmap (desugar) importDecl) <*> sequence (fmap (desugar) decl)
  desugar (XmlPage l moduleName modulePragma xName xAttr exp1 exp2) = XmlPage (id l) <$> (desugar moduleName) <*> sequence (fmap (desugar) modulePragma) <*> (desugar xName) <*> sequence (fmap (desugar) xAttr) <*> sequence (fmap (desugar) exp1) <*> sequence (fmap (desugar) exp2)
  desugar (XmlHybrid l moduleHead modulePragma importDecl decl xName xAttr exp1 exp2) = XmlHybrid (id l) <$> sequence (fmap (desugar) moduleHead) <*> sequence (fmap (desugar) modulePragma) <*> sequence (fmap (desugar) importDecl) <*> sequence (fmap (desugar) decl) <*> (desugar xName) <*> sequence (fmap (desugar) xAttr) <*> sequence (fmap (desugar) exp1) <*> sequence (fmap (desugar) exp2)

instance Monad m => Desugar m (ModuleHead l) where
  desugar (ModuleHead l moduleName warningText exportSpecList) = ModuleHead (id l) <$> (desugar moduleName) <*> sequence (fmap (desugar) warningText) <*> sequence (fmap (desugar) exportSpecList)

instance Monad m => Desugar m (ModuleName l) where
  desugar (ModuleName l string) = return $ ModuleName (id l) (id string)

instance Monad m => Desugar m (ModulePragma l) where
  desugar (LanguagePragma l name) = LanguagePragma (id l) <$> sequence (fmap (desugar) name)
  desugar (OptionsPragma l tool string) = OptionsPragma (id l) <$> sequence (fmap (desugar) tool) <*> return (id string)
  desugar (AnnModulePragma l annotation) = AnnModulePragma (id l) <$> (desugar annotation)

instance Monad m => Desugar m (Name l) where
  desugar (Ident l string) = return $ Ident (id l) (id string)
  desugar (Symbol l string) = return $ Symbol (id l) (id string)

instance Monad m => Desugar m (Namespace l) where
  desugar (NoNamespace l) = return $ NoNamespace (id l)
  desugar (TypeNamespace l) = return $ TypeNamespace (id l)
  desugar (PatternNamespace l) = return $ PatternNamespace (id l)

instance Monad m => Desugar m (Op l) where
  desugar (VarOp l name) = VarOp (id l) <$> (desugar name)
  desugar (ConOp l name) = ConOp (id l) <$> (desugar name)

instance Monad m => Desugar m (Overlap l) where
  desugar (NoOverlap l) = return $ NoOverlap (id l)
  desugar (Overlap l) = return $ Overlap (id l)
  desugar (Incoherent l) = return $ Incoherent (id l)

instance Monad m => Desugar m (PXAttr l) where
  desugar (PXAttr l xName pat) = PXAttr (id l) <$> (desugar xName) <*> (desugar pat)

instance Monad m => Desugar m (Pat l) where
  desugar (PVar l name) = PVar (id l) <$> (desugar name)
  desugar (PLit l sign literal) = PLit (id l) <$> (desugar sign) <*> (desugar literal)
  desugar (PNPlusK l name integer) = PNPlusK (id l) <$> (desugar name) <*> return (id integer)
  desugar (PInfixApp l pat1 qName pat2) = PInfixApp (id l) <$> (desugar pat1) <*> (desugar qName) <*> (desugar pat2)
  desugar (PApp l qName pat) = PApp (id l) <$> (desugar qName) <*> sequence (fmap (desugar) pat)
  desugar (PTuple l boxed pat) = PTuple (id l) <$> (desugar boxed) <*> sequence (fmap (desugar) pat)
  desugar (PList l pat) = PList (id l) <$> sequence (fmap (desugar) pat)
  desugar (PParen l pat) = PParen (id l) <$> (desugar pat)
  desugar (PRec l qName patField) = PRec (id l) <$> (desugar qName) <*> sequence (fmap (desugar) patField)
  desugar (PAsPat l name pat) = PAsPat (id l) <$> (desugar name) <*> (desugar pat)
  desugar (PWildCard l) = return $ PWildCard (id l)
  desugar (PIrrPat l pat) = PIrrPat (id l) <$> (desugar pat)
  desugar (PatTypeSig l pat type0) = PatTypeSig (id l) <$> (desugar pat) <*> (desugar type0)
  desugar (PViewPat l exp pat) = PViewPat (id l) <$> (desugar exp) <*> (desugar pat)
  desugar (PRPat l rPat) = PRPat (id l) <$> sequence (fmap (desugar) rPat)
  desugar (PXTag l xName pXAttr pat1 pat2) = PXTag (id l) <$> (desugar xName) <*> sequence (fmap (desugar) pXAttr) <*> sequence (fmap (desugar) pat1) <*> sequence (fmap (desugar) pat2)
  desugar (PXETag l xName pXAttr pat) = PXETag (id l) <$> (desugar xName) <*> sequence (fmap (desugar) pXAttr) <*> sequence (fmap (desugar) pat)
  desugar (PXPcdata l string) = return $ PXPcdata (id l) (id string)
  desugar (PXPatTag l pat) = PXPatTag (id l) <$> (desugar pat)
  desugar (PXRPats l rPat) = PXRPats (id l) <$> sequence (fmap (desugar) rPat)
  desugar (PQuasiQuote l string1 string2) = return $ PQuasiQuote (id l) (id string1) (id string2)
  desugar (PBangPat l pat) = PBangPat (id l) <$> (desugar pat)

instance Monad m => Desugar m (PatField l) where
  desugar (PFieldPat l qName pat) = PFieldPat (id l) <$> (desugar qName) <*> (desugar pat)
  desugar (PFieldPun l qName) = PFieldPun (id l) <$> (desugar qName)
  desugar (PFieldWildcard l) = return $ PFieldWildcard (id l)

instance Monad m => Desugar m (PatternSynDirection l) where
  desugar (Unidirectional) = return $ Unidirectional
  desugar (ImplicitBidirectional) = return $ ImplicitBidirectional
  desugar (ExplicitBidirectional l decl) = ExplicitBidirectional (id l) <$> sequence (fmap (desugar) decl)

instance Monad m => Desugar m (Promoted l) where
  desugar (PromotedInteger l integer string) = return $ PromotedInteger (id l) (id integer) (id string)
  desugar (PromotedString l string1 string2) = return $ PromotedString (id l) (id string1) (id string2)
  desugar (PromotedCon l bool qName) = PromotedCon (id l) (id bool) <$> (desugar qName)
  desugar (PromotedList l bool type0) = PromotedList (id l) (id bool) <$> sequence (fmap (desugar) type0)
  desugar (PromotedTuple l type0) = PromotedTuple (id l) <$> sequence (fmap (desugar) type0)
  desugar (PromotedUnit l) = return $ PromotedUnit (id l)

instance Monad m => Desugar m (QName l) where
  desugar (Qual l moduleName name) = Qual (id l) <$> (desugar moduleName) <*> (desugar name)
  desugar (UnQual l name) = UnQual (id l) <$> (desugar name)
  desugar (Special l specialCon) = Special (id l) <$> (desugar specialCon)

instance Monad m => Desugar m (QOp l) where
  desugar (QVarOp l qName) = QVarOp (id l) <$> (desugar qName)
  desugar (QConOp l qName) = QConOp (id l) <$> (desugar qName)

instance Monad m => Desugar m (QualConDecl l) where
  desugar (QualConDecl l tyVarBind context conDecl) = QualConDecl (id l) <$> sequence (fmap (sequence . fmap (desugar)) tyVarBind) <*> sequence (fmap (desugar) context) <*> (desugar conDecl)

instance Monad m => Desugar m (QualStmt l) where
  desugar (QualStmt l stmt) = QualStmt (id l) <$> (desugar stmt)
  desugar (ThenTrans l exp) = ThenTrans (id l) <$> (desugar exp)
  desugar (ThenBy l exp1 exp2) = ThenBy (id l) <$> (desugar exp1) <*> (desugar exp2)
  desugar (GroupBy l exp) = GroupBy (id l) <$> (desugar exp)
  desugar (GroupUsing l exp) = GroupUsing (id l) <$> (desugar exp)
  desugar (GroupByUsing l exp1 exp2) = GroupByUsing (id l) <$> (desugar exp1) <*> (desugar exp2)

instance Monad m => Desugar m (RPat l) where
  desugar (RPOp l rPat rPatOp) = RPOp (id l) <$> (desugar rPat) <*> (desugar rPatOp)
  desugar (RPEither l rPat1 rPat2) = RPEither (id l) <$> (desugar rPat1) <*> (desugar rPat2)
  desugar (RPSeq l rPat) = RPSeq (id l) <$> sequence (fmap (desugar) rPat)
  desugar (RPGuard l pat stmt) = RPGuard (id l) <$> (desugar pat) <*> sequence (fmap (desugar) stmt)
  desugar (RPCAs l name rPat) = RPCAs (id l) <$> (desugar name) <*> (desugar rPat)
  desugar (RPAs l name rPat) = RPAs (id l) <$> (desugar name) <*> (desugar rPat)
  desugar (RPParen l rPat) = RPParen (id l) <$> (desugar rPat)
  desugar (RPPat l pat) = RPPat (id l) <$> (desugar pat)

instance Monad m => Desugar m (RPatOp l) where
  desugar (RPStar l) = return $ RPStar (id l)
  desugar (RPStarG l) = return $ RPStarG (id l)
  desugar (RPPlus l) = return $ RPPlus (id l)
  desugar (RPPlusG l) = return $ RPPlusG (id l)
  desugar (RPOpt l) = return $ RPOpt (id l)
  desugar (RPOptG l) = return $ RPOptG (id l)

instance Monad m => Desugar m (Rhs l) where
  desugar (UnGuardedRhs l exp) = UnGuardedRhs (id l) <$> (desugar exp)
  desugar (GuardedRhss l guardedRhs) = GuardedRhss (id l) <$> sequence (fmap (desugar) guardedRhs)

instance Monad m => Desugar m (Role l) where
  desugar (Nominal l) = return $ Nominal (id l)
  desugar (Representational l) = return $ Representational (id l)
  desugar (Phantom l) = return $ Phantom (id l)
  desugar (RoleWildcard l) = return $ RoleWildcard (id l)

instance Monad m => Desugar m (Rule l) where
  desugar (Rule l string activation ruleVar exp1 exp2) = Rule (id l) (id string) <$> sequence (fmap (desugar) activation) <*> sequence (fmap (sequence . fmap (desugar)) ruleVar) <*> (desugar exp1) <*> (desugar exp2)

instance Monad m => Desugar m (RuleVar l) where
  desugar (RuleVar l name) = RuleVar (id l) <$> (desugar name)
  desugar (TypedRuleVar l name type0) = TypedRuleVar (id l) <$> (desugar name) <*> (desugar type0)

instance Monad m => Desugar m (Safety l) where
  desugar (PlayRisky l) = return $ PlayRisky (id l)
  desugar (PlaySafe l bool) = return $ PlaySafe (id l) (id bool)
  desugar (PlayInterruptible l) = return $ PlayInterruptible (id l)

instance Monad m => Desugar m (Sign l) where
  desugar (Signless l) = return $ Signless (id l)
  desugar (Negative l) = return $ Negative (id l)

instance Monad m => Desugar m (SpecialCon l) where
  desugar (UnitCon l) = return $ UnitCon (id l)
  desugar (ListCon l) = return $ ListCon (id l)
  desugar (FunCon l) = return $ FunCon (id l)
  desugar (TupleCon l boxed int) = TupleCon (id l) <$> (desugar boxed) <*> return (id int)
  desugar (Cons l) = return $ Cons (id l)
  desugar (UnboxedSingleCon l) = return $ UnboxedSingleCon (id l)

instance Monad m => Desugar m (Splice l) where
  desugar (IdSplice l string) = return $ IdSplice (id l) (id string)
  desugar (ParenSplice l exp) = ParenSplice (id l) <$> (desugar exp)

instance Monad m => Desugar m (Stmt l) where
  desugar (Generator l pat exp) = Generator (id l) <$> (desugar pat) <*> (desugar exp)
  desugar (Qualifier l exp) = Qualifier (id l) <$> (desugar exp)
  desugar (LetStmt l binds) = LetStmt (id l) <$> (desugar binds)
  desugar (RecStmt l stmt) = RecStmt (id l) <$> sequence (fmap (desugar) stmt)

instance Monad m => Desugar m (Tool) where
  desugar (GHC) = return $ GHC
  desugar (HUGS) = return $ HUGS
  desugar (NHC98) = return $ NHC98
  desugar (YHC) = return $ YHC
  desugar (HADDOCK) = return $ HADDOCK
  desugar (UnknownTool string) = return $ UnknownTool (id string)

instance Monad m => Desugar m (TyVarBind l) where
  desugar (KindedVar l name kind) = KindedVar (id l) <$> (desugar name) <*> (desugar kind)
  desugar (UnkindedVar l name) = UnkindedVar (id l) <$> (desugar name)

instance Monad m => Desugar m (Type l) where
  desugar (TyForall l tyVarBind context type0) = TyForall (id l) <$> sequence (fmap (sequence . fmap (desugar)) tyVarBind) <*> sequence (fmap (desugar) context) <*> (desugar type0)
  desugar (TyFun l type1 type2) = TyFun (id l) <$> (desugar type1) <*> (desugar type2)
  desugar (TyTuple l boxed type0) = TyTuple (id l) <$> (desugar boxed) <*> sequence (fmap (desugar) type0)
  desugar (TyList l type0) = TyList (id l) <$> (desugar type0)
  desugar (TyParArray l type0) = TyParArray (id l) <$> (desugar type0)
  desugar (TyApp l type1 type2) = TyApp (id l) <$> (desugar type1) <*> (desugar type2)
  desugar (TyVar l name) = TyVar (id l) <$> (desugar name)
  desugar (TyCon l qName) = TyCon (id l) <$> (desugar qName)
  desugar (TyParen l type0) = TyParen (id l) <$> (desugar type0)
  desugar (TyInfix l type1 qName type2) = TyInfix (id l) <$> (desugar type1) <*> (desugar qName) <*> (desugar type2)
  desugar (TyKind l type0 kind) = TyKind (id l) <$> (desugar type0) <*> (desugar kind)
  desugar (TyPromoted l promoted) = TyPromoted (id l) <$> (desugar promoted)
  desugar (TyEquals l type1 type2) = TyEquals (id l) <$> (desugar type1) <*> (desugar type2)
  desugar (TySplice l splice) = TySplice (id l) <$> (desugar splice)
  desugar (TyBang l bangType type0) = TyBang (id l) <$> (desugar bangType) <*> (desugar type0)
  desugar (TyWildCard l name) = TyWildCard (id l) <$> sequence (fmap (desugar) name)

instance Monad m => Desugar m (TypeEqn l) where
  desugar (TypeEqn l type1 type2) = TypeEqn (id l) <$> (desugar type1) <*> (desugar type2)

instance Monad m => Desugar m (WarningText l) where
  desugar (DeprText l string) = return $ DeprText (id l) (id string)
  desugar (WarnText l string) = return $ WarnText (id l) (id string)

instance Monad m => Desugar m (XAttr l) where
  desugar (XAttr l xName exp) = XAttr (id l) <$> (desugar xName) <*> (desugar exp)

instance Monad m => Desugar m (XName l) where
  desugar (XName l string) = return $ XName (id l) (id string)
  desugar (XDomName l string1 string2) = return $ XDomName (id l) (id string1) (id string2)

