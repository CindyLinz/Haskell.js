module BasicTrans
  ( transModule
  ) where

import Language.Haskell.Exts.Annotated

import Data.Char

import ForgetL

escapeJSString :: String -> String
escapeJSString ('\'' : others) = '\\' : '\'' : escapeJSString others
escapeJSString ('\\' : others) = '\\' : '\\' : escapeJSString others
escapeJSString (ch : others) = ch : escapeJSString others
escapeJSString _ = ""

transName :: Show l => Name l -> String
transName (Ident l name) = "'" ++ escapeJSString name ++ "'"
transName (Symbol l symbol) = "'(" ++ escapeJSString symbol ++ ")'"

transQName :: Show l => QName l -> String
transQName (UnQual l name) = "['var'," ++ transName name ++ "]"
transQName (Special l (UnitCon l2)) = "['var','()']"
transQName (Special l (ListCon l2)) = "['var','[]']"
transQName (Special l (TupleCon l2 boxed n)) = "['var','(" ++ replicate (n - 1) ',' ++ ")']"
transQName (Special l (Cons l2)) = "['var','(:)']"
transQName (Special l (UnboxedSingleCon l2)) = "['var','()']"
transQName (Qual l _ name) = "['var'," ++ transName name ++ "]"
-- XXX: qualified name implemented incorrectly
--transQName (Qual l _ _) = error "qualified name unimplemented"

transLit :: Show l => Literal l -> String
transLit (Char l ch rep) = "['app', ['var', 'C#'], ['dat', String.fromCharCode(" ++ show (ord ch) ++ ")]]"
transLit (Int l i rep) = "['app', ['var', 'I#'], ['dat', " ++ show i ++ "]]"
transLit other = error $ "transLit: " ++ show (forgetL other) ++ " not supported"

transLam :: Show l => [Pat l] -> Exp l -> String
transLam (PVar l name : ps) body = "['lam'," ++ transName name ++ "," ++ transLam ps body ++ "]"
transLam _ body = transExpr body

transSign :: Show l => Sign l -> String
transSign (Signless l) = ""
transSign (Negative l) = "-"

transExpr :: Show l => Exp l -> String
transExpr (Var l qName) = transQName qName
transExpr (Con l qName) = transQName qName
transExpr (Lit l lit) = transLit lit
transExpr (InfixApp l a (QVarOp l2 qName) b) = transExpr (App l (App l (Var l2 qName) a) b)
transExpr (InfixApp l a (QConOp l2 qName) b) = transExpr (App l (App l (Var l2 qName) a) b)
transExpr (App l f x) = "['app'," ++ transExpr f ++ "," ++ transExpr x ++ "]"
transExpr (NegApp l x) = error "NegApp not defined"
transExpr (Lambda l pats body) = transLam pats body
transExpr (Paren l expr) = transExpr expr
transExpr (List l []) = "['var','[]']"
transExpr (Let l (BDecls l2 binds) expr) =
  "['app', ['app', ['var', 'Y#'], ['lam', 'gen#', ['lam', 'tuple#', " ++ genApp (reverse binds) ++ "]]], " ++ genIn binds ++ "]"
  where
    genApp (PatBind l (PVar l2 name) _ _ : bs) = "['app', " ++ genApp bs ++ ", ['app', ['var', 'gen#'], " ++ genExtract binds ++ "]]"
      where
        genExtract (PatBind l (PVar l2 lamName) _ _ : bs) = "['lam', " ++ transName lamName ++ ", " ++ genExtract bs ++ "]"
        genExtract _ = "['var', " ++ transName name ++ "]"
    genApp _ = genDestruct binds
    genDestruct (PatBind l (PVar l2 name) _ _ : bs) = "['lam', " ++ transName name ++ ", " ++ genDestruct bs ++ "]"
    genDestruct _ = genDef (reverse binds)
    genDef (PatBind l _ (UnGuardedRhs l2 expr) _ : bs) = "['app', " ++ genDef bs ++ ", " ++ transExpr expr ++ "]"
    genDef _ = "['var', 'tuple#']"
    genIn (PatBind l (PVar l2 name) _ _ : bs) = "['lam', " ++ transName name ++ ", " ++ genIn bs ++ "]"
    genIn _ = transExpr expr
transExpr (Case l target alts) = case alts of
  (Alt l2 (PVar l3 name) (UnGuardedRhs l4 expr) Nothing : _) -> -- case target of name -> expr
    "['app',['lam'," ++ transName name ++ "," ++ transExpr expr ++ "]," ++ transExpr target ++ "]"
  (Alt l2 (PLit l3 _ (Int l4 _ _)) _ _ : _) -> -- 整數 literal: 1, 2, 3, ..
    "['app', " ++ transExpr target ++ ", " ++ genPrimIntMatch alts ++ "]"
  (Alt l2 (PLit l3 _ (PrimInt l4 _ _)) _ _ : _) -> -- unbox 整數 literal: 1#, 2#, 3#, ...
    "['app', " ++ genPrimIntMatch alts ++ ", " ++ transExpr target ++ "]"
  [Alt l2 (PList l3 []) (UnGuardedRhs l4 exprNil) _, Alt l5 (PApp l6 _ [PVar l7 aName, PVar l8 asName]) (UnGuardedRhs l9 exprCons) _ ] -> -- (G)ADT (for list)
    "['app', ['app', " ++ transExpr target ++ ", " ++ transExpr exprNil ++ "], ['lam', " ++ transName aName ++ ", ['lam', " ++ transName asName ++ ", " ++ transExpr exprCons ++ "]]]"
  (Alt l2 (PApp l3 _ _) _ _ : _) -> -- (G)ADT
    genApp (reverse alts)
    where
      genApp (Alt l2 (PApp l3 _ vars) (UnGuardedRhs l5 expr) Nothing : as) =
        "['app', " ++ genApp as ++ ", " ++ genLam vars ++ "]"
        where
          genLam (PVar l2 name : vs) = "['lam', " ++ transName name ++ ", " ++ genLam vs ++ "]"
          genLam (PWildCard l2 : vs) = "['lam', '_', " ++ genLam vs ++ "]"
          genLam _ = transExpr expr
      genApp _ = transExpr target
  _ -> error $ show alts ++ " unimplemented case pattern"
transExpr others = error $ show others ++ " not implemented"

genPrimIntMatch :: Show l => [Alt l] -> String
genPrimIntMatch alts =
  "['int', 'match-int#', 1, function(target){\n\
    \  var env = this;\n\
    \  target = weak_normal_form(target);\n\
    \  switch(target.expr[1]){\n" ++
    mconcat (flip map alts $ genBranch) ++
    "  }\n\
  \}]"
  where
    genBranch (Alt l (PLit l2 sign (PrimInt l3 n _)) rhs Nothing) =
      "case " ++ transSign sign ++ show n ++ ": " ++ genRHS rhs
    genBranch (Alt l (PLit l2 sign (Int l3 n _)) rhs Nothing) =
      "case " ++ transSign sign ++ show n ++ ": " ++ genRHS rhs
    genBranch (Alt l (PVar l2 name) rhs Nothing) =
      "default: env = clone_env(env); env[" ++ transName name ++ "] = target; " ++ genRHS rhs
    genBranch (Alt l (PWildCard l2) rhs Nothing) =
      "default: " ++ genRHS rhs
    genBranch alt = error $ show alt ++ " not implemented PrimInt branch"
    genRHS (UnGuardedRhs l expr) = "return weak_normal_form({env: env, expr: " ++ transExpr expr ++ "});\n"

transDecl :: Show l => Decl l -> String
transDecl (PatBind l (PVar l2 name) (UnGuardedRhs l3 expr) Nothing) =
  "env[" ++ transName name ++ "] = {env: env, expr: " ++ transExpr expr ++ "};\n\n"
transDecl (GDataDecl l (DataType l2) Nothing (DHead l3 name) mKinds gDecls Nothing) = mconcat $ flip map gDecls $ \(GadtDecl loc name Nothing ty) ->
  let
    slotCount :: Int
    slotCount = count 0 ty where
      count !acc (TyFun l _ other) = count (acc + 1) other
      count !acc _ = acc

    genSlots 0 = genBody gDecls
    genSlots n = "['lam','a" ++ show (slotCount - n + 1) ++ "'," ++ genSlots (n - 1) ++ "]"

    genBody (GadtDecl _ name _ _ : ds) = "['lam'," ++ transName name ++ "," ++ genBody ds ++ "]"
    genBody _ = genApp slotCount

    genApp 0 = "['var'," ++ transName name ++ "]"
    genApp n = "['app'," ++ genApp (n - 1) ++ ",['var','a" ++ show n ++ "']]"
  in
    "env[" ++ transName name ++ "] = {env: env, expr: " ++ genSlots slotCount ++ "};\n\n"
transDecl decl = error $ show decl ++ " not implemented"

transModule :: Show l => Module l -> String
transModule (Module l moduleName pragmas moduleImports moduleDecls) =
  mconcat $ map transDecl moduleDecls
