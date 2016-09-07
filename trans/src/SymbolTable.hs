module SymbolTable
  ( moduleReexportClosure
  , makeModuleReexport
  ) where

import Data.Functor
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import qualified Data.Set as S
import Lens.Micro
import Data.Graph
import Data.Array

import Language.Haskell.Exts.Annotated.Syntax
import Language.Haskell.Exts.SrcLoc

import SymbolTableData
import DesugarClass

nameStr :: Name l -> String
nameStr (Ident _ str) = str
nameStr (Symbol _ str) = str

makeModuleReexport :: [[String]] -> ModuleReexport
makeModuleReexport = go M.empty where
  go acc [] = acc
  go acc ([] : strs) = go acc strs
  go acc ((mod : res) : strs) = go (foldr (\re -> M.insertWith (\_ olds -> ModuleName () re : olds) key [ModuleName () re]) (M.insertWith (flip const) key [] acc) res) strs where key = ModuleName () mod

moduleReexportClosure :: ModuleReexport -> ModuleReexport
moduleReexportClosure rawMap =
  let
    sccs = map flattenSCC $ stronglyConnComp $ map (\(mod, reexports) -> (mod, mod, reexports)) (M.toList rawMap)
    sccN = length sccs
    sccArray = listArray (1, sccN) sccs
    modToSccI = M.fromList $ concatMap (\(i, scc) -> map (, i) scc) (zip [1..] sccs)
    sccClosure = listArray (1, sccN) (map genSccClosure [1..]) where
      genSccClosure i = S.toList $ S.fromList $ i :
        ( concatMap (sccClosure !)
        $ S.toList
        $ S.delete i
        $ S.fromList
        $ concatMap (\mod -> map (modToSccI M.!) (rawMap M.! mod)) (sccArray ! i)
        )
    sccClosureMod = listArray (1, sccN) (map (S.toList . S.fromList . concatMap (sccArray !)) (elems sccClosure))
  in
    M.fromList $ map (\(mod, _) -> (mod, sccClosureMod ! (modToSccI M.! mod))) (M.toList rawMap)

lookupSymbolTableLayer :: SymbolTableLayer -> Name () -> SymbolQueryResult
lookupSymbolTableLayer syml name =
  maybe SymbolQueryNotFound id (M.lookup name syml)

lookupSymbolTable :: SymbolTable -> Maybe (ModuleName ()) -> Name () -> SymbolQueryResult
lookupSymbolTable symt Nothing name = localQuery (symtLocal symt) where
  localQuery [] = SymbolQueryNotFound
  localQuery (syml : symls) = case lookupSymbolTableLayer syml name of
    SymbolQueryNotFound -> localQuery symls
    others -> others

addSymbolTableLayer :: SymbolTable -> SymbolTable
addSymbolTableLayer symt = symt & symtLocalL %~ (emptySymbolTableLayer :)

addSymbolToSymbolTableLayer :: SymbolProperty -> SymbolTableLayer -> SymbolTableLayer
addSymbolToSymbolTableLayer prop =
  M.insertWith whenDup
    (fmap (const ()) (sympName prop))
    (SymbolQuerySuccess prop)
  where
    nameSrcSpan = ann (sympName prop)
    whenDup _ (SymbolQuerySuccess oldProp) = SymbolQueryDup [nameSrcSpan, ann (sympName oldProp)]
    whenDup _ (SymbolQueryDup oldSrcSpans) = SymbolQueryDup (nameSrcSpan : oldSrcSpans)

addLocalSymbol :: SymbolProperty -> SymbolTable -> SymbolTable
addLocalSymbol prop symt = symt & symtLocalL . ix 0 %~ M.insertWith amend name (SymbolQuerySuccess prop) where
  name = fmap (const ()) (sympName prop)
  amend _ (SymbolQuerySuccess old) = SymbolQueryDup (map (ann . sympName) [prop, old])
  amend _ (SymbolQueryDup dups) = SymbolQueryDup (ann (sympName prop) : dups)

extractNameFromPattern :: Pat l -> [Name l]
extractNameFromPattern (PVar _ name) = [name]
extractNameFromPattern (PLit _ _ _) = []
extractNameFromPattern (PNPlusK _ _ _) = []
extractNameFromPattern (PInfixApp _ pat1 _ pat2) = extractNameFromPattern pat1 ++ extractNameFromPattern pat2
extractNameFromPattern (PApp _ _ pats) = concatMap extractNameFromPattern pats
extractNameFromPattern (PTuple _ _ pats) = concatMap extractNameFromPattern pats
extractNameFromPattern (PList _ pats) = concatMap extractNameFromPattern pats
extractNameFromPattern (PParen _ pat) = extractNameFromPattern pat
extractNameFromPattern (PRec _ _ fpats) = concatMap extractFpat fpats where
  extractFpat (PFieldPat _ _ pat) = extractNameFromPattern pat
  extractFpat (PFieldPun _ qname) = case qname of
    Qual _ _ name -> [name]
    UnQual _ name -> [name]
    Special _ _ -> []
  extractFpat (PFieldWildcard _) = []
extractNameFromPattern (PAsPat _ name pat) = name : extractNameFromPattern pat
extractNameFromPattern (PWildCard _) = []
extractNameFromPattern (PIrrPat _ pat) = extractNameFromPattern pat
extractNameFromPattern (PatTypeSig _ pat _) = extractNameFromPattern pat
extractNameFromPattern (PViewPat _ _ pat) = extractNameFromPattern pat
extractNameFromPattern (PBangPat _ pat) = extractNameFromPattern pat
extractNameFromPattern pat = error $ "extractNameFromPattern: unsupported pattern " ++ show (fmap (const ()) pat)

extractExportSymbol :: Module SrcSpan -> (SymbolTableLayer, [String])
extractExportSymbol mod =
  case mod of
    (Module _ Nothing _ _ decls) -> (implicitExport decls, [])
    (Module _ (Just (ModuleHead _ _ _ Nothing)) _ _ decls) -> (implicitExport decls, [])
    (Module _ (Just (ModuleHead _ _ _ (Just (ExportSpecList _ exports)))) _ _ decls) -> explicitExport decls exports
  where
    implicitExport :: [Decl SrcSpan] -> SymbolTableLayer
    implicitExport decls = go M.empty M.empty decls where
      amend :: Either [SrcSpan] SrcSpan -> Either [SrcSpan] SrcSpan -> Either [SrcSpan] SrcSpan
      amend (Right srcSpan) (Left oldSrcSpans) = Left (srcSpan : oldSrcSpans)
      amend (Right srcSpan) (Right oldSrcSpan) = Left [srcSpan, oldSrcSpan]

      go :: M.Map (Name ()) (Either [SrcSpan] SrcSpan) -> M.Map (Name ()) (Int, Assoc SrcSpan) -> [Decl SrcSpan] -> SymbolTableLayer
      go nameMap fixityMap [] = M.mapWithKey assignFixity nameMap
        where
          assignFixity name (Right nameSrcSpan) =
            SymbolQuerySuccess $ SymbolProperty
              (fmap (const nameSrcSpan) name)
              prio assoc
            where
              (prio, assoc) = M.findWithDefault (9, AssocNone nameSrcSpan) name fixityMap
          assignFixity name (Left nameSrcSpans) =
            SymbolQueryDup nameSrcSpans

      go nameMap fixityMap (decl:decls) = case decl of
        InfixDecl srcSpan assoc (maybe 9 id -> prio) ops -> go nameMap fixityMap' decls
          where
            fixityMap' = goOp fixityMap ops
            goOp acc [] = acc
            goOp acc (op : ops) = goOp (M.insert opName (prio, assoc) acc) ops
              where
                opName = fmap (const ()) $ case op of
                  VarOp _ name -> name
                  ConOp _ name -> name

        PatBind srcSpan pat rhs mBinds -> go nameMap' fixityMap decls
          where
            nameMap' = foldr
              (\name -> M.insertWith amend (fmap (const ()) name) (Right (ann name)))
              nameMap
              (extractNameFromPattern pat)

        FunBind srcSpan (match:_) -> case match of
          Match _ name _ _ _ ->
            go (M.insertWith amend (fmap (const ()) name) (Right (ann name)) nameMap) fixityMap decls

        _ -> go nameMap fixityMap decls

    explicitExport decls = go M.empty S.empty (implicitExport decls) where
      go syml expMods impNames (e:es) = case e of
        EVar _ (UnQual _ name) ->
          go (maybe id (M.insert key) (M.lookup key impNames) syml) expMods impNames es
          where key = fmap (const ()) name
        EAbs _ (NoNamespace _) (UnQual _ name) ->
          go (maybe id (M.insert key) (M.lookup key impNames) syml) expMods impNames es
          where key = fmap (const ()) name
        EThingAll _ (UnQual _ name) -> -- XXX 未實作 export record field
          go (maybe id (M.insert key) (M.lookup key impNames) syml) expMods impNames es
          where key = fmap (const ()) name
        EThingWith _ (UnQual _ name) cnames -> go syml' expMods impNames es
          where
            syml' = foldr
              (\ (fmap (const ()) -> key) -> maybe id (M.insert key) (M.lookup key impNames))
              syml (name : map cnameToName cnames)
            cnameToName (VarName _ name) = name
            cnameToName (ConName _ name) = name
        EModuleContents _ (ModuleName _ name) ->
          go syml (S.insert name expMods) impNames es
        _ -> go syml expMods impNames es
          -- XXX 未實作 export 來自別的 module 的 symbol
          -- XXX 未實作 export Type / Pattern
      go syml expMods _ _ = (syml, S.toAscList expMods)

importSymbols :: ImportDecl l -> SymbolTableLayer -> SymbolTable -> SymbolTable
importSymbols = undefined


