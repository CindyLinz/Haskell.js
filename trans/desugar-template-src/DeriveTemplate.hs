module DeriveTemplate where

import Data.Functor
import Data.List
import Data.Char
import Data.Monoid
import Control.Applicative
import Control.Monad
import qualified Data.Set as S
import qualified Data.Map as M
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

trivialTypes :: S.Set String
trivialTypes = S.fromList ["String", "Maybe", "Int", "Rational", "Char", "Integer", "Bool"]

deriveDesugarTemplate :: String -> Q [Dec]
deriveDesugarTemplate funName = do
  Just moduleName <- lookupTypeName "Module"

  moduleInfo <- reify moduleName
  runIO $ putStrLn $ show moduleInfo

  moduleInfo <- reify moduleName

  let code = genDataTransformer "deIf" moduleInfo
  runIO $ putStrLn code

  allComponent <- collectAllNonTrivialComponent moduleName
  runIO $ putStrLn (show allComponent)

  allCode <- forM (S.toList allComponent) $ \name -> do
    info <- reify name
    let code = genDataTransformer "deIf" info
    runIO $ putStrLn code
    return code

  fmap pure $ funD (mkName funName) [clause (map (varP . mkName) ["modName", "funPrefix"]) (normalB (varE (mkName "undefined"))) []]

maybeVarNameFromType :: Type -> Maybe Name
maybeVarNameFromType (ConT (name @ (Name (OccName nameStr) _)))
  | S.member nameStr trivialTypes = Nothing
  | otherwise = Just name
maybeVarNameFromType (AppT f x) = maybeVarNameFromType f <|> maybeVarNameFromType x
maybeVarNameFromType _ = Nothing

varNameFromType :: Type -> Name
varNameFromType (ConT name) = name
varNameFromType (all @ (AppT f x)) = case maybeVarNameFromType all of
  Just o -> o
  Nothing -> varNameFromType x
varNameFromType others = error $ "varNameFromType " ++ show others ++ " not implemented"

varNamesForNormalSlots :: [StrictType] -> [String]
varNamesForNormalSlots slots =
  let
    dupNames :: S.Set String
    dupNames = go S.empty S.empty rawNames where
      go res seen (nameStr : others) =
        if S.member nameStr seen then
          go (S.insert nameStr res) seen others
        else
          go res (S.insert nameStr seen) others
      go res _ _ = res

    rawNames = map (\(_, ty) -> case varNameFromType ty of Name (OccName str) _ -> lowerHead str) slots

    finalNames = go M.empty rawNames where
      go nameCount (nameStr : others) =
        if S.member nameStr dupNames then
          let
            nameCount' = M.insertWith (+) nameStr 1 nameCount
            serial = nameCount' M.! nameStr
          in
            (nameStr ++ show serial) : go nameCount' others
        else
          nameStr : go nameCount others
      go _ _ = []
  in
    finalNames

transExprFromType :: String -> Type -> String
transExprFromType funPrefix = genExpr where
  genExpr = \case
    ConT (Name (OccName name) _)
      | S.member name trivialTypes -> "id"
      | otherwise -> funPrefix ++ name
    AppT ListT x -> "fmap (" ++ genExpr x ++ ")"
    AppT (ConT (Name (OccName "Maybe") _)) x -> "fmap (" ++ genExpr x ++ ")"
    AppT (AppT (TupleT 2) a) b -> "(" ++ genExpr a ++ ") *** (" ++ genExpr b ++ ")"
    others -> error $ "exprFromType " ++ show others ++ " not implemented"

exprFromType :: String -> String -> Type -> String
exprFromType name funPrefix ty = "(" ++ transExprFromType funPrefix ty ++ " " ++ name ++ ")"

lowerHead :: String -> String
lowerHead (c:cs) = toLower c : cs
lowerHead _ = []

collectAllNonTrivialComponent :: Name -> Q (S.Set Name)
collectAllNonTrivialComponent root = go S.empty (S.singleton root) where
  go res pending =
    case S.minView pending of
      Nothing ->
        return res
      Just (name, others)
        | S.member name res ->
          go res others
        | otherwise -> do
          info <- reify name
          go (S.insert name res) (nonTrivialComponentInInfo info `S.union` others)

nonTrivialComponentInType :: Type -> S.Set Name
nonTrivialComponentInType = \case
  ConT (name @ (Name (OccName nameStr) _))
    | S.member nameStr trivialTypes -> S.empty
    | otherwise -> S.singleton name
  AppT f x -> nonTrivialComponentInType f `S.union` nonTrivialComponentInType x
  ListT -> S.empty
  TupleT _ -> S.empty
  others -> error $ "nonTrivialComponentInType " ++ show others ++ " not implemented"

nonTrivialComponentInInfo :: Info -> S.Set Name
nonTrivialComponentInInfo = \case
  TyConI (DataD [] _ [] cons _) -> mconcat (map nonTrivialComponentInCon cons)
  TyConI (TySynD _ [] ty) -> nonTrivialComponentInType ty
  TyConI (NewtypeD [] _ [] con _) -> nonTrivialComponentInCon con
  others -> error $ "nonTrivialComponentInInfo " ++ show others ++ " not implemented"

nonTrivialComponentInCon :: Con -> S.Set Name
nonTrivialComponentInCon = \case
  NormalC _ slots -> mconcat (map (nonTrivialComponentInType . snd) slots)
  RecC _ slots -> mconcat (map (\(_, _, ty) -> nonTrivialComponentInType ty) slots)
  others -> error $ "nonTrivialComponentInCon " ++ show others ++ " not implemented"

genDataTransformer :: String -> Info -> String
genDataTransformer funPrefix =
  let
    conToDef tyNameStr (NormalC (Name (OccName conNameStr) _) slots) =
      let
        varNames = varNamesForNormalSlots slots
      in
        funPrefix ++ tyNameStr ++
          " (" ++ intercalate " " (conNameStr : varNames) ++ ") = " ++
          intercalate " " (conNameStr : zipWith (\varNameStr (_, ty) -> exprFromType varNameStr funPrefix ty) varNames slots) ++
          "\n"

    conToDef tyNameStr (RecC (Name (OccName conNameStr) _) slots) =
      let
        varNames = map (\(Name (OccName nameStr) _, _, _) -> nameStr) slots
      in
        funPrefix ++ tyNameStr ++
          " (" ++ intercalate " " (conNameStr : varNames) ++ ") = " ++
          intercalate " " (conNameStr : zipWith (\varNameStr (_, _, ty) -> exprFromType varNameStr funPrefix ty) varNames slots) ++
          "\n"

    conToDef tyNameStr others = error $ "genCon " ++ show others ++ " not implemented"

  in
    \case
      TyConI (DataD [] (Name (OccName tyNameStr) _) [] cons _) ->
        let
          typeSig = funPrefix ++ tyNameStr ++ " :: " ++ tyNameStr ++ " -> " ++ tyNameStr ++ "\n"

        in
          concat $ typeSig : map (conToDef tyNameStr) cons

      TyConI (TySynD (Name (OccName tyNameStr) _) [] ty) ->
        let
          typeSig = funPrefix ++ tyNameStr ++ " :: " ++ tyNameStr ++ " -> " ++ tyNameStr ++ "\n"
          def = funPrefix ++ tyNameStr ++ " a = " ++ exprFromType "a" funPrefix ty ++ "\n"
        in
          concat [typeSig, def]

      TyConI (NewtypeD [] (Name (OccName tyNameStr) _) [] con _) ->
        let
          typeSig = funPrefix ++ tyNameStr ++ " :: " ++ tyNameStr ++ " -> " ++ tyNameStr ++ "\n"
          def = conToDef tyNameStr con
        in
          concat [typeSig, def]

      others -> error $ "genDataTransformer " ++ show others ++ " not implemented"
