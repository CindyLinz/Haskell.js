module Desugar.Infix
  (-- deInfixModule
--  , deInfixBinds
--  , deInfixPatternSynDirection
  ) where

import Language.Haskell.Exts.Annotated.Syntax
import qualified Language.Haskell.Exts.Syntax as S
import Language.Haskell.Exts.SrcLoc

--data Fixity l = Fixity Int (Assc l)
--
--extractFixityDecl :: Decl l -> Maybe [Fixity]
--extractFixityDecl (InfixDecl _ assc mPrio ops) = Just $ goOp ops where
--  prio = maybe 9 id mPrio
--
--  annAsscToAssc (AssocNone _) = S.AssocNone
--  annAsscToAssc (AssocLeft _) = S.AssocLeft
--  annAsscToAssc (AssocRight _) = S.AssocRight
--
--  annNameToName (Ident _ str) = S.Ident str
--  annNameToName (Symbol _ str) = S.Symbol str
--
--  goOp (VarOp _ name : ops) = goOneName name ops
--  goOp (ConOp _ name : ops) = goOneName name ops
--  goOp _ = []
--
--  goOneName name ops = Fixity (annAsscToAssc assc) prio (S.UnQual (annNameToName name)) : goOp ops
--extractFixityDecl _ = Nothing
--
--takeFixities :: [Decl l] -> [Fixity]
--takeFixities = concat . map (concat . extractFixityDecl)
--
--reshapeDecls :: Monad m => [Fixity] -> [Decl SrcSpanInfo] -> m [Decl SrcSpanInfo]
--reshapeDecls fixities decls =
--  let
--    normalDecls = filter (maybe True (const False) . extractFixityDecl) decls
--  in
--    mapM (applyFixities fixities) normalDecls
--
--deInfixModule :: Monad m => Module SrcSpanInfo -> m (Module SrcSpanInfo, [Fixity])
--deInfixModule (Module l header pragmas imports decls) = do
--  let fixities = takeFixities decls
--  doneDecls <- reshapeDecls fixities decls
--  return (Module l header pragmas imports doneDecls, fixities)
--
--deInfixBinds :: Monad m => Binds SrcSpanInfo -> m (Binds SrcSpanInfo)
--deInfixBinds (BDecls l decls) = do
--  let fixities = takeFixities decls
--  doneDecls <- reshapeDecls fixities decls
--  return (BDecls l doneDecls)
--deInfixBinds o = pure o
--
--deInfixPatternSynDirection :: Monad m => PatternSynDirection SrcSpanInfo -> m (PatternSynDirection SrcSpanInfo)
--deInfixPatternSynDirection (ExplicitBidirectional l decls) = do
--  let fixities = takeFixities decls
--  doneDecls <- reshapeDecls fixities decls
--  return (ExplicitBidirectional l doneDecls)
--deInfixPatternSynDirection o = pure o
