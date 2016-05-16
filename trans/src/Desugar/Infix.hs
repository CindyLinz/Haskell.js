module Desugar.Infix where

import Language.Haskell.Exts.Annotated.Fixity (Fixity (Fixity))

-- 先轉換上層的, 再呼叫局部的

deInfixModule :: Monad m => Module SrcSpanInfo -> m (Module SrcSpanInfo, [Fixity])
deInfixModule (Module l header pragmas imports decls) =
  let

deInfixBinds :: Monad m => Binds SrcSpanInfo -> m (Binds SrcSpanInfo)

deInfixPatternSynDirection :: Monad m => PatternSynDirection SrcSpanInfo -> m (PatternSynDirection SrcSpanInfo)

