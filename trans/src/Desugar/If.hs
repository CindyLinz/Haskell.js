module Desugar.If where
import Language.Haskell.Exts.Annotated.Syntax
import Control.Arrow ((***))

import DesugarClass

deIfExp :: Exp l -> Exp l
deIfExp (If l exp1 exp2 exp3) =
  Case l exp1
    [ Alt l (PApp l (Qual l (ModuleName l "Prelude") (Ident l "False")) []) (UnGuardedRhs l exp3) Nothing
    , Alt l (PApp l (Qual l (ModuleName l "Prelude") (Ident l "True")) []) (UnGuardedRhs l exp2) Nothing
    ]
deIfExp exp = exp
