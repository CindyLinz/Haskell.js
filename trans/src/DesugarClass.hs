module DesugarClass where

import SymbolTable

class Monad m => Desugarable m a where
  desugar :: a -> m a

data Desugar a = Desugar {unDesugar :: DesugarState -> (DesugarState, a)}
data DesugarState = DesugarState
  { dsgstValueSymbol :: ()
  , dsgstTypeSymbol :: ()
  }
initDesugarState :: DesugarState
initDesugarState = DesugarState () ()

