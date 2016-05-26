module DesugarClass where

import SymbolTableData

class Monad m => Desugarable m a where
  desugar :: a -> m a

data Desugar a = Desugar {unDesugar :: DesugarState -> (DesugarState, a)}
data DesugarState = DesugarState
  { dsgstValueSymbolTable :: SymbolTable
  , dsgstPatternSymbolTable :: SymbolTable
  }
initDesugarState :: DesugarState
initDesugarState = DesugarState
  { dsgstValueSymbolTable = emptySymbolTable
  , dsgstPatternSymbolTable = emptySymbolTable
  }

