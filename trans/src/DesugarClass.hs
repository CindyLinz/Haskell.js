module DesugarClass where

class Monad m => Desugar m a where
  desugar :: a -> m a
