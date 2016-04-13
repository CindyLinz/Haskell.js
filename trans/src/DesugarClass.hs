module DesugarClass where

class Monad m => Desugarable m a where
  desugar :: a -> m a

data Desugar a = Desugar {unDesugar :: DesugarState -> (DesugarState, a)}
data DesugarState = DesugarState
  { dsgstSymbol :: ()
  }
initDesugarState :: DesugarState
initDesugarState = DesugarState ()

instance Functor Desugar where
  fmap f (Desugar k) = Desugar $ \s ->
    let
      (s', a) = k s
    in
      (s', f a)

instance Applicative Desugar where
  pure a = Desugar $ \s -> (s, a)
  Desugar fGen <*> Desugar aGen = Desugar $ \s ->
    let
      (s', f) = fGen s
      (s'', a) = aGen s'
    in
      (s'', f a)

instance Monad Desugar where
  Desugar m >>= fGen = Desugar $ \s ->
    let
      (s', a) = m s
      Desugar f = fGen a
    in
      f s'
