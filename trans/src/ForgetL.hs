module ForgetL where

import Data.Functor

forgetL :: Functor f => f a -> f ()
forgetL = fmap (const ())
