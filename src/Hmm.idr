module Hmm

import Control.Linear.LIO

data UT : Type -> Type where
  MkUT : a -> UT a

Functor UT where
  map = ?dff

Applicative UT where
  (<*>) = ?em
  pure = ?pure

Monad UT where
  (>>=) = ?dwdwf

HasLinearIO UT where
  liftIO1 = ?ddd
