{-# LANGUAGE FlexibleContexts, FlexibleInstances, ForeignFunctionInterface, MonoLocalBinds, MultiParamTypeClasses, ScopedTypeVariables, TypeSynonymInstances, UndecidableInstances #-}
---------- GENERATED FILE, EDITS WILL BE LOST ----------

module Foreign.Pymol.Catamorphile (
  newSelectionIterator,
  ) where

import qualified Foreign as HoppyF
import qualified Foreign.Pymol.AtomIterators as M1
import qualified Prelude as HoppyP

foreign import ccall "genpop__newSelectionIterator" newSelectionIterator' ::  HoppyP.IO (HoppyF.Ptr M1.SeleAtomIterator)

newSelectionIterator :: (HoppyP.IO M1.SeleAtomIterator)
newSelectionIterator =
  HoppyP.fmap M1.SeleAtomIterator
  (newSelectionIterator')