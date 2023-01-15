{-# LANGUAGE FlexibleContexts, FlexibleInstances, ForeignFunctionInterface, MonoLocalBinds, MultiParamTypeClasses, ScopedTypeVariables, TypeSynonymInstances, UndecidableInstances #-}
---------- GENERATED FILE, EDITS WILL BE LOST ----------

module Data.Pymol.Support (
  newSelectionIterator,
  ) where

import qualified Data.Pymol.Layer3.AtomIterators as M2
import qualified Foreign as HoppyF
import qualified Prelude as HoppyP

foreign import ccall "genpop__newSelectionIterator" newSelectionIterator' ::  HoppyP.IO (HoppyF.Ptr M2.SeleAtomIterator)

newSelectionIterator :: (HoppyP.IO M2.SeleAtomIterator)
newSelectionIterator =
  HoppyP.fmap M2.SeleAtomIterator
  (newSelectionIterator')