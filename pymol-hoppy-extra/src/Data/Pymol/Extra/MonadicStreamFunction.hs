module Data.Pymol.Extra.MonadicStreamFunction
  (
  ) where

import           Control.Monad.IO.Class (MonadIO(liftIO))
import           Data.MonadicStreamFunction (MSF, constM)

import           Data.Pymol.Layer3.AtomIterators (AbstractAtomIteratorPtr, getAtm, next)

atomIteratorMSF ::
  (MonadIO m, AbstractAtomIteratorPtr ptr) =>
  ptr ->
  MSF m a (Maybe Int)
atomIteratorMSF it = constM . liftIO $ do
  continue <- next it
  if continue
    then Just <$> getAtm it
    else pure Nothing
