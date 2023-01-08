{-# LANGUAGE FlexibleContexts, FlexibleInstances, ForeignFunctionInterface, MonoLocalBinds, MultiParamTypeClasses, ScopedTypeVariables, TypeSynonymInstances, UndecidableInstances #-}
---------- GENERATED FILE, EDITS WILL BE LOST ----------

module Foreign.Pymol.AtomIterators (
  castSeleAtomIteratorToAbstractAtomIterator,
  castAbstractAtomIteratorToSeleAtomIterator,
  AbstractAtomIteratorValue (..),
  AbstractAtomIteratorConstPtr (..),
  AbstractAtomIteratorPtr (..),
  getAtm,
  next,
  AbstractAtomIteratorConst (..),
  castAbstractAtomIteratorToConst,
  AbstractAtomIterator (..),
  castAbstractAtomIteratorToNonconst,
  AbstractAtomIteratorSuper (..),
  AbstractAtomIteratorSuperConst (..),
  SeleAtomIteratorValue (..),
  SeleAtomIteratorConstPtr (..),
  SeleAtomIteratorPtr (..),
  SeleAtomIteratorConst (..),
  castSeleAtomIteratorToConst,
  SeleAtomIterator (..),
  castSeleAtomIteratorToNonconst,
  SeleAtomIteratorSuper (..),
  SeleAtomIteratorSuperConst (..),
  ) where

import qualified Foreign as HoppyF
import qualified Foreign.C as HoppyFC
import qualified Foreign.Hoppy.Runtime as HoppyFHR
import Prelude (($), (.), (=<<), (==))
import qualified Prelude as HoppyP

foreign import ccall "genpop__AbstractAtomIterator_getAtm" getAtm' ::  HoppyF.Ptr AbstractAtomIterator -> HoppyP.IO HoppyFC.CInt
foreign import ccall "genpop__AbstractAtomIterator_next" next' ::  HoppyF.Ptr AbstractAtomIterator -> HoppyP.IO ()
foreign import ccall "gendel__AbstractAtomIterator" delete'AbstractAtomIterator :: HoppyF.Ptr AbstractAtomIteratorConst -> HoppyP.IO ()
foreign import ccall "&gendel__AbstractAtomIterator" deletePtr'AbstractAtomIterator :: HoppyF.FunPtr (HoppyF.Ptr AbstractAtomIteratorConst -> HoppyP.IO ())
foreign import ccall "gencast__SeleAtomIterator__AbstractAtomIterator" castSeleAtomIteratorToAbstractAtomIterator :: HoppyF.Ptr SeleAtomIteratorConst -> HoppyF.Ptr AbstractAtomIteratorConst
foreign import ccall "gencast__AbstractAtomIterator__SeleAtomIterator" castAbstractAtomIteratorToSeleAtomIterator :: HoppyF.Ptr AbstractAtomIteratorConst -> HoppyF.Ptr SeleAtomIteratorConst
foreign import ccall "gendel__SeleAtomIterator" delete'SeleAtomIterator :: HoppyF.Ptr SeleAtomIteratorConst -> HoppyP.IO ()
foreign import ccall "&gendel__SeleAtomIterator" deletePtr'SeleAtomIterator :: HoppyF.FunPtr (HoppyF.Ptr SeleAtomIteratorConst -> HoppyP.IO ())

class AbstractAtomIteratorValue a where
  withAbstractAtomIteratorPtr :: a -> (AbstractAtomIteratorConst -> HoppyP.IO b) -> HoppyP.IO b

instance {-# OVERLAPPABLE #-} AbstractAtomIteratorConstPtr a => AbstractAtomIteratorValue a where
  withAbstractAtomIteratorPtr = HoppyP.flip ($) . toAbstractAtomIteratorConst

class (HoppyFHR.CppPtr this) => AbstractAtomIteratorConstPtr this where
  toAbstractAtomIteratorConst :: this -> AbstractAtomIteratorConst

class (AbstractAtomIteratorConstPtr this) => AbstractAtomIteratorPtr this where
  toAbstractAtomIterator :: this -> AbstractAtomIterator

getAtm :: (AbstractAtomIteratorPtr this) => (this) {- ^ this -} -> (HoppyP.IO HoppyP.Int)
getAtm arg'1 =
  HoppyFHR.withCppPtr (toAbstractAtomIterator arg'1) $ \arg'1' ->
  (
    HoppyP.return . HoppyFHR.coerceIntegral
  ) =<<
  (getAtm' arg'1')

next :: (AbstractAtomIteratorPtr this) => (this) {- ^ this -} -> (HoppyP.IO ())
next arg'1 =
  HoppyFHR.withCppPtr (toAbstractAtomIterator arg'1) $ \arg'1' ->
  (next' arg'1')

data AbstractAtomIteratorConst =
    AbstractAtomIteratorConst (HoppyF.Ptr AbstractAtomIteratorConst)
  | AbstractAtomIteratorConstGc (HoppyF.ForeignPtr ()) (HoppyF.Ptr AbstractAtomIteratorConst)
  deriving (HoppyP.Show)

instance HoppyP.Eq AbstractAtomIteratorConst where
  x == y = HoppyFHR.toPtr x == HoppyFHR.toPtr y

instance HoppyP.Ord AbstractAtomIteratorConst where
  compare x y = HoppyP.compare (HoppyFHR.toPtr x) (HoppyFHR.toPtr y)

castAbstractAtomIteratorToConst :: AbstractAtomIterator -> AbstractAtomIteratorConst
castAbstractAtomIteratorToConst (AbstractAtomIterator ptr') = AbstractAtomIteratorConst $ HoppyF.castPtr ptr'
castAbstractAtomIteratorToConst (AbstractAtomIteratorGc fptr' ptr') = AbstractAtomIteratorConstGc fptr' $ HoppyF.castPtr ptr'

instance HoppyFHR.CppPtr AbstractAtomIteratorConst where
  nullptr = AbstractAtomIteratorConst HoppyF.nullPtr
  
  withCppPtr (AbstractAtomIteratorConst ptr') f' = f' ptr'
  withCppPtr (AbstractAtomIteratorConstGc fptr' ptr') f' = HoppyF.withForeignPtr fptr' $ \_ -> f' ptr'
  
  toPtr (AbstractAtomIteratorConst ptr') = ptr'
  toPtr (AbstractAtomIteratorConstGc _ ptr') = ptr'
  
  touchCppPtr (AbstractAtomIteratorConst _) = HoppyP.return ()
  touchCppPtr (AbstractAtomIteratorConstGc fptr' _) = HoppyF.touchForeignPtr fptr'

instance HoppyFHR.Deletable AbstractAtomIteratorConst where
  delete (AbstractAtomIteratorConst ptr') = delete'AbstractAtomIterator ptr'
  delete (AbstractAtomIteratorConstGc _ _) = HoppyP.fail $ HoppyP.concat ["Deletable.delete: Asked to delete a GC-managed ", "AbstractAtomIteratorConst", " object."]
  
  toGc this'@(AbstractAtomIteratorConst ptr') = if ptr' == HoppyF.nullPtr then HoppyP.return this' else HoppyP.fmap (HoppyP.flip AbstractAtomIteratorConstGc ptr') $ HoppyF.newForeignPtr (HoppyF.castFunPtr deletePtr'AbstractAtomIterator :: HoppyF.FunPtr (HoppyF.Ptr () -> HoppyP.IO ())) (HoppyF.castPtr ptr' :: HoppyF.Ptr ())
  toGc this'@(AbstractAtomIteratorConstGc {}) = HoppyP.return this'

instance AbstractAtomIteratorConstPtr AbstractAtomIteratorConst where
  toAbstractAtomIteratorConst = HoppyP.id

data AbstractAtomIterator =
    AbstractAtomIterator (HoppyF.Ptr AbstractAtomIterator)
  | AbstractAtomIteratorGc (HoppyF.ForeignPtr ()) (HoppyF.Ptr AbstractAtomIterator)
  deriving (HoppyP.Show)

instance HoppyP.Eq AbstractAtomIterator where
  x == y = HoppyFHR.toPtr x == HoppyFHR.toPtr y

instance HoppyP.Ord AbstractAtomIterator where
  compare x y = HoppyP.compare (HoppyFHR.toPtr x) (HoppyFHR.toPtr y)

castAbstractAtomIteratorToNonconst :: AbstractAtomIteratorConst -> AbstractAtomIterator
castAbstractAtomIteratorToNonconst (AbstractAtomIteratorConst ptr') = AbstractAtomIterator $ HoppyF.castPtr ptr'
castAbstractAtomIteratorToNonconst (AbstractAtomIteratorConstGc fptr' ptr') = AbstractAtomIteratorGc fptr' $ HoppyF.castPtr ptr'

instance HoppyFHR.CppPtr AbstractAtomIterator where
  nullptr = AbstractAtomIterator HoppyF.nullPtr
  
  withCppPtr (AbstractAtomIterator ptr') f' = f' ptr'
  withCppPtr (AbstractAtomIteratorGc fptr' ptr') f' = HoppyF.withForeignPtr fptr' $ \_ -> f' ptr'
  
  toPtr (AbstractAtomIterator ptr') = ptr'
  toPtr (AbstractAtomIteratorGc _ ptr') = ptr'
  
  touchCppPtr (AbstractAtomIterator _) = HoppyP.return ()
  touchCppPtr (AbstractAtomIteratorGc fptr' _) = HoppyF.touchForeignPtr fptr'

instance HoppyFHR.Deletable AbstractAtomIterator where
  delete (AbstractAtomIterator ptr') = delete'AbstractAtomIterator $ (HoppyF.castPtr ptr' :: HoppyF.Ptr AbstractAtomIteratorConst)
  delete (AbstractAtomIteratorGc _ _) = HoppyP.fail $ HoppyP.concat ["Deletable.delete: Asked to delete a GC-managed ", "AbstractAtomIterator", " object."]
  
  toGc this'@(AbstractAtomIterator ptr') = if ptr' == HoppyF.nullPtr then HoppyP.return this' else HoppyP.fmap (HoppyP.flip AbstractAtomIteratorGc ptr') $ HoppyF.newForeignPtr (HoppyF.castFunPtr deletePtr'AbstractAtomIterator :: HoppyF.FunPtr (HoppyF.Ptr () -> HoppyP.IO ())) (HoppyF.castPtr ptr' :: HoppyF.Ptr ())
  toGc this'@(AbstractAtomIteratorGc {}) = HoppyP.return this'

instance AbstractAtomIteratorConstPtr AbstractAtomIterator where
  toAbstractAtomIteratorConst (AbstractAtomIterator ptr') = AbstractAtomIteratorConst $ (HoppyF.castPtr :: HoppyF.Ptr AbstractAtomIterator -> HoppyF.Ptr AbstractAtomIteratorConst) ptr'
  toAbstractAtomIteratorConst (AbstractAtomIteratorGc fptr' ptr') = AbstractAtomIteratorConstGc fptr' $ (HoppyF.castPtr :: HoppyF.Ptr AbstractAtomIterator -> HoppyF.Ptr AbstractAtomIteratorConst) ptr'

instance AbstractAtomIteratorPtr AbstractAtomIterator where
  toAbstractAtomIterator = HoppyP.id

class AbstractAtomIteratorSuper a where
  downToAbstractAtomIterator :: a -> AbstractAtomIterator


class AbstractAtomIteratorSuperConst a where
  downToAbstractAtomIteratorConst :: a -> AbstractAtomIteratorConst


instance HoppyFHR.Assignable (HoppyF.Ptr (HoppyF.Ptr AbstractAtomIterator)) AbstractAtomIterator where
  assign ptr' value' = HoppyF.poke ptr' $ HoppyFHR.toPtr value'

instance HoppyFHR.Decodable (HoppyF.Ptr (HoppyF.Ptr AbstractAtomIterator)) AbstractAtomIterator where
  decode = HoppyP.fmap AbstractAtomIterator . HoppyF.peek

class SeleAtomIteratorValue a where
  withSeleAtomIteratorPtr :: a -> (SeleAtomIteratorConst -> HoppyP.IO b) -> HoppyP.IO b

instance {-# OVERLAPPABLE #-} SeleAtomIteratorConstPtr a => SeleAtomIteratorValue a where
  withSeleAtomIteratorPtr = HoppyP.flip ($) . toSeleAtomIteratorConst

class (AbstractAtomIteratorConstPtr this) => SeleAtomIteratorConstPtr this where
  toSeleAtomIteratorConst :: this -> SeleAtomIteratorConst

class (SeleAtomIteratorConstPtr this, AbstractAtomIteratorPtr this) => SeleAtomIteratorPtr this where
  toSeleAtomIterator :: this -> SeleAtomIterator

data SeleAtomIteratorConst =
    SeleAtomIteratorConst (HoppyF.Ptr SeleAtomIteratorConst)
  | SeleAtomIteratorConstGc (HoppyF.ForeignPtr ()) (HoppyF.Ptr SeleAtomIteratorConst)
  deriving (HoppyP.Show)

instance HoppyP.Eq SeleAtomIteratorConst where
  x == y = HoppyFHR.toPtr x == HoppyFHR.toPtr y

instance HoppyP.Ord SeleAtomIteratorConst where
  compare x y = HoppyP.compare (HoppyFHR.toPtr x) (HoppyFHR.toPtr y)

castSeleAtomIteratorToConst :: SeleAtomIterator -> SeleAtomIteratorConst
castSeleAtomIteratorToConst (SeleAtomIterator ptr') = SeleAtomIteratorConst $ HoppyF.castPtr ptr'
castSeleAtomIteratorToConst (SeleAtomIteratorGc fptr' ptr') = SeleAtomIteratorConstGc fptr' $ HoppyF.castPtr ptr'

instance HoppyFHR.CppPtr SeleAtomIteratorConst where
  nullptr = SeleAtomIteratorConst HoppyF.nullPtr
  
  withCppPtr (SeleAtomIteratorConst ptr') f' = f' ptr'
  withCppPtr (SeleAtomIteratorConstGc fptr' ptr') f' = HoppyF.withForeignPtr fptr' $ \_ -> f' ptr'
  
  toPtr (SeleAtomIteratorConst ptr') = ptr'
  toPtr (SeleAtomIteratorConstGc _ ptr') = ptr'
  
  touchCppPtr (SeleAtomIteratorConst _) = HoppyP.return ()
  touchCppPtr (SeleAtomIteratorConstGc fptr' _) = HoppyF.touchForeignPtr fptr'

instance HoppyFHR.Deletable SeleAtomIteratorConst where
  delete (SeleAtomIteratorConst ptr') = delete'SeleAtomIterator ptr'
  delete (SeleAtomIteratorConstGc _ _) = HoppyP.fail $ HoppyP.concat ["Deletable.delete: Asked to delete a GC-managed ", "SeleAtomIteratorConst", " object."]
  
  toGc this'@(SeleAtomIteratorConst ptr') = if ptr' == HoppyF.nullPtr then HoppyP.return this' else HoppyP.fmap (HoppyP.flip SeleAtomIteratorConstGc ptr') $ HoppyF.newForeignPtr (HoppyF.castFunPtr deletePtr'SeleAtomIterator :: HoppyF.FunPtr (HoppyF.Ptr () -> HoppyP.IO ())) (HoppyF.castPtr ptr' :: HoppyF.Ptr ())
  toGc this'@(SeleAtomIteratorConstGc {}) = HoppyP.return this'

instance SeleAtomIteratorConstPtr SeleAtomIteratorConst where
  toSeleAtomIteratorConst = HoppyP.id

instance AbstractAtomIteratorConstPtr SeleAtomIteratorConst where
  toAbstractAtomIteratorConst (SeleAtomIteratorConst ptr') = AbstractAtomIteratorConst $ castSeleAtomIteratorToAbstractAtomIterator ptr'
  toAbstractAtomIteratorConst (SeleAtomIteratorConstGc fptr' ptr') = AbstractAtomIteratorConstGc fptr' $ castSeleAtomIteratorToAbstractAtomIterator ptr'

data SeleAtomIterator =
    SeleAtomIterator (HoppyF.Ptr SeleAtomIterator)
  | SeleAtomIteratorGc (HoppyF.ForeignPtr ()) (HoppyF.Ptr SeleAtomIterator)
  deriving (HoppyP.Show)

instance HoppyP.Eq SeleAtomIterator where
  x == y = HoppyFHR.toPtr x == HoppyFHR.toPtr y

instance HoppyP.Ord SeleAtomIterator where
  compare x y = HoppyP.compare (HoppyFHR.toPtr x) (HoppyFHR.toPtr y)

castSeleAtomIteratorToNonconst :: SeleAtomIteratorConst -> SeleAtomIterator
castSeleAtomIteratorToNonconst (SeleAtomIteratorConst ptr') = SeleAtomIterator $ HoppyF.castPtr ptr'
castSeleAtomIteratorToNonconst (SeleAtomIteratorConstGc fptr' ptr') = SeleAtomIteratorGc fptr' $ HoppyF.castPtr ptr'

instance HoppyFHR.CppPtr SeleAtomIterator where
  nullptr = SeleAtomIterator HoppyF.nullPtr
  
  withCppPtr (SeleAtomIterator ptr') f' = f' ptr'
  withCppPtr (SeleAtomIteratorGc fptr' ptr') f' = HoppyF.withForeignPtr fptr' $ \_ -> f' ptr'
  
  toPtr (SeleAtomIterator ptr') = ptr'
  toPtr (SeleAtomIteratorGc _ ptr') = ptr'
  
  touchCppPtr (SeleAtomIterator _) = HoppyP.return ()
  touchCppPtr (SeleAtomIteratorGc fptr' _) = HoppyF.touchForeignPtr fptr'

instance HoppyFHR.Deletable SeleAtomIterator where
  delete (SeleAtomIterator ptr') = delete'SeleAtomIterator $ (HoppyF.castPtr ptr' :: HoppyF.Ptr SeleAtomIteratorConst)
  delete (SeleAtomIteratorGc _ _) = HoppyP.fail $ HoppyP.concat ["Deletable.delete: Asked to delete a GC-managed ", "SeleAtomIterator", " object."]
  
  toGc this'@(SeleAtomIterator ptr') = if ptr' == HoppyF.nullPtr then HoppyP.return this' else HoppyP.fmap (HoppyP.flip SeleAtomIteratorGc ptr') $ HoppyF.newForeignPtr (HoppyF.castFunPtr deletePtr'SeleAtomIterator :: HoppyF.FunPtr (HoppyF.Ptr () -> HoppyP.IO ())) (HoppyF.castPtr ptr' :: HoppyF.Ptr ())
  toGc this'@(SeleAtomIteratorGc {}) = HoppyP.return this'

instance SeleAtomIteratorConstPtr SeleAtomIterator where
  toSeleAtomIteratorConst (SeleAtomIterator ptr') = SeleAtomIteratorConst $ (HoppyF.castPtr :: HoppyF.Ptr SeleAtomIterator -> HoppyF.Ptr SeleAtomIteratorConst) ptr'
  toSeleAtomIteratorConst (SeleAtomIteratorGc fptr' ptr') = SeleAtomIteratorConstGc fptr' $ (HoppyF.castPtr :: HoppyF.Ptr SeleAtomIterator -> HoppyF.Ptr SeleAtomIteratorConst) ptr'

instance SeleAtomIteratorPtr SeleAtomIterator where
  toSeleAtomIterator = HoppyP.id

instance AbstractAtomIteratorConstPtr SeleAtomIterator where
  toAbstractAtomIteratorConst (SeleAtomIterator ptr') = AbstractAtomIteratorConst $ castSeleAtomIteratorToAbstractAtomIterator $ (HoppyF.castPtr :: HoppyF.Ptr SeleAtomIterator -> HoppyF.Ptr SeleAtomIteratorConst) ptr'
  toAbstractAtomIteratorConst (SeleAtomIteratorGc fptr' ptr') = AbstractAtomIteratorConstGc fptr' $ castSeleAtomIteratorToAbstractAtomIterator $ (HoppyF.castPtr :: HoppyF.Ptr SeleAtomIterator -> HoppyF.Ptr SeleAtomIteratorConst) ptr'

instance AbstractAtomIteratorPtr SeleAtomIterator where
  toAbstractAtomIterator (SeleAtomIterator ptr') = AbstractAtomIterator $ (HoppyF.castPtr :: HoppyF.Ptr AbstractAtomIteratorConst -> HoppyF.Ptr AbstractAtomIterator) $ castSeleAtomIteratorToAbstractAtomIterator $ (HoppyF.castPtr :: HoppyF.Ptr SeleAtomIterator -> HoppyF.Ptr SeleAtomIteratorConst) ptr'
  toAbstractAtomIterator (SeleAtomIteratorGc fptr' ptr') = AbstractAtomIteratorGc fptr' $ (HoppyF.castPtr :: HoppyF.Ptr AbstractAtomIteratorConst -> HoppyF.Ptr AbstractAtomIterator) $ castSeleAtomIteratorToAbstractAtomIterator $ (HoppyF.castPtr :: HoppyF.Ptr SeleAtomIterator -> HoppyF.Ptr SeleAtomIteratorConst) ptr'

class SeleAtomIteratorSuper a where
  downToSeleAtomIterator :: a -> SeleAtomIterator

instance SeleAtomIteratorSuper AbstractAtomIterator where
  downToSeleAtomIterator = castSeleAtomIteratorToNonconst . cast' . castAbstractAtomIteratorToConst
    where
      cast' (AbstractAtomIteratorConst ptr') = SeleAtomIteratorConst $ castAbstractAtomIteratorToSeleAtomIterator ptr'
      cast' (AbstractAtomIteratorConstGc fptr' ptr') = SeleAtomIteratorConstGc fptr' $ castAbstractAtomIteratorToSeleAtomIterator ptr'

class SeleAtomIteratorSuperConst a where
  downToSeleAtomIteratorConst :: a -> SeleAtomIteratorConst

instance SeleAtomIteratorSuperConst AbstractAtomIteratorConst where
  downToSeleAtomIteratorConst = cast'
    where
      cast' (AbstractAtomIteratorConst ptr') = SeleAtomIteratorConst $ castAbstractAtomIteratorToSeleAtomIterator ptr'
      cast' (AbstractAtomIteratorConstGc fptr' ptr') = SeleAtomIteratorConstGc fptr' $ castAbstractAtomIteratorToSeleAtomIterator ptr'

instance HoppyFHR.Assignable (HoppyF.Ptr (HoppyF.Ptr SeleAtomIterator)) SeleAtomIterator where
  assign ptr' value' = HoppyF.poke ptr' $ HoppyFHR.toPtr value'

instance HoppyFHR.Decodable (HoppyF.Ptr (HoppyF.Ptr SeleAtomIterator)) SeleAtomIterator where
  decode = HoppyP.fmap SeleAtomIterator . HoppyF.peek