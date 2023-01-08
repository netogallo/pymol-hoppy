module Foreign.Pymol.Layer3.AtomIterators(class_AbstractAtomIterator, class_SeleAtomIterator, mod_AtomIterators) where

import Foreign.Hoppy.Generator.Spec ( Class, Module, addReqIncludes, classSetEntityPrefix, ident, includeLocal, makeClass
                                    , makeModule, mkMethod', moduleAddExports, moduleModify', np, toExport)
import Foreign.Hoppy.Generator.Types (intT, voidT)

mod_AtomIterators :: Module
mod_AtomIterators =
  moduleModify' (makeModule "AtomIterators" "gen_AtomIterators.hpp" "gen_AtomIterators.cpp") $
  moduleAddExports
  [ toExport class_AbstractAtomIterator
  , toExport class_SeleAtomIterator
  ]

class_AbstractAtomIterator :: Class
class_AbstractAtomIterator =
  classSetEntityPrefix "" $
  addReqIncludes [includeLocal "AtomIterators.h"] $
  makeClass (ident "AbstractAtomIterator") Nothing [] methods

  where
    getAtm = mkMethod' "getAtm" "getAtm" np intT
    next = mkMethod' "next" "next" np voidT
    methods = [getAtm, next]

class_SeleAtomIterator :: Class
class_SeleAtomIterator =
  classSetEntityPrefix "" $
  addReqIncludes [includeLocal "AtomIterators.h"] $
  makeClass (ident "SeleAtomIterator") Nothing [class_AbstractAtomIterator] []
