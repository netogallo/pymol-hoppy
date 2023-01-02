module Foreign.Pymol.Layer3.AtomIterators(class_SeleAtomIterator, mod_AtomIterators) where

import Foreign.Hoppy.Generator.Spec ( Class, Module, addReqIncludes, ident, includeLocal, makeClass, makeModule
                                    , moduleAddExports, moduleModify', toExport)

mod_AtomIterators :: Module
mod_AtomIterators =
  moduleModify' (makeModule "AtomIterators" "gen_AtomIterators.hpp" "gen_AtomIterators.cpp") $
  moduleAddExports [toExport class_SeleAtomIterator]

class_SeleAtomIterator :: Class
class_SeleAtomIterator =
  addReqIncludes [includeLocal "AtomIterators.h"] $
  makeClass (ident "SeleAtomIterator") Nothing [] []
