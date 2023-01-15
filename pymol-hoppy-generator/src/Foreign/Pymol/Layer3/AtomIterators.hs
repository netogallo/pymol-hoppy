module Foreign.Pymol.Layer3.AtomIterators
  ( classAbstractAtomIterator
  , classSeleAtomIterator
  , component
  ) where


import Foreign.Hoppy.Generator.Spec ( Class, Module, addReqIncludes, classSetEntityPrefix, ident, includeLocal, makeClass
                                    , makeModule, mkMethod', moduleAddExports, moduleModify', np, toExport)
import Foreign.Hoppy.Generator.Types (intT, voidT)

component :: Module
component =
  moduleModify' (makeModule "Layer3.AtomIterators" "gen/layer3/gen_AtomIterators.hpp" "gen/layer3/gen_AtomIterators.cpp") $
  moduleAddExports
  [ toExport classAbstractAtomIterator
  , toExport classSeleAtomIterator
  ]

classAbstractAtomIterator :: Class
classAbstractAtomIterator =
  classSetEntityPrefix "" $
  addReqIncludes [includeLocal "AtomIterators.h"] $
  makeClass (ident "AbstractAtomIterator") Nothing [] methods

  where
    getAtm = mkMethod' "getAtm" "getAtm" np intT
    next = mkMethod' "next" "next" np voidT
    methods = [getAtm, next]

classSeleAtomIterator :: Class
classSeleAtomIterator =
  classSetEntityPrefix "" $
  addReqIncludes [includeLocal "AtomIterators.h"] $
  makeClass (ident "SeleAtomIterator") Nothing [classAbstractAtomIterator] []
