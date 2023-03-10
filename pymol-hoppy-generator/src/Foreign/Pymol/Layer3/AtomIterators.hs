module Foreign.Pymol.Layer3.AtomIterators
  ( classAbstractAtomIterator
  , classDummyAtomIterator
  , classSeleAtomIterator
  , component
  ) where


import           Foreign.Hoppy.Generator.Spec ( Class, Module(..), addReqIncludes, classSetEntityPrefix, ident, includeLocal, makeClass
                                              , mkCtor, makeModule, mkMethod', moduleAddExports, moduleModify', np, toExport)
import           Foreign.Hoppy.Generator.Types (boolT, intT, objT, ptrT, voidT)

import           Foreign.Pymol.Layer2.AtomInfo (classAtomInfo)

component :: Module
component =
  moduleModify' moduleInit $
  moduleAddExports
  [ toExport classAbstractAtomIterator
  , toExport classSeleAtomIterator
  , toExport classDummyAtomIterator
  ]

  where
    moduleBaseName = "AtomIterators"
    moduleInit =
      (makeModule moduleBaseName "gen/layer3/gen_AtomIterators.hpp" "gen/layer3/gen_AtomIterators.cpp")
      { moduleHaskellName = Just ["Layer3", moduleBaseName] }

classAbstractAtomIterator :: Class
classAbstractAtomIterator =
  classSetEntityPrefix "" $
  addReqIncludes [includeLocal "AtomIterators.h"] $
  makeClass (ident "AbstractAtomIterator") Nothing [] methods

  where
    methods =
      [ mkMethod' "getAtm" "getAtm" np intT
      , mkMethod' "next" "next" np boolT
      , mkMethod' "getAtomInfo" "getAtomInfo" np . ptrT $ objT classAtomInfo
      ]

classSeleAtomIterator :: Class
classSeleAtomIterator =
  classSetEntityPrefix "" $
  addReqIncludes [includeLocal "AtomIterators.h"] $
  makeClass (ident "SeleAtomIterator") Nothing [classAbstractAtomIterator] []

classDummyAtomIterator :: Class
classDummyAtomIterator =
  classSetEntityPrefix "" $
  addReqIncludes [includeLocal "dummy/DummyAtomIterator.h"] $
  makeClass (ident "DummyAtomIterator") Nothing [classAbstractAtomIterator] methods

  where
    methods =
      [ mkCtor "create" np ]
