module Foreign.Pymol.Support (component) where

import Foreign.Hoppy.Generator.Spec ( Purity (Nonpure), Function, Module, Type, addReqIncludes, ident, includeLocal
                                    , makeFn, makeModule, moduleAddExports, moduleModify'
                                    , toExport
                                    )
import Foreign.Hoppy.Generator.Types ( ptrT, objT )

import Foreign.Pymol.Layer3.AtomIterators (classSeleAtomIterator)

component :: Module
component =
  moduleModify' (makeModule "Support" "gen/gen_PymolSupport.hpp" "gen/gen_PymolSupport.cpp")
  $ moduleAddExports [toExport newSelectionIterator]

newSelectionIterator :: Function
newSelectionIterator =
  addReqIncludes [includeLocal "Support.hpp"]
  $ makeFn (ident "newSelectionIterator") Nothing Nonpure ([] :: [Type]) (ptrT $ objT classSeleAtomIterator)
