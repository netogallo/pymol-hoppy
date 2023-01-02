module Foreign.Catamorphile (mod_catamorphile) where

import Foreign.Hoppy.Generator.Spec ( Purity (Nonpure), Function, Module, Type, addReqIncludes, ident, includeLocal
                                    , makeFn, makeModule, moduleAddExports, moduleModify'
                                    , toExport)
import Foreign.Hoppy.Generator.Types (ptrT, objT)

import Foreign.Pymol.Layer3.AtomIterators (class_SeleAtomIterator)

mod_catamorphile :: Module
mod_catamorphile =
  moduleModify' (makeModule "Catamorphile" "gen_Catamorphile.hpp" "gen_Catamorphile.cpp")
  $ moduleAddExports [toExport f_newExporterHelper]

f_newExporterHelper :: Function
f_newExporterHelper =
  addReqIncludes [includeLocal "Catamorphile.hpp"]
  $ makeFn (ident "newSelectionIterator") Nothing Nonpure ([] :: [Type]) (ptrT $ objT class_SeleAtomIterator)
