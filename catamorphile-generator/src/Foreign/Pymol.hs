module Foreign.Pymol (interfaceResult) where

import Foreign.Hoppy.Generator.Spec ( Interface, interface, interfaceAddHaskellModuleBase, moduleModify'
                                    , moduleSetHppPath, moduleSetCppPath)
import Foreign.Hoppy.Generator.Std (mod_std)

import Foreign.Catamorphile (mod_catamorphile)
import Foreign.Pymol.Layer3.AtomIterators (mod_AtomIterators)

interfaceResult :: Either String Interface
interfaceResult = do
  iface <- interface "pymol"
           [ mod_AtomIterators
           , mod_catamorphile
           , moduleModify' mod_std $ do
               moduleSetHppPath "gen_std.hpp"
               moduleSetCppPath "gen_std.cpp"
           ]
  interfaceAddHaskellModuleBase ["Foreign", "Pymol"] iface
