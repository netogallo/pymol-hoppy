module Foreign.Pymol (interfaceResult) where

import           Foreign.Hoppy.Generator.Spec ( Interface, interface, interfaceAddHaskellModuleBase, moduleModify'
                                    , moduleSetHppPath, moduleSetCppPath)
import           Foreign.Hoppy.Generator.Std (mod_std)

import qualified Foreign.Pymol.Support as Support
import qualified Foreign.Pymol.Layer2.AtomInfo as AtomInfo
import qualified Foreign.Pymol.Layer3.AtomIterators as AtomIterators

interfaceResult :: Either String Interface
interfaceResult = do
  iface <- interface "pymol"
           [ Support.component
           , AtomInfo.component
           , AtomIterators.component
           , moduleModify' mod_std $ do
               moduleSetHppPath "gen/gen_std.hpp"
               moduleSetCppPath "gen/gen_std.cpp"
           ]
  interfaceAddHaskellModuleBase ["Data", "Pymol"] iface
