module Foreign.Pymol.Layer2.AtomInfo
  ( classAtomInfo
  , component
  ) where
import Foreign.Hoppy.Generator.Spec ( Class, Module, classSetEntityPrefix, addReqIncludes, includeLocal, makeClass, ident, mkClassVariable
                                    , makeModule, moduleAddExports, moduleModify', toExport)
import Foreign.Hoppy.Generator.Types (floatT, ptrT)

component :: Module
component =
  moduleModify' (makeModule "Layer2.AtomInfo" "gen/layer2/gen_AtomInfo.hpp" "gen/layer2/gen_AtomInfo.cpp") $
  moduleAddExports
  [ toExport classAtomInfo
  ]

classAtomInfo :: Class
classAtomInfo =
  classSetEntityPrefix ""
  . addReqIncludes [includeLocal "AtomInfo.h"]
  $ makeClass (ident "AtomInfoType") Nothing [] methods

  where
    methods =
      [ mkClassVariable "anisou" $ ptrT floatT
      ]
