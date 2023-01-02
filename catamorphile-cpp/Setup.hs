module Main (main) where

import Foreign.Hoppy.Setup (ProjectConfig (..), cppMain)
import qualified Foreign.Pymol as Pymol

main =
  cppMain
  ProjectConfig
  { interfaceResult = Pymol.interfaceResult
  , cppPackageName = "catamorphile-cpp"
  , cppSourcesDir = "cpp"
  , hsSourcesDir = "src"
  }
