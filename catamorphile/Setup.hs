module Main where


import Foreign.Hoppy.Setup (ProjectConfig (..), hsMain)
import qualified Foreign.Pymol as Pymol

main =
  hsMain
  ProjectConfig
  { interfaceResult = Pymol.interfaceResult
  , cppPackageName = "catamorphile-cpp"
  , cppSourcesDir = "cpp"
  , hsSourcesDir = "src"
  }
