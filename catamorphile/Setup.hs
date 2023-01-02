module Main where


import Foreign.Hoppy.Setup (ProjectConfig (..), hsMain)
import qualified Foreign.Hoppy.Example.Generator as Generator

main =
  hsMain
  ProjectConfig
  { interfaceResult = Generator.interfaceResult
  , cppPackageName = "hoppy-example-cpp"
  , cppSourcesDir = "cpp"
  , hsSourcesDir = "src"
  }
