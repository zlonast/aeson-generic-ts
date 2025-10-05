import qualified FpTsSpec

import           GHC.Tuple   (Unit)

import           System.IO   (IO)

import           Test.Hspec  (Spec, describe, hspec)

import qualified VanillaSpec

main :: IO Unit
main = hspec spec

spec :: Spec
spec = do
  describe "FpTsSpec"    FpTsSpec.spec
  describe "VanillaSpec" VanillaSpec.spec
