import           Disorder.Core.Main

import qualified Test.Anemone.Foreign.Memcmp
import qualified Test.Anemone.Foreign.Atoi
import qualified Test.Anemone.Foreign.Pack

main :: IO ()
main =
  disorderMain
    [ Test.Anemone.Foreign.Memcmp.tests
    , Test.Anemone.Foreign.Atoi.tests
    , Test.Anemone.Foreign.Pack.tests
    ]
