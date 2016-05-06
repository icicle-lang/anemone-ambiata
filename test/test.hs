import           Disorder.Core.Main

import qualified Test.Anemone.Foreign.Memcmp

main :: IO ()
main =
  disorderMain
    [ Test.Anemone.Foreign.Memcmp.tests
    ]
