import           Disorder.Core.Main

import qualified Test.Anemone.Foreign.Atoi
import qualified Test.Anemone.Foreign.Memcmp
import qualified Test.Anemone.Foreign.Mempool
import qualified Test.Anemone.Foreign.Pack
import qualified Test.Anemone.Foreign.Strtod
import qualified Test.Anemone.Foreign.VInt

main :: IO ()
main =
  disorderMain
    [ Test.Anemone.Foreign.Atoi.tests
    , Test.Anemone.Foreign.Memcmp.tests
    , Test.Anemone.Foreign.Mempool.tests
    , Test.Anemone.Foreign.Pack.tests
    , Test.Anemone.Foreign.Strtod.tests
    , Test.Anemone.Foreign.VInt.tests
    ]
