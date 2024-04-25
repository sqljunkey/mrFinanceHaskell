
--import Test
import Test
import Control.Exception 
import System.IO 
import Control.Monad.Trans.Reader

-- Set up actions to run on start and end, and run the main loop
main :: IO ()
main = bracket connect disconnect loop
  where
    disconnect = hClose . botSocket
    loop st = runReaderT run st