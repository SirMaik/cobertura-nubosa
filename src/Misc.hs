module Misc
    ( mapTuple,
      whenEither,
      printErr
    ) where


import Data.Char
import Data.Either
import Control.Monad (join)
import Control.Arrow ((***))
import System.IO


mapTuple :: (a -> b) -> (a,a) -> (b,b)
mapTuple = join (***)


whenEither :: Bool -> a -> b -> Either b a
whenEither b x y = if b then Right x else Left y


printErr :: String -> IO ()
printErr err = hPutStrLn stderr $ "Error: " ++ err
