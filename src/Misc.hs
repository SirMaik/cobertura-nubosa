
--Módulo donde se definen funciones complementarias.

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

--Mapea una función a ambas componentes de una tupla. Ambas componentes deben de tener el mismo tipo. 
mapTuple :: (a -> b) -> (a,a) -> (b,b)
mapTuple = join (***)

whenEither :: Bool -> a -> b -> Either b a
whenEither b x y = if b then Right x else Left y

--Recibe una cadena y la imprime en stderr. 
printErr :: String -> IO ()
printErr err = hPutStrLn stderr $ "Error: " ++ err
