{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}

module Main where

import Cobertura
import Misc
import Codec.Picture
import Control.Monad
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit
import System.IO
import Data.Maybe
import Control.Applicative
import HFlags
import Paths_cobertura_nubosa


defineFlag "S:seg"      False  "Whether segmentation image is created or not. Necessarily used with -o."
defineFlag "s:source"   ""     "Source image. Must be provided." 
defineFlag "o:out"      ""     "Output name of segmentation image. Necessary when -S."
defineFlag "m:mask"     ""     "Source of mask. If not provided then default_mask.png will be used."
--HFlags tiene un bug que causa que la última bandera definida no es procesada, por eso es necesaria la siguiente línea:
defineFlag "empty"      ""     "" 

--Ruta hacia la máscara predeterminada. 
defaultMask = "data/masks/default_mask.png"

main :: IO ()
main = do args <- $initHFlags "cobertura-nubosa v0.1.0.0"
          --Se verifican que las combinaciones de banderas sean válidas
          if | flags_source == ""                       -> printErr "Source image must be provided after -s flag."     >> exitFailure
             | flags_seg    == False && flags_out /= "" -> printErr "Invalid option -o without segmentation set off."  >> exitFailure
             | flags_seg    == True  && flags_out == "" -> printErr "Invalid option -S without output name specified." >> exitFailure
             | otherwise                                -> return ()
          --Se selecciona la ruta hacia la máscara dependiendo si el usuario proporcionó una o no. 
          maskPath <- if flags_mask == "" then getDataFileName defaultMask else return flags_mask
          --Se intentan abrir las imágenes
          img      <- readImage flags_source
          mask     <- readImage maskPath          
          either --En el caso de que suceda algún error se imprime:
                 (\err                   -> print err >> exitFailure)
                 --Si no sucedieron errores entonces se procesa la imagen de entrada:
                 (\(img, mask, maskFunc) -> ((\img -> when flags_seg (writePng flags_out img) >> print ("CCI=" ++ show (cci img))) . segImage) $ maskFunc img mask)
                 --Se hace un preprocesamiento que puede generar errores:
                 $ liftA2 (,) img mask
                 >>= return . (mapTuple convertRGBA8)
                 >>= (\(img, mask) -> case compareSizes img mask of
                                       (Right EQ) -> return applyMask
                                       (Right GT) -> return applySmallerMask
                                       _          -> Left "Incompatible Sizes" 
                                     >>= \maskFunc -> return (img, mask, maskFunc))
