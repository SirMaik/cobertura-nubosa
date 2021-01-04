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
--HFlags has a bug where last defineFlag is not processed, that's why the next line is necessary:
defineFlag "empty"      ""     "" 


defaultMask = "data/masks/default_mask.png"

main :: IO ()
main = do args <- $initHFlags "cobertura-nubosa v0.1.0.0"
          if | flags_source == ""                       -> printErr "Source image must be provided after -s flag."     >> exitFailure
             | flags_seg    == False && flags_out /= "" -> printErr "Invalid option -o without segmentation set off."  >> exitFailure
             | flags_seg    == True  && flags_out == "" -> printErr "Invalid option -S without output name specified." >> exitFailure
             | otherwise                                -> return ()
          maskPath <- if flags_mask == "" then getDataFileName defaultMask else return flags_mask
          img      <- readImage flags_source
          mask     <- readImage maskPath          
          either (\err                   -> print err >> exitFailure)
                 (\(img, mask, maskFunc) -> ((\img -> when flags_seg (writePng flags_out img) >> print ("CCI=" ++ show (cci img))) . segImage) $ maskFunc img mask)
                 $ liftA2 (,) img mask
                 >>= return . (mapTuple convertRGBA8)
                 >>= (\(img, mask) -> case compareSizes img mask of
                                       (Right EQ) -> return applyMask
                                       (Right GT) -> return applySmallerMask
                                       _          -> Left "Incompatible Sizes" 
                                     >>= \maskFunc -> return (img, mask, maskFunc))
