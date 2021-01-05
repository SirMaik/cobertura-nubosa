{-# LANGUAGE MultiWayIf #-}

--Módulo donde se definen las funciones necesarias para calcular el índice de cobertura nubosa.

module Cobertura
    ( compareSizes,
      pixelMap2,
      circCenteredMask,
      applyMask,
      applySmallerMask,
      segImage,
      cci
    ) where

import Codec.Picture
import Codec.Picture.Types
import Codec.Picture.Metadata as Meta
import Data.Maybe
import Control.Applicative



--Compara las dimensiones de dos imágenes.
--La manera en la que se relacionan las alturas y las anchuras debe de ser la misma.
--Esto último es algo restrictivo y podría definirse de una manera más flexible,
--sin embargo nos ayuda a garantizar que la máscara quepa dentro de la imagen. 
compareSizes :: Image a -> Image b -> Either String Ordering
compareSizes i1 i2 = let w1 = imageWidth  i1
                         h1 = imageHeight i1
                         w2 = imageWidth  i2
                         h2 = imageHeight i2
                         c1 = compare w1 w2
                         c2 = compare h1 h2
                     in if c1 == c2 then Right c1 else Left "Incompatible sizes"

--Recibe una función que toma dos pixeles y genera otro a partir de ellos.
--Se usa dicha función para hacer un mapeo a dos imágenes del mismo tamaño pixel por pixel. 
pixelMap2 :: (PixelRGBA8 -> PixelRGBA8 -> PixelRGBA8) -> Image PixelRGBA8 -> Image PixelRGBA8 -> Image PixelRGBA8
pixelMap2 f img1 img2 = pixelMapXY (\x y p1 -> f p1 (pixelAt img2 x y)) img1


--Recibe las coordenadas de un pixel, un pixel y una imágen y genera un pixel nuevo. 
--Se usa dicha función para hacer un mapeo a dos imágenes teniendo el control de como se accede a los pixeles de la segunda,
--lo cual permite que tengan distintos tamaños. 
pixelMap2XY :: (Int -> Int -> PixelRGBA8 -> Image PixelRGBA8 -> PixelRGBA8) -> Image PixelRGBA8 -> Image PixelRGBA8 -> Image PixelRGBA8
pixelMap2XY f img1 img2 = pixelMapXY (\x y pix1 -> f x y pix1 img2) img1


--Genera una máscara que consiste en un círculo de radio r centrado en una imágen de w * h.
circCenteredMask :: Int -> Int -> Int -> Image PixelRGBA8
circCenteredMask w h r = let xCenter     = w `div` 2 
                             yCenter     = h `div` 2
                             rSquared    = r^2
                             visible     = PixelRGBA8   0   0   0  255
                             transparent = PixelRGBA8 255 255 255    0
                             asignPixel :: Int -> Int -> PixelRGBA8
                             asignPixel  = \x y -> if | (x-xCenter)^2 + (y-yCenter)^2 <= rSquared -> visible
                                                      | otherwise                                 -> transparent 
                         in generateImage asignPixel w h

                                        
--Toma una imágen y una máscara del mismo tamaño y aplica la máscara. 
applyMask :: Image PixelRGBA8 -> Image PixelRGBA8 -> Image PixelRGBA8
applyMask = pixelMap2 (mixWithAlpha (\_ v1 _ -> v1) (\_ alpha2 -> alpha2))


--Toma una imágen y una máscara más chica. Centra la máscara y la aplica. 
applySmallerMask :: Image PixelRGBA8 -> Image PixelRGBA8 -> Image PixelRGBA8
applySmallerMask img msk = let xShift = (imageWidth  img - imageWidth  msk) `div` 2
                               yShift = (imageHeight img - imageHeight msk) `div` 2
                           in pixelMap2XY (\x y pix1 img2 -> mixWithAlpha (\_ _ v2 -> v2) (\alpha1 _ -> alpha1) pix1 $ pixelAt img2 (x + xShift) (y + yShift)) msk img

--Calcula la relación rojo/azul de un pixel. 
redBlueRatio :: PixelRGBA8 -> Float
redBlueRatio (PixelRGBA8 r _ b _) = if b /= 0 then fromIntegral r / fromIntegral b else 1

--Revuelve si un pixel es visible o no. 
isVisible :: PixelRGBA8 -> Bool
isVisible = ((0/=) . pixelOpacity)

--Constantes que definen los colores del cielo y nubes en la imágen segmentada.
sky   = PixelRGBA8    0   0   0 255
cloud = PixelRGBA8  255 255 255 255

--Recibe una imágen y la segmenta. 
segImage :: Image PixelRGBA8 -> Image PixelRGBA8
segImage = pixelMap (\pix -> if | (not . isVisible) pix   -> pix
                                | redBlueRatio pix < 0.95 -> sky
                                | otherwise               -> cloud)                

--Recibe una imágen y calcula su índice de cobertura nubosa.   
cci :: Image PixelRGBA8 -> Float
cci = ((\(c,t) -> fromIntegral c / fromIntegral t) . pixelFold (\(c,t) _ _ pix -> if | pix == cloud -> (c+1,t+1)
                                                                                     | pix == sky   -> (c  ,t+1)
                                                                                     | otherwise    -> (c  ,t  ))    (0,0))
  
