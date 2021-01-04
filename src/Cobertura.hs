{-# LANGUAGE MultiWayIf #-}

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




compareSizes :: Image a -> Image b -> Either String Ordering
compareSizes i1 i2 = let w1 = imageWidth  i1
                         h1 = imageHeight i1
                         w2 = imageWidth  i2
                         h2 = imageHeight i2
                         c1 = compare w1 w2
                         c2 = compare h1 h2
                     in if c1 == c2 then Right c1 else Left "Incompatible sizes"

pixelMap2 :: (PixelRGBA8 -> PixelRGBA8 -> PixelRGBA8) -> Image PixelRGBA8 -> Image PixelRGBA8 -> Image PixelRGBA8
pixelMap2 f img1 img2 = pixelMapXY (\x y p1 -> f p1 (pixelAt img2 x y)) img1


pixelMap2XY :: (Int -> Int -> PixelRGBA8 -> Image PixelRGBA8 -> PixelRGBA8) -> Image PixelRGBA8 -> Image PixelRGBA8 -> Image PixelRGBA8
pixelMap2XY f img1 img2 = pixelMapXY (\x y pix1 -> f x y pix1 img2) img1


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

                                        
--Let's supp
applyMask :: Image PixelRGBA8 -> Image PixelRGBA8 -> Image PixelRGBA8
applyMask = pixelMap2 (mixWithAlpha (\_ v1 _ -> v1) (\_ alpha2 -> alpha2))


applySmallerMask :: Image PixelRGBA8 -> Image PixelRGBA8 -> Image PixelRGBA8
applySmallerMask img msk = let xShift = (imageWidth  img - imageWidth  msk) `div` 2
                               yShift = (imageHeight img - imageHeight msk) `div` 2
                           in pixelMap2XY (\x y pix1 img2 -> mixWithAlpha (\_ _ v2 -> v2) (\alpha1 _ -> alpha1) pix1 $ pixelAt img2 (x + xShift) (y + yShift)) msk img


redBlueRatio :: PixelRGBA8 -> Float
redBlueRatio (PixelRGBA8 r _ b _) = fromIntegral r / fromIntegral b


isVisible :: PixelRGBA8 -> Bool
isVisible = ((0/=) . pixelOpacity)


sky   = PixelRGBA8    0   0   0 255
cloud = PixelRGBA8  255 255 255 255


segImage :: Image PixelRGBA8 -> Image PixelRGBA8
segImage = pixelMap (\pix -> if | (not . isVisible) pix   -> pix
                                | redBlueRatio pix < 0.95 -> sky
                                | otherwise               -> cloud)                
  
cci :: Image PixelRGBA8 -> Float
cci = ((\(c,t) -> fromIntegral c / fromIntegral t) . pixelFold (\(c,t) _ _ pix -> if | pix == cloud -> (c+1,t+1)
                                                                                     | pix == sky   -> (c  ,t+1)
                                                                                     | otherwise    -> (c  ,t  ))    (0,0))
  
-- Unused Functions

verifySize :: Metadatas -> Word -> Word -> Bool
verifySize m w h = case (Meta.lookup Width m, Meta.lookup Height m) of
                     (Just w', Just h') -> w == w' && h == h'
                     _                  -> False
