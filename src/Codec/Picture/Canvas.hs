-- | Functions for drawing lines, squares and so on pixel by pixel
module Codec.Picture.Canvas (
    Canvas,
    canvasWidth,
    canvasHeight,
    imageToCanvas,
    canvasToImage,
    getColor,
    setColor,
    drawLine,
    drawPolygon,
    drawRectangle,
    drawSquare
  ) where

import Codec.Picture
import Data.Bits
import Data.Maybe
import qualified Data.List as L
import qualified Data.IntMap.Strict as M

-- | Canvas ADT
data Canvas a =
  Canvas {
    canvasWidth :: !Int,    -- ^ Canvas width
    canvasHeight :: !Int,   -- ^ Canvas height
    pixels :: !(M.IntMap a)
  } deriving (Eq)

instance Show (Canvas a) where
    show c = "Canvas { w = " ++ show (canvasWidth c) ++ ", h = " ++ show (canvasHeight c) ++ ", ... }"

-- | Convert Image to Canvas
imageToCanvas :: Pixel a => Image a -> Either String (Canvas a)
imageToCanvas img
    | w > maxWH = err "width"
    | h > maxWH = err "height"
    | otherwise =
        Right Canvas {
            canvasWidth = w,
            canvasHeight = h,
            pixels = M.fromList [(makeKey x y, pixelAt img x y) | x <- [0..w-1], y <- [0..h-1]]
        }
    where
        w = imageWidth img
        h = imageHeight img
        err s = Left $ "Image " ++ s ++ " is larger than supported maximum: " ++ show maxWH

-- | Convert Canvas to Image
canvasToImage :: Pixel a => Canvas a -> Image a
canvasToImage c =
    generateImage (\x y -> getColor x y c) (canvasWidth c) (canvasHeight c)

-- | Get color of specified pixel
getColor :: Pixel a => Int -> Int -> Canvas a -> a
getColor x y canvas =
    fromJust $ M.lookup (makeKey x y) (pixels canvas)

-- | Set color of specified pixel
setColor :: Pixel a => Int -> Int -> a -> Canvas a -> Canvas a
setColor x y color canvas =
    canvas { pixels = M.insert (makeKey x y) color (pixels canvas) }

-- | Draw a line with specified color
drawLine :: Pixel a => Int -> Int -> Int -> Int -> a -> Canvas a -> Canvas a
drawLine x1 y1 x2 y2 color canvas =
    let dx = fromIntegral (x2 - x1) :: Double
        dy = fromIntegral (y2 - y1) :: Double in
    if abs dx > abs dy
        then L.foldl' 
                (\acc x -> let y = y1 + truncate (dy * fromIntegral (x - x1) / dx) in setColor x y color acc)
                canvas [min x1 x2 .. max x1 x2]
        else L.foldl'
                (\acc y -> let x = x1 + truncate (dx * fromIntegral (y - y1) / dy) in setColor x y color acc)
                canvas [min y1 y2 .. max y1 y2]

-- | Draw a polygon with specified color
drawPolygon :: Pixel a => [(Int, Int)] -> a -> Canvas a -> Canvas a
drawPolygon [ ] _ canvas = canvas
drawPolygon [_] _ canvas = canvas
drawPolygon ((x1,y1):xs@((x2,y2):_)) color canvas =
    let c' = drawLine x1 y1 x2 y2 color canvas in
    drawPolygon xs color c'

-- | Draw a rectangle with specified color
drawRectangle :: Pixel a => Int -> Int -> Int -> Int -> a -> Canvas a -> Canvas a
drawRectangle x y w h =
    drawPolygon [(x,y),(x+w,y),(x+w,y+h),(x,y+h),(x,y)]

-- | Draw a square with specified color
drawSquare :: Pixel a => Int -> Int -> Int -> a -> Canvas a -> Canvas a 
drawSquare x y s =
    drawRectangle x y s s

makeKey :: Int -> Int -> Int
makeKey x y = x `shiftL` 14 + y

maxWH :: Int
maxWH = 1 `shiftL` 14 - 1
