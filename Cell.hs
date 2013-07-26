module Cell
	(	CellType(..)
	,	Cell(..)
	,	CellCoordinate(..)
	,	CellFace(..)
	,	cellColor
	,	cellLight
	,	cellContour
	,	cellPush
	,	cellVariance
	,	isTranslucent
	,	isTransparent
	,	isOpaque
	,	blockingFace
	) where

import Linear.V4
import Linear.V3

data CellType = Air | Dirt | Rock | Gold | Crystal Float (V3 Float) | WhiteCell | RedCell
	deriving (Show, Read, Eq, Ord)
data Cell = Cell CellType
	deriving (Show, Read, Eq)

type CellCoordinate = V3 Int

-- type, face index (from faces), total irradiance (i.e., NOT emittance)
data CellFace = CellFace CellType Int (V4 Float)
	deriving (Show, Read, Eq)

cellColor :: CellType -> V4 Float
cellColor Air = V4 0 0 0 0
cellColor Dirt = V4 0.6 0.4 0.1 1.0
cellColor Rock = V4 0.6 0.5 0.4 1.0
cellColor Gold = V4 0.9 0.8 0.4 1.0
cellColor (Crystal _ (V3 r g b)) = V4 r g b 0.4
cellColor WhiteCell = V4 1.0 1.0 1.0 1.0
cellColor RedCell = V4 1.0 0.0 0.0 1.0

cellLight :: CellType -> Float
cellLight x = case x of
	Crystal i _ -> i
	_ -> 0

cellContour :: Num a => CellType -> [[a]]
cellContour _ = [[128]]
{-
cellContour Air = [[128]]
cellContour Dirt = [[128]]
cellContour Rock =
	[	[128, 112, 112, 128]
	,	[112,  96,  96, 112]
	,	[112,  96,  96, 112]
	,	[128, 112, 112, 128]
	]
cellContour Gold =
	[	[128, 112,  96, 128]
	,	[112, 112,  96, 128]
	,	[ 96,  96,  96, 128]
	,	[128, 128, 128, 128]
	]
cellContour (Crystal _ _) =
	[	[160,  96, 128, 144]
	,	[ 96, 128,  96, 128]
	,	[128,  96, 128,  96]
	,	[144, 128,  96, 160]
	]
-}

cellPush :: CellType -> Float
cellPush Air = 0.5
cellPush Gold = 0.9
cellPush Dirt = 0.7
cellPush Rock = 0.8
cellPush (Crystal _ _) = 1
cellPush _ = 1

cellVariance :: CellType -> Float
cellVariance Air = 0.25
cellVariance Dirt = 0.25
cellVariance Rock = 0.1
cellVariance Gold = 0.1
cellVariance (Crystal _ _) = 0.1
cellVariance _ = 0

blockingFace :: Maybe Cell -> Bool
blockingFace Nothing = True -- if False then unloaded cells act like air cells (i.e., they generate adjacent faces) whereas for the most part they should _not_, since cells on the edges of the map should (ideally) never be visible
blockingFace (Just (Cell t)) = not $ isTranslucent t

isOpaque :: CellType -> Bool
isOpaque t = case cellColor t of
	(V4 _ _ _ a)
		|	a < 1.0 -> False
		|	otherwise -> True

isTransparent :: CellType -> Bool
isTransparent t = case cellColor t of
	(V4 _ _ _ a)
		|	a <= 0.0 -> True
		|	otherwise -> False

isTranslucent :: CellType -> Bool
isTranslucent t = case cellColor t of
	(V4 _ _ _ a)
		| a >= 1.0 -> False
		| otherwise -> True
