module Cell
	(	Cell(..)
	,	CellFace(..)
	,	cellType
	,	cellContour
	,	blockingFace
	) where

import Material
import Linear.V4
import Linear.V3

data Cell = Cell Material
	deriving (Show, Read, Eq)

-- type, face index (from faces), total irradiance (i.e., NOT emittance)
data CellFace a = CellFace Material Int (V4 Float) a
	deriving (Show, Read, Eq)

instance Functor CellFace where
	fmap f (CellFace t i l a) = CellFace t i l (f a)

cellType :: Cell -> Material
cellType (Cell t) = t

cellContour :: Num a => Material -> [[a]]
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

blockingFace :: Maybe Cell -> Bool
blockingFace Nothing = True -- if False then unloaded cells act like air cells (i.e., they generate adjacent faces) whereas for the most part they should _not_, since cells on the edges of the map should (ideally) never be visible
blockingFace (Just (Cell t)) = not $ isTranslucent t
