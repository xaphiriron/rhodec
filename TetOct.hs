module TetOct
	(	Coordinate(..)
	,	lattice
	,	unlattice
	) where

import Linear.V3
import Control.Applicative

type Coordinate = V3 Int

data CellType = Octahedral | Tetrahedralα | Tetrahedralβ
	deriving (Show, Read, Eq)

-- to the center of the cell, i.e., the same coordinate as the equivalent rhombic vertex (the -2 is to offset them, since the 0 0 0 octagon cell here is being treated as the 10th vertex of the 0 0 0 rhombic dodecahedron cell. v 10 == V3 0 (-2) 0)
lattice :: Num a => Coordinate -> V3 a
lattice toc = V3 (z+x) (y-2) (z-x)
	where
		V3 x y z = fmap fromIntegral toc

unlattice :: Integral a => V3 a -> Coordinate
unlattice (V3 x' y' z') = fmap fromIntegral $ V3 x y z
	where
		x = (x' - z') `div` 2
		y = y' + 2
		z = z' + x

cellType :: V3 Int -> CellType
cellType c = case fmap (`mod` 4) c of
	V3 x 0 z
		|	all (== 0) $ (`mod` 2) <$> [x,z] -> Octahedral
	V3 x 1 z
		|	x `mod` 2 == 0 && z `mod` 2 == 1 -> Tetrahedralα
		|	x `mod` 2 == 1 && z `mod` 2 == 0 -> Tetrahedralβ
	V3 x 2 z
		|	all (== 1) $ (`mod` 2) <$> [x,z] -> Octahedral
	V3 x 3 z
		|	x `mod` 2 == 0 && z `mod` 2 == 1 -> Tetrahedralβ
		|	x `mod` 2 == 1 && z `mod` 2 == 0 -> Tetrahedralα
	_ -> error "oh no invalid cell"

adjacencies :: CellType -> [Coordinate]
adjacencies t = case t of
	Octahedral ->
		[ V3   0    1    1  -- u n
		, V3 (-1)   1    0  -- u w
		, V3   0    1  (-1) -- u s
		, V3   1    1    0  -- u e
		, V3   0  (-1)   1  -- l n
		, V3 (-1) (-1)   0  -- l w
		, V3   0  (-1) (-1) -- l s
		, V3   1  (-1)   0  -- l e
		]
	Tetrahedralα ->
		[ V3 (-1)   1    0  -- u w
		, V3   1    1    0  -- u e
		, V3   0  (-1)   1  -- l n
		, V3   0  (-1) (-1) -- l s
		]
	Tetrahedralβ ->
		[ V3   0    1    1  -- u n
		, V3   0    1  (-1) -- u s
		, V3 (-1) (-1)   0  -- l w
		, V3   1  (-1)   0  -- l e
		]

adjacentCells :: Coordinate -> [Coordinate]
adjacentCells c = (c +) <$> adj
	where
		adj = adjacencies $ cellType c
