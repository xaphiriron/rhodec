module RhoDec
	(	Coordinate(..)
	,	lattice
	,	unlattice
	,	unlatticeI
	) where

import Linear.V3

type Coordinate = V3 Int

-- note that this calculation influences which faces are shared between which adjacent coordinates. it's basically arbitrary & i'm using a constants table left over from c, which is why the axes are the way they are.
lattice :: Num a => Coordinate -> V3 a
lattice (V3 x y z) = fmap fromIntegral $
	sum
		[	V3 x2 x2 0
		,	V3 0 y2 y2
		,	V3 z2 0 z2
		]
		where
			x2 = x * 2
			y2 = y * 2
			z2 = z * 2

-- this is some extremely compressed gaussian elimination. it's from the c version. i don't know how well it works on non-grid points; i don't remember how to do gaussian elimination. - tzh 2013 06 04
unlattice :: RealFrac a => V3 a -> Coordinate
unlattice (V3 x y z) = fmap round $ V3 ax ay az
	where
		az = l3 / 4
		ay = (l2 + az * 2) / 2
		ax = (l1 - az * 2) / 2
		l1 = x
		l2 = y - l1
		l3 = z - l2

unlatticeI :: Integral a => V3 a -> Coordinate
unlatticeI (V3 x y z) = fmap fromIntegral $ V3 ax ay az
	where
		az = l3 `div` 4
		ay = (l2 + az * 2) `div` 2
		ax = (l1 - az * 2) `div` 2
		l1 = x
		l2 = y - l1
		l3 = z - l2
