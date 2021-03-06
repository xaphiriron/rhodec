module Lattice
	(	XQuad(..)
	,	WorldSpace(..)
	,	Side(..)
	,	Face(..)
	,	CellFaceQ(..)
	,	collide
	,	deformations
	,	geometry
	,	lighting
	,	faceColor

	,	faces
	,	v
	,	hitVisibleFace
	,	raycast
	) where

import Data.Tuple (swap)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.List (group, elemIndex, genericLength, groupBy, sortBy, nub)
import Data.Function (on)
import Control.Arrow (first, second, (&&&))
import Control.Applicative (Applicative, (<$>), (<*>), pure)
import Control.Monad (liftM, ap, join)

import qualified Data.Map as Map
import Data.Map (Map(..))
import Graphics.Rendering.OpenGL (GLfloat)
import qualified Graphics.Rendering.OpenGL as GL (Vertex3(..), Color4(..), Normal3(..))
import System.Random (mkStdGen)
import Control.Monad.Random (getRandomR, evalRand)

import Linear.V2
import Linear.V3
import Linear.V4
import Linear.Metric
import Linear.Epsilon (Epsilon(..))
import Linear.Vector((^*), (*^))

import Cell
import Material(Material(..))
import qualified Material
import qualified RhoDec as RD
import qualified TetOct as TO

-- normal v0 v1 v2 v3
data XQuad a = XQuad a a a a a
	deriving (Show, Eq)

instance Functor XQuad where
	fmap f (XQuad n a b c d) = XQuad (f n) (f a) (f b) (f c) (f d)

instance Applicative XQuad where
	pure a = XQuad a a a a a
	(XQuad fn fa fb fc fd) <*> (XQuad n a b c d) =
		XQuad (fn n) (fa a) (fb b) (fc c) (fd d)

recross :: (Floating a, Epsilon a) => XQuad (V3 a) -> XQuad (V3 a)
recross (XQuad _ v0 v1 v2 v3) = XQuad n v0 v1 v2 v3
	where
		n = normalize $ (v1 - v0) `cross` (v3 - v0)

type WorldSpace = V3 Float

type CellFaceQ = CellFace (XQuad (V3 Float))

-- Face = Face side (index on faces list) (V2 i j, representing a point in the face plane where the axes are (v3 - v0) and (v1 - v0)) (quad deformation data)
data Side = Front | Back
	deriving (Show, Read, Eq, Enum, Bounded)
data Face = Face RD.Coordinate Int Side (V3 Float) (XQuad (V3 Float -> V3 Float))

data Turn = L | R | T
	deriving (Show, Read, Eq, Enum, Bounded)

-- todo: APPARENTLY a lot of these spare `V3 a` values are unnecessary (e.g., for the plane code use a scalar distance-of-plane-from-origin value; there's apparently something similar for line representations so you don't flat-out have to use two vectors). it might be worthwhile to change that, if it comes up

-- Plane normal {world space point on the plane to use as origin} (b/c i never got the hand of the Plane (V3 a b c) d) representation)
data Plane a = Plane (V3 a) (V3 a)
	deriving (Show, Read, Eq)

ns :: [V3 Float]
ns =
	[	V3   0.7071    0.7071    0.0 -- 6 11 7 13, 0
	,	V3   0.0       0.7071    0.7071 -- 3 9 7 11, 1
	,	V3   0.7071    0.0       0.7071 -- 7 9 5 13, 2

	,	V3   0.0       0.7071  (-0.7071) -- 6 8 2 11, 3
	,	V3 (-0.7071)   0.0       0.7071 -- 1 9 3 12, 4
	,	V3   0.7071  (-0.7071)   0.0 -- 5 10 4 13, 5

	,	V3 (-0.7071)   0.7071    0.0 -- 3 11 2 12, 6
	,	V3   0.7071    0.0     (-0.7071) -- 4 8 6 13, 7
	,	V3   0.0     (-0.7071)   0.7071 -- 5 9 1 10, 8

	,	V3 (-0.7071)   0.0     (-0.7071) -- 2 8 0 12, 9
	,	V3   0.0     (-0.7071) (-0.7071) -- 0 8 4 10, 10
	,	V3 (-0.7071) (-0.7071)   0.0 -- 0 10 1 12, 11
	]

{- this has several properties:
	the int values are arguments to `v`, which turns them into 3d points (knowing the int values themselves is useful in some cases)
	all the quads are front-facing.
	the vector/quad pair is such that each face listed shares that face with the adjacent cell denoted in the vector
	the list is ordered so that its reverse gives the face on the opposite side of the cell, or, identically, the its reverse gives the face shared with the adjacent cell
-}
faces :: [(XQuad Int, RD.Coordinate)]
faces =
	[	(XQuad 0 6 11 7 13, V3   1    0    0 )
	,	(XQuad 0 3  9 7 11, V3   0    1    0 )
	,	(XQuad 0 7  9 5 13, V3   0    0    1 )

	,	(XQuad 0 6  8 2 11, V3   1    0  (-1))
	,	(XQuad 0 1  9 3 12, V3 (-1)   1    0 )
	,	(XQuad 0 5 10 4 13, V3   0  (-1)   1 )

	,	(XQuad 0 3 11 2 12, V3   0    1  (-1))
	,	(XQuad 0 4  8 6 13, V3   1  (-1)   0 )
	,	(XQuad 0 5  9 1 10, V3 (-1)   0    1 )

	,	(XQuad 0 2  8 0 12, V3   0    0  (-1))
	,	(XQuad 0 0  8 4 10, V3   0  (-1)   0 )
	,	(XQuad 0 0 10 1 12, V3 (-1)   0    0 )
	]

v :: Num a => Int -> V3 a
v x = fromMaybe (error "invalid vertex index") $
	[	V3 n1 n1 n1
	,	V3 n1 n1 p1
	,	V3 n1 p1 n1
	,	V3 n1 p1 p1
	,	V3 p1 n1 n1
	,	V3 p1 n1 p1
	,	V3 p1 p1 n1
	,	V3 p1 p1 p1
	,	V3  0  0 n2
	,	V3  0  0 p2
	,	V3  0 n2  0
	,	V3  0 p2  0
	,	V3 n2  0  0
	,	V3 p2  0  0
	] !!! x
	where
		p1 = 1
		n1 = -1
		p2 = 2
		n2 = -2
		(!!!) as i
			| i < 0 = Nothing
			| otherwise = listToMaybe . drop i $ as

-- as in, these vertices represent the same lattice points
sharedVertices :: [[(Int, RD.Coordinate)]]
sharedVertices =
	[	[(0,V3 0 0 0),(3,V3 0 (-1) 0),(5,V3 0 0 (-1)),(6,V3 (-1) 0 0)]
	,	[(1,V3 0 0 0),(2,V3 (-1) 0 1),(4,V3 (-1) 1 0),(7,V3 (-1) 0 0)]
	,	[(2,V3 0 0 0),(1,V3 1 0 (-1)),(4,V3 0 1 (-1)),(7,V3 0 0 (-1))]
	,	[(3,V3 0 0 0),(0,V3 0 1 0),(5,V3 0 1 (-1)),(6,V3 (-1) 1 0)]
	,	[(4,V3 0 0 0),(1,V3 1 (-1) 0),(2,V3 0 (-1) 1),(7,V3 0 (-1) 0)]
	,	[(5,V3 0 0 0),(0,V3 0 0 1),(3,V3 0 (-1) 1),(6,V3 (-1) 0 1)]
	,	[(6,V3 0 0 0),(0,V3 1 0 0),(3,V3 1 (-1) 0),(5,V3 1 0 (-1))]
	,	[(7,V3 0 0 0),(1,V3 1 0 0),(2,V3 0 0 1),(4,V3 0 1 0)]
	,	[(8,V3 0 0 0),(9,V3 1 (-1) (-1)),(10,V3 1 0 (-1)),(11,V3 0 (-1) 0),(12,V3 1 (-1) 0),(13,V3 0 0 (-1))]
	,	[(9,V3 0 0 0),(8,V3 (-1) 1 1),(10,V3 0 1 0),(11,V3 (-1) 0 1),(12,V3 0 0 1),(13,V3 (-1) 1 0)]
	,	[(10,V3 0 0 0),(8,V3 (-1) 0 1),(9,V3 0 (-1) 0),(11,V3 (-1) (-1) 1),(12,V3 0 (-1) 1),(13,V3 (-1) 0 0)]
	,	[(11,V3 0 0 0),(8,V3 0 1 0),(9,V3 1 0 (-1)),(10,V3 1 1 (-1)),(12,V3 1 0 0),(13,V3 0 1 (-1))]
	,	[(12,V3 0 0 0),(8,V3 (-1) 1 0),(9,V3 0 0 (-1)),(10,V3 0 1 (-1)),(11,V3 (-1) 0 0),(13,V3 (-1) 1 (-1))]
	,	[(13,V3 0 0 0),(8,V3 0 0 1),(9,V3 1 (-1) 0),(10,V3 1 0 0),(11,V3 0 (-1) 1),(12,V3 1 (-1) 1)]
	]

average :: Fractional a => [a] -> a
average vs = sum vs / genericLength vs

clamp :: Ord a => a -> a -> a -> a
clamp lo hi = max lo . min hi

toVertex :: V3 a -> GL.Vertex3 a
toVertex (V3 x y z) = GL.Vertex3 x y z

toNormal :: V3 a -> GL.Normal3 a
toNormal (V3 x y z) = GL.Normal3 x y z

toColor :: V4 a -> GL.Color4 a
toColor (V4 w x y z) = GL.Color4 w x y z

points :: XQuad a -> [a]
points (XQuad _ a b c d) = [a, b, c, d]

centroid :: (Num a, Fractional a) => [V3 a] -> V3 a
centroid vs = sum vs / (fromIntegral . length $ vs)

adjacentFaces :: RD.Coordinate -> [RD.Coordinate]
adjacentFaces v = fmap ((+ v) . snd) faces

adjacentCells :: Map RD.Coordinate Cell -> RD.Coordinate -> [Maybe Cell]
adjacentCells w = fmap (`Map.lookup` w) . adjacentFaces

adjacent :: Map RD.Coordinate Cell -> RD.Coordinate -> Int -> Maybe Cell
adjacent w c i = (`Map.lookup` w) . (c +) . snd $ faces !! i


{- i dont know anything about how this works. i designed it around having to
   offset by 0 -2 0 to get the TO origin cell to line up with the bottom RD
   vertex -- that's vertex 10, which accounts for those particular magic
   numbers. originally this was just:
toRhombic toc =
	let rc = RD.unlatticeI . subtract (v 10) . TO.lattice $ toc
	in fmap (first (rc +) . swap) $ sharedVertices !! 10
   except, of course, there are TO coordinates that don't correspond to the
   8--13 set of shared vertices. specifically, the TO coordinates that are
   tetrahedral cells corresond to one of two other shared vertex sets. i don't
   know why 5 is the magic number in one and not subtracting v 10 and 7 is the
   magic number in the other. i suspect it has more to do with the integer
   division in unlatticeI than anything else. if you have to alter this in the
   future, i'm sorry.
    - tzh 2013 08 06
-}
toRhombic :: TO.Coordinate -> [(RD.Coordinate, Int)]
toRhombic toc =
	case TO.cellType toc of
		TO.Octahedral -> -- 8 9 10 11 12 13
			let rc = RD.unlatticeI . subtract (v 10) . TO.lattice $ toc
			in fmap (first (rc +) . swap) $ sharedVertices !! 10
		TO.Tetrahedralα -> -- 0 3 5 6
			let rc = RD.unlatticeI . subtract (v 10) . TO.lattice $ toc
			in fmap (first (rc +) . swap) $ sharedVertices !! 5
		TO.Tetrahedralβ -> -- 1 2 4 7
			let rc = RD.unlatticeI . subtract (V3 0 0 0) . TO.lattice $ toc
			in fmap (first (rc +) . swap) $ sharedVertices !! 7

toTetOct :: RD.Coordinate -> Int -> TO.Coordinate
toTetOct rc i =
	TO.unlattice $ RD.lattice rc + v i

deformFace :: Num a => Map TO.Coordinate a -> RD.Coordinate -> Int -> XQuad (a -> a)
deformFace vw c i =
	fmap ((+) . fromMaybe 0 . (`Map.lookup` vw) . toTetOct c) .
		fst $ faces !! i

-- return the collision faces on the first cell that matches the filter function, along a ray from the given coordinates
-- deformation data, distance cutoff, ray origin, ray angle, filter function
collide :: Map TO.Coordinate (V3 Float) -> Int -> WorldSpace -> V3 Float -> ((Face, Face) -> Bool) -> Maybe (Face, Face)
-- `drop 1` here to discard the first collision, which is (presumably) with the starting cell and thus has a spurious front-face collison value that's actually backwards behind the start of the ray
collide d l o r f = listToMaybe . filter f . drop 1 . take l $ raycast d o r

raycast :: Map TO.Coordinate (V3 Float) -> WorldSpace -> V3 Float -> [(Face, Face)]
raycast d o r = raycast' d o r $ RD.unlattice o

-- ray origin -> ray vector -> cell to collide against this try -> [(front face hit, back face hit)]
raycast' :: Map TO.Coordinate (V3 Float) -> WorldSpace -> V3 Float -> RD.Coordinate -> [(Face, Face)]
raycast' d o r c =
	case rayCollide of
		Nothing -> [] {-error $
			unwords
				[	"Cast not inside cell bounds:"
				,	show o
				,	show r
				,	show c
				]-} -- our math is wrong. a mistake has led us here. alternately, the rayCollide function hit a tangent that it couldn't correct for (which is most tangents)
		Just fs@(_, b) -> fs : raycast' d o r (c + adjFaceCell b)
	where
		adjFaceCell :: Face -> RD.Coordinate
		adjFaceCell (Face _ i _ _ _) = snd $ faces !! i
		rayCollide :: Maybe (Face, Face)
		rayCollide =
			case (check Front fs, check Back fs) of
				(Just f, Just b) -> Just (f, b)
				(Just f@(Face c i _ point _), Nothing) ->
					{-
					... if there's ONE T then you're passing through an edge, and you can hit the sole other face that shares that edge
					... if there're TWO (adjacent) Ts then you're passing through a vertex, and you can pick any of the other faces that share that vertex
					... otherwise there's some mysterious kind of numerical instability
					-}
					-- error handling for tangental hits :(
					case maybe 0 (length . filter (== T)) $ turnsHit (deformFace d c i) o r c i of
						1 ->
							case mj i of
								Just j -> Just (f, Face c j Back point (deformFace d c j))
								Nothing -> error "Weird instability while error-correcting for back face."
						0 -> Nothing -- error "Tangental hit without any visible tangents on a front face :("
						_ -> Nothing -- error "Tangental hit on vertex, with only front face."
				(Nothing, Just b@(Face nc i _ point _)) ->
					case maybe 0 (length . filter (== T)) $ turnsHit (deformFace d c i) o r c i of
						1 ->
							case mj i of
								Just j -> Just (Face nc j Front point (deformFace d c j), b)
								Nothing -> error "Weird instability while error-correcting for front face."
						0 -> Nothing -- error "Tangental hit without any visible tangents on a back face :("
						_ -> Nothing -- error "Tangental hit on vertex, with only back face."
				_ -> Nothing
			where
				check s =
					listToMaybe .
						filter (\(Face _ _ t _ _) -> s == t)
				fs =
					mapMaybe (\i -> quadHit (deformFace d c i) o r c i) [0..11]
				mj i = do
					t <- elemIndex T =<< turnsHit (deformFace d c i) o r c i
					let u = (t + 1) `mod` 4
					let tv = (!! t) (points $ fst $ faces !! i)
					let uv = (!! u) (points $ fst $ faces !! i)
					listToMaybe .
						fmap fst .
							filter ((/= i) . fst) .
								filter ((== 2) . length . filter (\x -> x == tv || x == uv) . points . snd) .
									zip [0..] $
										fmap fst faces

turnsHit :: XQuad (V3 Float -> V3 Float) -> WorldSpace -> V3 Float -> RD.Coordinate -> Int -> Maybe [Turn]
turnsHit d o r c i = pointInQuad' q i <$> mpoint
	where
		q@(XQuad _ v0 v1 v2 v3) =
			recross . (d <*>) .
				fmap ((RD.lattice c +) . v) . fst $ faces !! i
		mpoint =
			fmap (\t -> o + (r ^* t)) $
				planeLineIntersection (quadPlane q i) o r

quadHit :: XQuad (V3 Float -> V3 Float) -> WorldSpace -> V3 Float -> RD.Coordinate -> Int -> Maybe Face
quadHit d o r c i =
	if fromMaybe False $ pointInQuad q i <$> mpoint
		then Face c i <$> mside <*> mpoint <*> pure d
		else Nothing
	where
		q@(XQuad n v0 v1 v2 v3) =
			recross . (d <*>) .
				fmap ((RD.lattice c +) . v) . fst $ faces !! i
		mside = case r `dot` n of
			x	|	x < 0 -> Just Front
				|	x > 0 -> Just Back
				|	otherwise -> Nothing
		mpoint =
			fmap (\t -> o + (r ^* t)) $
				planeLineIntersection (quadPlane q i) o r

turns :: (Num a, Ord a) => V2 a -> (V2 a, V2 a) -> Turn
turns (V2 ox oy) (V2 l1x l1y, V2 l2x l2y) =
	case (l1x - ox) * (l2y - oy) - (l2x - ox) * (l1y - oy) of
		s	|	s < 0 -> R
			|	s > 0 -> L
			|	otherwise -> T

pointInQuad :: (Floating a, Epsilon a, Ord a) => XQuad (V3 a) -> Int -> V3 a -> Bool
pointInQuad q i p = (== 1) . length . group . filter (/= T) $ pointInQuad' q i p

-- this is side-agnostic; i think the thing is if the dimension dropped is negative then we can flip around the resulting V2s (i.e., V2 z y instead of V2 y z) to get a situation where all Ls means front face hit and all Rs means back face hit. as it is, since we don't, sometimes either can mean either
pointInQuad' :: (Floating a, Epsilon a, Ord a) => XQuad (V3 a) -> Int -> V3 a -> [Turn]
pointInQuad' q@(XQuad n v0 v1 v2 v3) i p = fmap (turns $ project2d p) lines
	where
		lines = zip (points flat) (rotate 1 $ points flat)
			where
				flat = fmap project2d q
		(V3 nx ny nz) = fmap abs n
		project2d =
			case maximum [nx, ny, nz] of
				m	|	m == nx -> \(V3 _ y z) -> V2 y z
					|	m == ny -> \(V3 x _ z) -> V2 z x
					|	m == nz -> \(V3 x y _) -> V2 x y

rotate :: Int -> [a] -> [a]
rotate n xs
	| n > ln = rotate (n - ln) xs
	| n < 0 = rotate (ln + n) xs
	| otherwise = drop n xs ++ take n xs
	where ln = length xs

-- plane (line "origin") (line ray vector)
planeLineIntersection :: (Eq a, Fractional a) => Plane a -> V3 a -> V3 a -> Maybe a
planeLineIntersection (Plane n o) l0 l1 =
	case n `dot` l1 of
		0 -> Nothing -- line parallel or coincicent with plane
		denom -> Just $ -(n `dot` l0') / denom
	where
		l0' = l0 - o

quadPlane :: Floating a => XQuad (V3 a) -> Int -> Plane a
quadPlane q@(XQuad n v0 _ _ _) i = Plane n v0

-- todo: all the deformation code does the same thing, but the code itself is phrased slightly different in all cases. ideally it should all be almost identical, or even like a typeclass that some Edge Center Vertex data types are instances of. particularly the direction of each pushvector is really confusing since the code to generate them is different in all three cases even though there's no reason for it not to be identical

-- the vectors: point FROM the given point TO the center of the adjacent cell. so really they're more like pullVectors, as in, the higher the cellPush type contant the more the vertex is pulled towards the center of the given vertex
pushVectors :: RD.Coordinate -> Int -> [(RD.Coordinate, V3 GLfloat)]
pushVectors c i = zip
	((c +) <$> fmap snd (sharedVertices !! i)) $
	(-) (v i) <$> fmap (RD.lattice . snd) (sharedVertices !! i)

matDeform :: Map RD.Coordinate Cell -> RD.Coordinate -> Int -> V3 GLfloat
matDeform m c i = noise + smooth
	where
		noise = evalRand (do
			-- todo: generate a length-one vector that's actually statistically random wrt direction of angle
			x' <- getRandomR (-1, 1)
			y' <- getRandomR (-1, 1)
			z' <- getRandomR (-1, 1)
			return $ normalize (V3 x' y' z') ^* j
			) $ rc (RD.lattice c + v i)
			where
				rc (V3 x y z) = mkStdGen $
					sum [(round x + 17) * 3433, (round y + 19) * 3449, (round z + 23) * 3457]
				j = clamp 0 1 . average . fmap (maybe 0 (Material.variance . cellType)) $ cells
				cells = fmap ((`Map.lookup` m) . fst) $ pushVectors c i
		smooth =
			sum . fmap typePush .
				groupBy ((==) `on` fst) .
					sortBy (compare `on` fst) $
						cells
			where
				cells =
					fmap (first $ maybe Air cellType . (`Map.lookup` m)) $
						pushVectors c i
				l = genericLength cells
				typePush :: [(Material, V3 Float)] -> V3 Float
				typePush [] = 0
				typePush (v@(t,_):vs) = (Material.push t *^) . (c *^) . (/ l) . normalize . sum . fmap snd $ v:vs
					where
						c = genericLength vs + 1

isEmitting :: CellFace a -> Bool
isEmitting = (\(V4 _ _ _ l) -> l /= 0) . emittance

-- this is a RGBA color, for use in actually coloring the face in OpenGL.
faceColor :: CellFace a -> V4 Float
faceColor (CellFace t _ (V4 ir ig ib il) _) =
	((* e) <$> V4 mr mg mb 0) + V4 (mr * ir) (mg * ig) (mb * ib) ma
	where
		e = clamp 0 1 $ Material.light t
		V4 mr mg mb ma = Material.color t

-- this is a RGBL color, for use in calculating more RGBL lighting values.
emittance :: CellFace a -> V4 Float
emittance (CellFace t _ (V4 ir ig ib il) _) =
	((* e) <$> V4 mr mg mb 1) + V4 (mr * ir) (mg * ig) (mb * ib) il
	where
		e = Material.light t
		V4 mr mg mb _ = Material.color t

--todo: reduce duplication below. note the two almost-identical visible & cellFaces functions.

-- turn a cell map into a vertex deformation map (w/ only vertices used in at least one visible face set) ACTUALLY STRIKE THAT THIS IS JUST EVERY VERTEX, W/E
deformations :: Map RD.Coordinate Cell -> Map TO.Coordinate (V3 Float)
deformations cw =
	Map.foldrWithKey vertex Map.empty .
		Map.mapWithKey visibleFaces $
			cw
	where
		vertex :: RD.Coordinate -> (Material, [Int]) -> Map TO.Coordinate (V3 Float) -> Map TO.Coordinate (V3 Float)
		vertex c (t, is) vw =
			Map.union vw . Map.fromList .
				fmap (toTetOct c &&& matDeform cw c) $
					is
		visibleFaces :: RD.Coordinate -> Cell -> (Material, [Int])
		visibleFaces c cell = {-(\(t, is) -> (t, filter (visible t) is)) .-} cellFaces $ cell
			where
				visible :: Material -> Int -> Bool
				visible t i =
					-- i.e., don't generate faces for air cells
					not (Material.isTransparent t) &&
						-- True/False here determines if unloaded cells are considered to be solid for geometry-generation purposes. On the whole they shouldn't be (thus True) but sometimes it's useful to see just where the loaded map stops
						(not . maybe True (Material.isOpaque . cellType) $ adjacent cw c i)
				cellFaces :: Cell -> (Material, [Int])
				cellFaces (Cell t) = (t, [0..11])

geometry :: Map RD.Coordinate Cell -> Map TO.Coordinate (V3 Float) -> Map RD.Coordinate [CellFaceQ]
geometry cw vw = Map.mapWithKey (\c -> fmap (reface c) . visibleFaces c) cw
	where
		reface :: RD.Coordinate -> CellFace () -> CellFaceQ
		reface c (CellFace t f l _) =
			CellFace t f l .
				recross $
					(deformFace vw c f <*>) .
						fmap ((RD.lattice c +) . v) . fst $ faces !! f
		visibleFaces :: RD.Coordinate -> Cell -> [CellFace ()]
		visibleFaces c cell = filter visible . cellFaces $ cell
			where
				visible :: CellFace () -> Bool
				visible (CellFace t i _ _) =
					-- i.e., don't generate faces for air cells
					not (Material.isTransparent t) &&
						-- True/False here determines if unloaded cells are considered to be solid for geometry-generation purposes. On the whole they shouldn't be (thus True) but sometimes it's useful to see just where the loaded map stops
						(not . maybe True (Material.isOpaque . cellType) $ adjacent cw c i)
				cellFaces :: Cell -> [CellFace ()]
				cellFaces (Cell t) = CellFace t <$> [0..11] <*> pure (V4 0 0 0 0) <*> pure ()

lighting ::  Map RD.Coordinate [CellFaceQ] -> Map RD.Coordinate [CellFaceQ]
lighting w = Map.mapWithKey (fmap . update) w
	where
		lights :: [(RD.Coordinate, CellFaceQ)]
		lights =
			filter (isEmitting . snd) .
				join .
					fmap liftSnd .
						Map.toList .
							Map.filter (any isEmitting) $
								w
		update :: RD.Coordinate -> CellFaceQ -> CellFaceQ
		update c f@(CellFace t i l q) = CellFace t i l' q
			where
				l' = sum $ totalIrradiance c f
				totalIrradiance :: RD.Coordinate -> CellFaceQ -> [V4 Float]
				totalIrradiance c f = lightFrom w <$> lights <*> pure c <*> pure f

{- nb: this casts using the lattice-aligned coordinates for every cell.
   the justification here is that collide/raycast casts through a regular lattice (not a deformed one) because to do otherwise would require more state -- raycast would need the world cell data to generate the deformed lattice -- and a really absurd amount of calculation (i.e., generating all cell walls even for occluded or air faces).
   consequentially, casting with centroid values from deformed faces introduces error, since you can have sufficiently-deformed faces such that the differences in casting strategies leads to different results, leading to (mostly) black splotches where casting fails where it should suceed.
-}
-- idk how to really deal with how this might be lit by multiple faces of the same cell at one time. increasing the number of light sources also increases the power of the lighting. one thing to maybe see if you can include: the angle of the lit _face_ vs. the target face. right now it compares the face normal of the target with the _angle_ of the light, totally ignoring the face normal of the _source_.
lightFrom :: Map RD.Coordinate [CellFaceQ] -> (RD.Coordinate, CellFaceQ) -> RD.Coordinate -> CellFaceQ -> V4 Float
lightFrom w (lc, lf@(CellFace _ li _ _)) c (CellFace _ i _ q@(XQuad n _ _ _ _)) =
		-- don't even bother colliding if this is distant enough to be weak. note: "weakness" in proper radiosity terms is not a fixed quantity, so when multiple passes become a thing this should be checked again (b/c then "weakness" can we revised upwards b/c there's a lot more total light in the scene)
		-- don't bother calculating self-shadowing; it doesn't end well (there are zero-length rays when raycasting)
		if lc == c || (\(V4 _ _ _ l) -> l) light < 0.0125
			then 0
			else
				-- cast from the light to the target
				case collide Map.empty 50 lightCentroid diff (hitVisibleFace w) of
					Nothing -> 0
					Just f@(Face hit _ _ _ _, _)
						|	hit == c -> light
						|	otherwise -> 0
		where
			light = d * θ *^ emittance lf
			lightCentroid = (RD.lattice lc +) . centroid . points . fmap v . fst $ faces !! li
			-- ignore light hits on the back of the face (where the dot product is positive)
			θ = (min 1 . abs . min 0) $ n `dot` normalize diff
			diff = targetCentroid - lightCentroid -- from the light, towards the target
			-- 4 here is a world space -> coord space lighting transform constant (since each rhodec is 4~ world-space units wide)
			d = 4 * (1 / (dist * dist))
			dist = distance targetCentroid lightCentroid
			targetCentroid = (RD.lattice c +) . centroid . points . fmap v . fst $ faces !! i

liftSnd :: Monad m => (a, m b) -> m (a, b)
liftSnd (a, mb) = do
	b <- mb
	return (a, b)

liftFst :: Monad m => (m a, b) -> m (a, b)
liftFst (ma, b) = do
	a <- ma
	return (a, b)

hitVisibleFace :: Map RD.Coordinate [CellFaceQ] -> (Face, Face) -> Bool
hitVisibleFace w (Face c f _ _ _, _) =
	case Map.lookup c w of
		Nothing -> True
		Just fs -> any (\(CellFace _ cf _ _) -> f == cf) fs

