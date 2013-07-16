{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

{- This program is free software. It comes without any warranty, to
 - the extent permitted by applicable law. You can redistribute it
 - and/or modify it under the terms of the Do What The Fuck You Want
 - To Public License, Version 2, as published by Sam Hocevar. See
 - http://www.wtfpl.net/, or the LICENSE file distributed with this
 - repository for more details.
 -}

{- TODO:
  * read input properly and hopefully not in such an imperative fashion
  * LIGHTING
    * something like 1/4th of the total lighting calc time is taken up by
      calls to `turns` and `pointInQuad`
    * also lighting is horrifically slow (as one might expect)
    * consider doing a flood rayout out from each lit face instead of having
      every single visible face in the world raycast to every light
-}

import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Arrow ((***), (&&&), first, second)
import Control.Monad ((=<<), join, liftM, unless, replicateM, replicateM_, when, ap)
import Control.Monad.STM
import Control.Exception (evaluate)
import System.Clock
import System.Random
import Control.Monad.Random

import Control.Lens
import Control.Applicative (Applicative(..), pure, (<*>), (<$>))
import Data.Function (on)
import Data.List (sortBy, group, groupBy, mapAccumR, transpose, elemIndex)
import Data.Maybe (catMaybes, mapMaybe, catMaybes, isNothing, fromMaybe, listToMaybe)
import Data.Ratio ((%))
import Data.Monoid
import Data.Map (Map)
import qualified Data.Map as Map

import Graphics.Rendering.OpenGL (($=), GLfloat, GLubyte, GLmatrix(..), PrimitiveMode(..), ClearBuffer(..), ComparisonFunction(..), Capability(Enabled, Disabled), MatrixMode(..), Color4(..), Light(..), Vertex4(..), MatrixComponent(..), MatrixOrder(RowMajor))
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW (Key(..), KeyButtonState (Release, Press))
import qualified Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL.GL.VertexSpec
import qualified Foreign.Ptr as F (castPtr, Ptr(..))
import qualified Foreign.Storable as F (peek, Storable(..))
import qualified Foreign.Marshal.Utils as F (new)

import Linear.Metric
import Linear.Quaternion
import Linear.Epsilon(Epsilon(..))
import Linear.V2
import Linear.V3
import Linear.V4
import Linear.Vector((^+^), (^-^), (^*), (*^), lerp)

import LSystem hiding ((^*^))
import qualified LSystem ((^*^))

import Cell

data XQuad a = XQuad a a a a
	deriving (Eq, Show)

instance Functor XQuad where
	fmap f (XQuad a b c d) = XQuad (f a) (f b) (f c) (f d)

toVertex :: V3 a -> GL.Vertex3 a
toVertex (V3 x y z) = GL.Vertex3 x y z

toNormal :: V3 a -> GL.Normal3 a
toNormal (V3 x y z) = GL.Normal3 x y z

toColor :: V4 a -> GL.Color4 a
toColor (V4 w x y z) = GL.Color4 w x y z

instance Num TimeSpec where
	(+) = liftTime (+)
	(*) = liftTime (*)
	(-) = liftTime (-)
	negate t = t * TimeSpec (-1) 0
	abs t =
		case signum t of
			-1 -> negate t
			_ -> t
	signum (TimeSpec s us) = fromIntegral $ signum s
	fromInteger x =
		uncurry TimeSpec .
			(fromInteger *** fromInteger) $
				x `quotRem` 1000000000

liftTime :: (Int -> Int -> Int) -> TimeSpec -> TimeSpec -> TimeSpec
liftTime f (TimeSpec as aus) (TimeSpec bs bus) =
	let
		(q, r) = (aus `f` bus) `quotRem` 1000000000
		(q', r') = if r < 0
			then (q - 1, r + 1000000000)
			else (q, r)
	in
		TimeSpec ((as `f` bs) + q') r'

-- don't really care about a full Integral instance (which also requires Enum and Rational)
fauxQuot :: Num a => TimeSpec -> TimeSpec -> a
fauxQuot a b = fromInteger $ fauxToInteger a `quot` fauxToInteger b
	where
		fauxToInteger (TimeSpec s us) = toInteger s * 1000000000 + toInteger us

drainTChan :: TChan a -> STM [a]
drainTChan cs = do
	b <- isEmptyTChan cs
	if b
		then return []
		else do
			c <- readTChan cs
			liftM (c : ) $ drainTChan cs

points :: XQuad a -> [a]
points (XQuad a b c d) = [a, b, c, d]

centroid :: (Num a, Fractional a) => [V3 a] -> V3 a
centroid vs = sum vs / (fromIntegral . length $ vs)

type WorldSpace = V3 Float

-- Face = Face side (index on faces list) (V2 i j, representing a point in the face plane where the axes are (v3 - v0) and (v1 - v0))
data Side = Front | Back
	deriving (Show, Read, Eq, Enum, Bounded)
data Face = Face CellCoordinate Int Side (V3 Float)
	deriving (Show, Read, Eq)

data World = World
	{	_cells :: Map CellCoordinate Cell
	,	_storedGeometry :: [(CellCoordinate, [CellFace])]
	}
type Generator = Map (V3 Int) (Maybe GeneratorState)

data FaceLink = Closed | Open | Passage Int Int
data GeneratorState = GeneratorState
	{	above :: Maybe FaceLink
	,	below :: Maybe FaceLink
	,	left :: Maybe FaceLink
	,	right :: Maybe FaceLink
	,	front :: Maybe FaceLink
	,	back :: Maybe FaceLink
	}

data GameState = GameState
	{	_camera :: Camera GLfloat
	,	_world :: World
	--,	_generator :: Generator -- this should always be fully populated w/ the 17x17x17 grid surrounding the player. 8 cell blocks, cubic, in all directions. (assuming that the player is always "in" a cell block)
	,	_quit :: Bool
	}

{-

assume there are also
loadChunk :: V3 Int -> IO World -- that loads the cells in the 8x8x8x4 cubic chunk on an imagined grid
generateChunk :: V3 Int -> Generators -> World -- that generates the given 8x8x8x4 cubic chunk

(the two above meant to be used as world %~ (`union` newWorld) $ state, more-or-less)

unloadChunk :: V3 Int -> World -> IO World -- that removes (and saves) all cells in the given cubic faux-chunk

adjacent :: Num a => V3 a -> [(Generator -> Maybe FaceLink, V3 a)]
adjacent (V3 x y z) =
	[	(back, V3 x y z-1)
	,	(front, V3 x y z+1)
	,	(below, V3 x y-1 z)
	,	(above, V3 x y+1 z)
	,	(left, V3 x-1 y z)
	,	(right, V3 x+1 y z)
	]

uncurry (=<<) . (id *** Map.lookup GENERATORSTATES) . adjacent $ loc
-- e.g., [Nothing, Just Closed, Just Open, Nothing, Nothing, Passage 3 6]

-}

data GLFWEvent
	= KeyEvent Key KeyButtonState
	| CharEvent Char KeyButtonState
	| MouseButtonEvent GLFW.MouseButton KeyButtonState
	| MousePositionEvent GL.Position
	| MouseWheelEvent Int
	| CloseEvent
	deriving (Eq, Show)

data UpdateAction
	= AdjustViewAngle (V2 GLfloat)
	| ManualImpel (V2 GLfloat)
	| GameQuit
	deriving (Eq)

data Camera a = Camera (Quaternion a) (V3 a)

instance (RealFloat a) => Monoid (Camera a) where
	mempty = Camera (Quaternion 1 $ V3 0 0 0) (V3 0 0 0)
	mappend (Camera aq ap) (Camera bq bp) = Camera (aq * bq) (ap + bp)

reorient :: (RealFloat a) => V2 a -> Quaternion a -> Quaternion a
reorient (V2 x y) o = (yq * o) * xq
	where
		yq = euler2quat y 0 0
		xq = euler2quat 0 x 0

-- expects x y values in radians
euler2quat :: (Floating a) => a -> a -> a -> Quaternion a
euler2quat x y z =
	Quaternion (cos x * cos y * cos z + sin x * sin y * sin z) $ -- w
		V3
			(sin x * cos y * cos z - cos x * sin y * sin z) -- x
			(cos x * sin y * cos z + sin x * cos y * sin z) -- y
			(cos x * cos y * sin z - sin x * sin y * cos z) -- z

quat2matrix :: (Num a, MatrixComponent a) => Quaternion a -> IO (GLmatrix a)
quat2matrix (Quaternion w (V3 x y z)) =
	GL.newMatrix RowMajor
		[ 1-2*y*y-2*z*z,   2*x*y-2*w*z,   2*x*z+2*w*y,             0
		,   2*x*y+2*w*z, 1-2*x*x-2*z*z,   2*y*z-2*w*x,             0
		,   2*x*z-2*w*y,   2*y*z+2*w*x, 1-2*x*x-2*y*y,             0
		,             0,             0,             0,             1
		]

positionMatrix :: (Num a, MatrixComponent a) => Quaternion a -> V3 a -> IO (GLmatrix a)
positionMatrix (Quaternion w (V3 x y z)) (V3 px py pz) =
	GL.newMatrix RowMajor
		[ sx, sy, sz, sum [-px * sx, -py * sy, -pz * sz]
		, ux, uy, uz, sum [-px * ux, -py * uy, -pz * uz]
		, fx, fy, fz, sum [-px * fx, -py * fy, -pz * fz]
		,  0,  0,  0,  1]
	where
		sx = 1-2*y*y-2*z*z
		sy =   2*x*y-2*w*z
		sz =   2*x*z+2*w*y
		ux =   2*x*y+2*w*z
		uy = 1-2*x*x-2*z*z
		uz =   2*y*z-2*w*x
		fx =   2*x*z-2*w*y
		fy =   2*y*z+2*w*x
		fz = 1-2*x*x-2*y*y

cameraRotationMatrix :: (Num a, MatrixComponent a) => Camera a -> IO (GLmatrix a)
cameraRotationMatrix (Camera q _) = quat2matrix q

cameraPositionMatrix :: (Num a, MatrixComponent a) => Camera a -> IO (GLmatrix a)
cameraPositionMatrix (Camera q v) = positionMatrix q v

makeLenses ''World
makeLenses ''GameState


adjacentFaces :: CellCoordinate -> [CellCoordinate]
adjacentFaces v = fmap ((+ v) . snd) faces

adjacentCells :: Map CellCoordinate Cell -> CellCoordinate -> [Maybe Cell]
adjacentCells w = fmap (`Map.lookup` w) . adjacentFaces

adjacent :: Map CellCoordinate Cell -> CellCoordinate -> Int -> Maybe Cell
adjacent w c i = (`Map.lookup` w) . (c +) . snd $ faces !! i

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
	all the quads are front-facing.
	the vector/quad pair is such that each face listed shares that face with the adjacent cell denoted in the vector
	the list is ordered so that its reverse gives the face on the opposite side of the cell, or, identically, the its reverse gives the face shared with the adjacent cell
-}
faces :: [(XQuad (V3 GLfloat), CellCoordinate)]
faces =
	[	(XQuad (v 6) (v 11) (v 7) (v 13), V3   1    0    0 )
	,	(XQuad (v 3) (v  9) (v 7) (v 11), V3   0    1    0 )
	,	(XQuad (v 7) (v  9) (v 5) (v 13), V3   0    0    1 )

	,	(XQuad (v 6) (v  8) (v 2) (v 11), V3   1    0  (-1))
	,	(XQuad (v 1) (v  9) (v 3) (v 12), V3 (-1)   1    0 )
	,	(XQuad (v 5) (v 10) (v 4) (v 13), V3   0  (-1)   1 )

	,	(XQuad (v 3) (v 11) (v 2) (v 12), V3   0    1  (-1))
	,	(XQuad (v 4) (v  8) (v 6) (v 13), V3   1  (-1)   0 )
	,	(XQuad (v 5) (v  9) (v 1) (v 10), V3 (-1)   0    1 )

	,	(XQuad (v 2) (v  8) (v 0) (v 12), V3   0    0  (-1))
	,	(XQuad (v 0) (v  8) (v 4) (v 10), V3   0  (-1)   0 )
	,	(XQuad (v 0) (v 10) (v 1) (v 12), V3 (-1)   0    0 )
	]

v :: Int -> V3 GLfloat
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

-- note that this calculation influences which faces are shared between which adjacent coordinates. it's basically arbitrary & i'm using a constants table left over from c, which is why the axes are the way they are.
lattice :: CellCoordinate -> WorldSpace
lattice (V3 x y z) =
	sum
		[	V3 x2 x2 0
		,	V3 0 y2 y2
		,	V3 z2 0 z2
		]
		where
			x2 = fromIntegral $ x * 2
			y2 = fromIntegral $ y * 2
			z2 = fromIntegral $ z * 2

-- this is some extremely compressed gaussian elimination. it's from the c version. i don't know if it works on non-grid points; i don't remember how to do gaussian elimination. - tzh 2013 06 04
unlattice :: WorldSpace -> CellCoordinate
unlattice (V3 x y z) = fmap round $ V3 ax ay az
	where
		az = l3 / 4
		ay = (l2 + az * 2) / 2
		ax = (l1 - az * 2) / 2
		l1 = x
		l2 = y - l1
		l3 = z - l2

-- return the collision faces on the first cell that matches the filter function, along a ray along the given coordinates
collide :: Int -> WorldSpace -> V3 Float -> ((Face, Face) -> Bool) -> Maybe (Face, Face)
-- `drop 1` here to discard the first collision, which is (presumably) with the starting cell and thus has a spurious front-face collison value that's actually backwards behind the start of the ray
collide l o r f = listToMaybe . filter f . drop 1 . take l $ raycast o r

isSolid :: World -> (Face, Face) -> Bool
isSolid w (Face c i s _, _) = case Map.lookup c (w ^. cells) of
	Nothing -> False
	Just (Cell Air) -> False
	Just (Cell _) -> True

isLoaded :: World -> (Face, Face) -> Bool
isLoaded w (Face c i s _, _) = case Map.lookup c (w ^. cells) of
	Nothing -> False
	_ -> True

isTargetCell :: CellCoordinate -> World -> (Face, Face) -> Bool
isTargetCell tc w (Face c _ _ _, _) = tc == c

hitVisibleFace :: Map CellCoordinate [CellFace] -> (Face, Face) -> Bool
hitVisibleFace w (Face c f _ _, _) = case Map.lookup c w of
	Nothing -> True
	Just fs -> any (\(CellFace _ cf _) -> f == cf) fs

raycast :: WorldSpace -> V3 Float -> [(Face, Face)]
raycast o r = raycast' o r $ unlattice o

-- ray origin -> ray vector -> cell to collide against this try -> [(front face hit, back face hit)]
raycast' :: WorldSpace -> V3 Float -> CellCoordinate -> [(Face, Face)]
raycast' o r c =
	case rayCollide o r c of
		Nothing -> [] {-error $
			unwords
				[	"Cast not inside cell bounds:"
				,	show o
				,	show r
				,	show c
				]-} -- our math is wrong. a mistake has led us here. alternately, the rayCollide function hit a tangent that it couldn't correct for (which is most tangents)
		Just fs@(_, b) -> fs : raycast' o r (c + adjFaceCell b)
	where
		adjFaceCell :: Face -> V3 Int
		adjFaceCell (Face _ i _ _) = snd . (faces !!) $ i
		-- a _line_ can either miss the given cell entirely (Nothing) or it can hit two faces as it passes through it. we're assuming hits through one point will never, in practice, happen (it'll error)
		rayCollide :: WorldSpace -> V3 Float -> CellCoordinate -> Maybe (Face, Face)
		rayCollide origin ray cell =
			case (mf, mb) of
				(Just f, Just b) -> Just (f, b)
				(Just f@(Face c i _ point), Nothing) ->
					-- error handling :(
					case maybe 0 (length . filter (== T)) $ turnsHit origin ray cell i of
						1 ->
							case mj origin ray cell i of
								Just j -> Just (f, Face c j Back point)
								Nothing -> error "Weird instability while error-correcting for back face."
						0 -> Nothing -- error "Tangental hit without any visible tangents on a front face :("
						_ -> Nothing -- error "Tangental hit on vertex, with only front face."
				(Nothing, Just b@(Face c i _ point)) ->
					case maybe 0 (length . filter (== T)) $ turnsHit origin ray cell i of
						1 ->
							case mj origin ray cell i of
								Just j -> Just (Face c j Front point, b)
								Nothing -> error "Weird instability while error-correcting for front face."
						0 -> Nothing -- error "Tangental hit without any visible tangents on a back face :("
						_ -> Nothing -- error "Tangental hit on vertex, with only back face."
				_ -> Nothing
			where
				mf = check Front fs
				mb = check Back fs
				check s =
					listToMaybe .
						filter (\(Face _ _ t _) -> s == t)
				fs =
					mapMaybe (quadHit origin ray cell) [0..11]
				mj o r c i = do
					{-
					... if there's ONE T then you're passing through an edge, and you can hit the sole other face that shares that edge
					... if there're TWO (adjacent) Ts then you're passing through a vertex, and you can pick any of the other faces that share that vertex
					... otherwise there's some mysterious kind of numerical instability
					-}
					t <- (=<<) (elemIndex T) $ turnsHit o r c i
					let u = (t + 1) `mod` 4
					let tv = (!! t) (points $ fst $ faces !! i)
					let uv = (!! u) (points $ fst $ faces !! i)
					listToMaybe .
						fmap fst .
							filter ((/= i) . fst) .
								filter ((== 2) . length . filter (\x -> x == tv || x == uv) . points . snd) .
									zip [0..] $
										fmap fst faces

turnsHit ::  WorldSpace -> V3 Float -> CellCoordinate -> Int -> Maybe [Turn]
turnsHit o r c i = pointInQuad' q i <$> mpoint
	where
		q@(XQuad v0 v1 v2 v3) = fmap (lattice c +) . fst $ faces !! i
		n = ns !! i
		mpoint = fmap (\t -> o + (r ^* t)) $ planeLineIntersection (quadPlane q i) o r

quadHit :: WorldSpace -> V3 Float -> CellCoordinate -> Int -> Maybe Face
quadHit o r c i =
	if fromMaybe False $ pointInQuad q i <$> mpoint
		then Face c i <$> mside <*> mpoint
		else Nothing
	where
		q@(XQuad v0 v1 v2 v3) = fmap (lattice c +) . fst $ faces !! i
		n = ns !! i
		mside = case r `dot` n of
			x	|	x < 0 -> Just Front
				|	x > 0 -> Just Back
				|	otherwise -> Nothing
		mpoint = fmap (\t -> o + (r ^* t)) $ planeLineIntersection (quadPlane q i) o r

data Turn = L | R | T
	deriving (Show, Read, Eq, Enum, Bounded)

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
pointInQuad' q@(XQuad v0 v1 v2 v3) i p = fmap (turns $ project2d p) lines
	where
		lines = zip (points flat) (rotate 1 $ points flat)
			where
				flat = fmap project2d q
				rotate n xs
					| n > ln = rotate (n - ln) xs
					| n < 0 = rotate (ln + n) xs
					| otherwise = drop n xs ++ take n xs
					where ln = length xs
		(V3 nx ny nz) = ns !! i
		project2d =
			case maximum $ fmap abs [nx, ny, nz] of
				m	|	m == abs nx -> \(V3 _ y z) -> V2 y z
					|	m == abs ny -> \(V3 x _ z) -> V2 z x
					|	m == abs nz -> \(V3 x y _) -> V2 x y

-- todo: APPARENTLY a lot of these spare `V3 a` values are unnecessary (e.g., for the plane code use a scalar distance-of-plane-from-origin value; there's apparently something similar for line representations so you don't flat-out have to use two vectors). it might be worthwhile to change that, if it comes up

-- Plane normal {world space point on the plane to use as origin} (b/c i never got the hand of the Plane (V3 a b c) d) representation)
data Plane a = Plane (V3 a) (V3 a)
	deriving (Show, Read, Eq)

-- plane (line "origin") (line ray vector)
planeLineIntersection :: (Eq a, Fractional a) => Plane a -> V3 a -> V3 a -> Maybe a
planeLineIntersection (Plane n o) l0 l1 =
	case n `dot` l1 of
		0 -> Nothing -- line parallel or coincicent with plane
		denom -> Just $ -(n `dot` l0') / denom
	where
		l0' = l0 - o

quadNormal :: (Floating a, Epsilon a) => XQuad (V3 a) -> V3 a
quadNormal (XQuad v0 v1 _ v3) = normalize $ (v1 - v0) `cross` (v3 - v0)

-- this becomes Float-specific due to looking up the normal rather than calculating it from scratch
quadPlane :: XQuad (V3 Float) -> Int -> Plane Float
quadPlane q@(XQuad v0 _ _ _) i = Plane (ns !! i) v0

ijk :: Num a => V3 a -> V3 a
ijk (V3 i j k) =
	sum [i *^ iaxis, j *^ jaxis, k *^ kaxis]
	where
		iaxis = V3 1 1 (-1)
		jaxis = V3 1 (-1) 1
		kaxis = V3 (-1) 1 1

grid :: Int -> [[a]] -> [[a]]
grid x = take x . cycle . fmap (take x . cycle)

renderPLines :: Turtle (V2 GLfloat) -> TurtleRule Bool GLfloat -> (Turtle (V2 GLfloat), IO ())
renderPLines t (Move _ s f l) =
		(	nt
		,	when (t ^. pen) $ do
				let p = t ^. position
				let np = nt ^. position
				drawTriStrip $
					linePoly 7 s f (p, np)
		)
			where
				nt = position %~ (+ (localV t $ V2 0 l)) $ t
renderPLines t (Turn x) = (nt, return ())
	where nt = direction .~ localV t (angle x) $ t
renderPLines t StackPush = (nt, return ())
	where nt = stack %~ ((t ^. position, t ^. direction) :) $ t
renderPLines t StackPop = (nt, return ())
	where
		nt = (stack %~ drop 1) . (position .~ pos) . (direction .~ dir) $ t
		(pos, dir) = fromMaybe (error "stack empty") $ listToMaybe (t ^. stack)
-- renderPLines t _ = (t, return ())

linePoly :: (Eq a, Floating a, Epsilon a) => Int -> a -> a -> (V2 a, V2 a) -> [V3 a]
linePoly sides bsize asize (V2 sx sy, V2 fx fy) = prismPoints (V3 sx sy 0) (V3 fx fy 0) sides bsize asize

-- given a vector, calculate two arbitrary-but-consistent orthogonal vectors (these probably don't establish a "proper" right-handed (or left-handed) relative coordinate system. i don't remember what order produces what direction of vector for the cross product.)
orth :: (Eq a, Floating a, Epsilon a) => V3 a -> (V3 a, V3 a)
orth base =
	if normalize base == V3 0 1 0 || normalize base == V3 0 (-1) 0
		then
			let v1 = normalize $ cross base (V3 1 0 0)
			in (v1, normalize $ cross base v1)
		else
			let v1 = normalize $ cross base (V3 0 1 0)
			in (v1, normalize $ cross base v1)

poly2d :: (Floating a) => Int -> a -> [V2 a]
poly2d sides size = take sides . iterate (pivot LSystem.^*^) $ V2 0 1 ^* size
	where
		pivot = angle $ 360 / fromIntegral sides

projectNorm :: (Eq a, Floating a, Epsilon a) => V3 a -> V2 a -> V3 a
projectNorm n (V2 x y) = x3 ^* x ^+^ y3 ^* y
	where
		(x3, y3) = orth n

prismPoints :: (Eq a, Floating a, Epsilon a) => V3 a -> V3 a -> Int -> a -> a -> [V3 a]
prismPoints base along sides bsize asize =
	(\x -> x ++ take 2 x) $
		interleave
			(fmap ((along +) . projectNorm (along - base)) $ poly2d sides asize)
			(fmap ((base +) . projectNorm (along - base)) $ poly2d sides bsize)

interleave :: [a] -> [a] -> [a]
interleave (a:as) (b:bs) = a:b:interleave as bs
interleave _ _ = []

drawQuad :: (Color4 GLfloat, GL.Normal3 GLfloat, XQuad (V3 GLfloat)) -> IO ()
drawQuad (c, n, XQuad v0 v1 v2 v3) = do
	GL.materialAmbientAndDiffuse GL.Front $= c
	GL.color c
	GL.renderPrimitive Quads $ do
		GL.normal n
		GL.vertex $ toVertex v0
		GL.vertex $ toVertex v1
		GL.vertex $ toVertex v2
		GL.vertex $ toVertex v3

drawTriStrip :: [V3 GLfloat] -> IO ()
drawTriStrip = GL.renderPrimitive TriangleStrip . sequence_ . fmap (GL.vertex . toVertex)

emitter :: Cell -> Bool
emitter (Cell ct) = (0 /=) . cellLight $ ct

update :: TChan GLFWEvent -> GameState -> IO GameState
update eventsR s = do
	as <- input eventsR s -- player input events
	bs <- return [] -- bs <- update' s (physics / in-world events)
	evaluate $ foldr actionApply s $ as ++ bs

eventAction :: GLFWEvent -> GameState -> Maybe UpdateAction
eventAction CloseEvent _ = Just GameQuit
eventAction (KeyEvent (GLFW.SpecialKey GLFW.ESC) GLFW.Press) _ = Just GameQuit
eventAction _ _ = Nothing

input :: TChan GLFWEvent -> GameState -> IO [UpdateAction]
input eventsR s = do
	GLFW.pollEvents
	evs <- atomically $ drainTChan eventsR
	sequence_ $ fmap print evs
	userControls <- runThisEveryTick
	return $ userControls ++ mapMaybe (`eventAction` s) evs

runThisEveryTick :: IO [UpdateAction]
runThisEveryTick = do
	pos <- GL.get GLFW.mousePos
	let vv = (\(GL.Position x y) ->
		V2
			((fromIntegral x - hw) / hw)
			((fromIntegral y - hh) / hh)) pos
	mv <- moveVector Keys
	-- YEAH LET'S JUST SIDE EFFECT IT ALL UP IN HERE, RESET THE MOUSE POINTER
	GLFW.mousePos $= GL.Position (round hw) (round hh)
	-- the 0.1 multiplications are to turn the normalized vectors into a mildly reasonable value :V
	return
		[	AdjustViewAngle $ vv ^* 0.1
		,	ManualImpel $ mv ^* 0.1
		]
		where
			hw = 640 / 2
			hh = 480 / 2

class MoveVector a where
	moveVector :: (Floating b, Epsilon b) => a -> IO (V2 b)

data Keys = Keys
instance MoveVector Keys where
	moveVector _ = do
		forwardMotion <- do
			f <- GLFW.getKey GLFW.UP
			b <- GLFW.getKey GLFW.DOWN
			return $ V2 0 (case (f, b) of
				(Press, _) -> -1
				(_, Press) -> 1
				_ -> 0)
		sideMotion <- do
			l <- GLFW.getKey GLFW.LEFT
			r <- GLFW.getKey GLFW.RIGHT
			return $ V2 (case (l, r) of
				(Press, _) -> -1
				(_, Press) -> 1
				_ -> 0) 0
		return . normalize $ forwardMotion ^+^ sideMotion

-- todo: i'd like to isolate this so that both rotation and movement work relative to some (varying) "up vector", so that 1. pitch stops when the front sight vector is along the up vector (i.e., the dot product is one) and 2. movement happens orthogonal to the gravity vector, along the vector that's like, the dual cross product from the view vector (i.e., up `cross` (sight `cross` up) or whichever ordering is right there)

{-
	pos->move_up.x = 0.0;
	pos->move_up.y = 1.0;
	pos->move_up.z = 0.0;
	pos->move_front = vector_cross (&pos->view_side, &pos->move_up);
	pos->move_front = vector_normalize (&pos->move_front);
	pos->move_side = vector_cross (&pos->move_up, &pos->view_front);
	pos->move_side = vector_normalize (&pos->move_side);
	pos->orientation = quat_normalize (&pos->orientation);


let gravity = {whatever}
let side_move = normalize $ gravity `cross` (side `cross` gravity)
let front_move = normalize $ gravity `cross` side_move


	pitch = (pos->orientation.y * pos->orientation.z +
		pos->orientation.w * pos->orientation.x) * 180.0;
	pitch += yrel * sensitivity;
	if (pitch < 90.0 && pitch > -90.0)
	{
		mouse_movement = euler_to_quaternion
		(
			yrel * sensitivity,
			0,
			0
		);
		// pitch is relative to the object's axes (so movement * orientation)
		pos->orientation = quat_multiply (&mouse_movement, &pos->orientation);
	}

within :: a -> (a, a) -> Boolean
within x (l, h) = x > l && x < h

if within (y * z + w * x) (-0.5, 0.5)
	then -- return rotation
	else -- do nothing
-}

actionApply :: UpdateAction -> GameState -> GameState
actionApply (AdjustViewAngle d) s = camera %~ cameraReorient d $ s
	where
		cameraReorient :: (RealFloat a) => V2 a -> Camera a -> Camera a
		cameraReorient d (Camera q p) = Camera (reorient d q) p
actionApply (ManualImpel d) s = -- update camera and optionally resort geometry
	world . storedGeometry %~
		(if unlattice op /= unlattice np
			then distanceSort
			else id) $ cus
	where
		distanceSort =
			reverse . sortBy (compare `on` distance
				(fmap fromIntegral .
					unlattice $ np) .
				fmap fromIntegral .
					fst)
		cus = camera %~ cameraMove d $ s
		(Camera _ op) = _camera s
		(Camera _ np) = _camera cus
		cameraMove :: (Floating a) => V2 a -> Camera a -> Camera a
		cameraMove (V2 x z) (Camera q p) = Camera q (p ^+^ (q ?*^ V3 x 0 z))
			where
				-- there's probably a shorter version of this calculation, but the only way i know how to do it is the quaternion -> coordinate frame math. what this _does_ is use the orientation quaternion (`q`) to establish a coordinate frame, which the given vector is assumed to be in units of that frame (e.g., if the orientation x vector is V3 0.5 1 0 then giving it a vector V3 2 0 0 results in V3 1 2 0). this means the given vector (here a movement vector) is gonna be in local units, rather than world units, so that e.g., moving left moves to the local left, rather than down the world z axis. ...sorry if this is a long-winded explanation, but it's not a _particularly_ straightforward calculation
				-- this is a 6dof calculation-- movement "forward" is straight down the view axis. this means that movement isn't constrained to any plane. there's another way to do this, which is to change the movement vector to be relative to some other plane -- the global y axis is a common one. that involves a little more quaternion / vector math.
				(?*^) :: Num a => Quaternion a -> V3 a -> V3 a
				(Quaternion w (V3 x y z)) ?*^ (V3 vx vy vz) =
					sum [vx *^ V3 sx sy sz, vy *^ V3 ux uy uz, vz *^ V3 fx fy fz]
						where
							sx = 1-2*y*y-2*z*z
							sy =   2*x*y-2*w*z
							sz =   2*x*z+2*w*y
							ux =   2*x*y+2*w*z
							uy = 1-2*x*x-2*z*z
							uz =   2*y*z-2*w*x
							fx =   2*x*z-2*w*y
							fy =   2*y*z+2*w*x
							fz = 1-2*x*x-2*y*y
actionApply GameQuit s = quit .~ True $ s


interpolate :: (RealFloat a) => Camera a -> Camera a -> a -> Camera a
interpolate (Camera aq av) (Camera bq bv) i =
	Camera
		(slerp aq bq i)
		(lerp i av bv)

render :: GameState -> TimeSpec -> IO ()
render c t = do
	GL.clear [ColorBuffer, DepthBuffer]
	GL.matrixMode $= Modelview 0
	GL.loadIdentity
	let ic = c ^. camera --interpolate (o ^. camera) (c ^. camera) 0
	(GL.matrix (Just $ Modelview 0) $=) =<< cameraPositionMatrix (c ^. camera)
	GL.translate (GL.Vector3 0.0 0.0 (-7.0) :: GL.Vector3 GLfloat)
	m <- GL.getMatrixComponents RowMajor =<< (GL.get (GL.matrix . Just $ GL.Modelview 0) :: IO (GLmatrix GLfloat))

	let thd (a, b, c) = c
	let w = c ^. world
	sequence_ .
		fmap drawQuad .
			join .
				fmap quads $
					_storedGeometry w
	GLFW.swapBuffers

glfwInit :: TChan GLFWEvent -> IO ()
glfwInit eventsW = do
	GLFW.initialize
	GLFW.disableSpecial GLFW.AutoPollEvent
	GLFW.disableSpecial GLFW.KeyRepeat
	GLFW.disableSpecial GLFW.MouseCursor
	GLFW.enableSpecial GLFW.SystemKey
	-- GLFW.enableSpecial GLFW.MouseCursor
	let w = 640
	let h = 480
	GLFW.openWindow
		(GL.Size w h)
		[	GLFW.DisplayRGBBits 4 4 3
		,	GLFW.DisplayDepthBits 8
		,	GLFW.DisplayAlphaBits 8
		]
		GLFW.Window
	GLFW.windowTitle $= "GLFW Demo"
	GL.lighting $= Disabled
	GL.blend $= GL.Enabled
	GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
	GL.cullFace $= Just GL.Back
	GL.clearColor $= Color4 0.5 0.5 0.5 0

	GL.depthFunc $= Just Lequal

	GL.matrixMode $= Projection
	initViewMatrix w h

	GLFW.keyCallback $= \key state ->
		atomically . writeTChan eventsW $ KeyEvent key state
	GLFW.charCallback $= \char state ->
		atomically . writeTChan eventsW $ CharEvent char state
	GLFW.mouseButtonCallback $= \button state ->
		atomically . writeTChan eventsW $ MouseButtonEvent button state
	GLFW.mousePosCallback $= \position ->
		atomically . writeTChan eventsW $ MousePositionEvent position
	GLFW.mouseWheelCallback $= \v ->
		atomically . writeTChan eventsW $ MouseWheelEvent v

	GLFW.windowCloseCallback $= do
		putStrLn "windowCloseCallback called"
		atomically . writeTChan eventsW $ CloseEvent
		return True
	GLFW.windowSizeCallback $= \size@(GL.Size nw nh) -> do
			putStrLn "windowSizeCallback called"
			GL.viewport   $= (GL.Position 0 0, size)
			initViewMatrix nw nh
	where
		initViewMatrix w h = do
			GL.matrixMode $= GL.Projection
			GL.loadIdentity
			GL.perspective 60.0 (fromIntegral w / fromIntegral h) 2.0 100.0
			GL.matrixMode $= Modelview 0
			GL.loadIdentity

glfwEnd :: IO ()
glfwEnd = do
	GLFW.closeWindow
	GLFW.terminate

main = do
	r <- getStdGen
	let (baseMap, _) = runRand (do
		g <- getSplit
		let (hull, g') = rect 8 8 8
			[	(WhiteCell, 1 % 1)
			] g
		let (inside, g'') = rect 6 6 6
			[	(Air, 1 % 1)
			] g'
		let (pillar, _) = rect 8 1 1
			[	(RedCell, 1 % 1)
			] g''
		return $ mconcat
			[	shift (ijk $ V3 0 5 2) pillar
			,	shift (ijk $ V3 0 5 5) pillar
			,	shift (ijk $ V3 0 2 5) pillar
			,	shift (ijk $ V3 0 2 2) pillar
			,	shift (ijk $ V3 4 4 4) $
					Map.singleton (V3 0 0 0) (Cell $ Crystal 6 (V3 0.8 0.9 1.0))
			,	shift (ijk $ V3 1 1 1) inside
			,	hull
			]
		) r
		{-
		caveCube (CaveParams
			8
			[	(Dirt, 1 % 2)
			,	(Rock, 1 % 2)
			,	(Gold, 1 % 32)
			]
			[	(Air, 31 % 32)
			,	(Crystal 6 (V3 0.8 0.9 1.0), 1 % 128)
			]
			(0.5, 0.5)
			(1, 1)) (6, 6, 6) (V3 1 1 1) r
		-}
	let distanceSort = reverse .
		sortBy (compare `on` distance
			(fmap fromIntegral .
				-- from starting camera position, currently V3 0 0 0
				unlattice $ V3 0 0 0) .
			fmap fromIntegral .
				fst) . Map.toList
	let baseWorld = World
		baseMap
		(distanceSort . (!! 1) . iterate lighting . geometry $ baseMap)
	stateVar <- newTVarIO $ GameState mempty baseWorld False
	--state'Var <- newTVarIO $ GameState mempty baseWorld False
	events <- newTChanIO
	lastUpdateVar <- newTVarIO =<<
		liftM (subtract $ TimeSpec 0 40000000) (getTime Monotonic)
	let updateThread = do
		now <- getTime Monotonic
		lastUpdate <- readTVarIO lastUpdateVar
		-- TimeSpec is in seconds + nanoseconds; threadDelay calls are in microseconds
		let updates = (now - lastUpdate) `fauxQuot` TimeSpec 0 40000000
		replicateM_ (min 13 updates) $ do
			-- okay there might be a race condition here, since the read/evaluate part happens in a non-atomic block. given that there's only one update thread i _think_ it should be fine, though? (it probably won't be) (the race condition here: read new state; some other iteration of the update thread reads state, updates state, etc, then this thread resumes and overwrites it. since all this happens on one thread, though -- neither repeat nor sequence_ should start new threads -- i don't think there's _really_ an issue. maybe. we'll see.) - tzh 2013 02 16
			state <- update events =<< readTVarIO stateVar -- new state is calculated
			{-
			state <- readTVarIO state'Var     -- get prior new state
			state' <- update events state -- new state is calculated
			-}
			updateTime <- atomically $ do
				prevUpdate <- readTVar lastUpdateVar
				let updateTime = prevUpdate + 40000000
				writeTVar stateVar state       -- prior new state becomes old state
				--writeTVar state'Var state'     -- new state is set
				writeTVar lastUpdateVar updateTime
				return updateTime
			--putStrLn $ "updated to " ++ show updateTime ++ "."
			return ()
		--state <- readTVarIO state'Var
		state <- readTVarIO stateVar
		unless (state ^. quit) $ do
			threadDelay 40000 -- in microseconds; 1/25th second; 4.0e-2
			updateThread
	let renderThread = do
		now <- getTime Monotonic
		(s, u) <- atomically $ do
			state <- readTVar stateVar
			--state' <- readTVar state'Var
			lastUpdate <- readTVar lastUpdateVar
			return (state, {-state',-} lastUpdate)
		render s $ now - u -- the `now - u` calc should ideally be somewhere between 0 and 4000000 at all times but given the nature of threading it's very much not ensured that it won't ever be above 4000000 (it will be above that all the time)
		threadDelay 8000 -- in microseconds; 1/125th second; 8.0e-3
		renderThread
	-- i'd like to be able to put glfwInit in the forkIO so all the gl calls are in one thread but 1. there will always be glfw calls in both threads and 2. update' checks the window's Opened state (to tell when it's been x'd out of) which means i guess it has to have for sure been called before the first update call? - tzh 2013 02 21
	glfwInit events
	forkIO renderThread
	updateThread
	glfwEnd

cube :: Int -> CellType -> Map CellCoordinate Cell
cube s t = fst $ rect s s s [(t, 1 % 1)] (mkStdGen 0)

randomCube :: RandomGen g => Int -> [(CellType, Rational)] -> g -> Map CellCoordinate Cell
randomCube s ts r = fst $ rect s s s ts r

weightedList :: RandomGen g => g -> [(a, Rational)] -> [a]
weightedList gen weights = evalRand m gen
	where m = sequence . repeat . fromList $ weights

shift :: CellCoordinate -> Map CellCoordinate Cell -> Map CellCoordinate Cell
shift o = Map.mapKeys (+ o)

-- todo: return the new random param by extracting a finite number of random values
rect :: RandomGen g => Int -> Int -> Int -> [(CellType, Rational)] -> g -> (Map CellCoordinate Cell, g)
rect i j k ts r =
	(,)
		(foldr (uncurry Map.insert) Map.empty $
			zip
				(sum' <$> take i iaxis <*> take j jaxis <*> take k kaxis <*> subplanes)
				(Cell <$> weightedList r ts))
		r
	where
		sum' a b c d = sum [a, b, c, d]
		iaxis = iterate (+ V3 1 1 (-1)) 0
		jaxis = iterate (+ V3 1 (-1) 1) 0
		kaxis = iterate (+ V3 (-1) 1 1) 0
		subplanes = [V3 0 0 0, V3 0 0 1, V3 0 1 0, V3 1 0 0]

data CaveParams = CaveParams
	{	bounding :: Int
	,	base :: [(CellType, Rational)]
	,	interior :: [(CellType, Rational)]
	,	subSizePercent :: (Float, Float)
	,	subCount :: (Int, Int)
	}

caveCube :: RandomGen g => CaveParams -> (Int, Int, Int) -> V3 Int -> g -> (Map CellCoordinate Cell, g)
caveCube p ijk offset r =
	first (<> hull) $ caveCube' p ijk offset r
	where
		hull = shift (V3 1 1 1 ^* negate (bounding p)) $ randomCube (bounding p * 2) (base p) r

{-
:: Int -> Int -> Int -> g -> ((Int, Int, Int), g)
i j k r = runRand (do
	i' <- getRandomR (1, i)
	j' <- getRandomR (1, j)
	k' <- getRandomR (1, k)
	return $ (i', j', k')) r
-}

caveCube' :: RandomGen g => CaveParams -> (Int, Int, Int) -> V3 Int -> g -> (Map CellCoordinate Cell, g)
caveCube' p (pi, pj, pk) offset r =
	if and . fmap (> 1) $ [i, j, k]
		then
			let (subs, rf) = runRand genSubs r''
			in
				(	shift offset $ mconcat (newrect : subs)
				,	rf
				)
		else
			(shift offset newrect, r'')
	where
		((i, j, k), r') = runRand (do
			i <- getRandomR (quot pi 2, pi)
			j <- getRandomR (quot pj 2, pj)
			k <- getRandomR (quot pk 2, pk)
			return (i, j, k)) r
		(newrect, r'') = rect i j k (interior p) r'
		genSubs :: RandomGen g => Rand g [Map CellCoordinate Cell]
		genSubs = do
			subs <- getRandomR (subCount p)
			replicateM subs $ do
				faceoffset <- face
				r <- getSplit
				return . fst $ caveCube' p (i, j, k) faceoffset r
		face :: RandomGen g => Rand g (V3 Int)
		face = do
			r <- getRandomR (0, 5) :: RandomGen g => Rand g Int
			ir <- getRandomR (0, pi - i)
			jr <- getRandomR (0, pj - j)
			kr <- getRandomR (0, pk - k)
			-- these values are such because we want two values to be random, and the third either flush along the positive edge of the prism (pi, aka "offset from the origin by size of the prism, so they touch faces at the `size` line") or the negative edge of the prism (negate i, aka "offset back from the origin by the size of this prism, so they touch faces at 0")
			-- the random values, meanwhile, are clamped so that the sub-prism, no matter its size, has an equal chance of any distribution that keeps it within the bounds of that face, so if e.g., the parent face is 6 long and the sub face is 2 long there are 6 - 2 = 4 possible positions that keep both ends of the sub face fully within the bounds of the parent face
			let offsets = case r of
				0 -> [pi, jr, kr]
				1 -> [-i, jr, kr]
				2 -> [ir, pj, kr]
				3 -> [ir, -j, kr]
				4 -> [ir, jr, pk]
				5 -> [ir, jr, -k]
			return . sum $ zipWith (^*) [iaxis, jaxis, kaxis] offsets
				where
					iaxis = V3 1 1 (-1)
					jaxis = V3 1 (-1) 1
					kaxis = V3 (-1) 1 1


clamp :: Ord a => a -> a -> a -> a
clamp lo hi = max lo . min hi

-- this is a RGBA color, for use in actually coloring the face in OpenGL.
faceColor :: CellFace -> V4 Float
faceColor (CellFace t i (V4 ir ig ib il)) =
	((* e) <$> V4 mr mg mb 0) + V4 (mr * ir) (mg * ig) (mb * ib) ma
	where
		e = clamp 0 1 $ cellLight t
		V4 mr mg mb ma = cellColor t

-- this is a RGBL color, for use in calculating more RGBL lighting values.
emittance :: CellFace -> V4 Float
emittance (CellFace t i (V4 ir ig ib il)) =
	(* e) <$> V4 mr mg mb 1 + V4 (mr * ir) (mg * ig) (mb * ib) il
	where
		e = cellLight t
		V4 mr mg mb _ = cellColor t

quads :: (CellCoordinate, [CellFace]) -> [(Color4 GLfloat, Normal3 GLfloat, XQuad (V3 GLfloat))]
quads (c, fs) = fmap quad fs
	where
		quad f@(CellFace _ i _) =
			(	toColor $ faceColor f
			,	toNormal $ ns !! i
			,	fmap (lattice c +) . fst $ faces !! i
			)

geometry :: Map CellCoordinate Cell -> Map CellCoordinate [CellFace]
geometry m = Map.mapWithKey visibleFaces m
	where
		visibleFaces :: CellCoordinate -> Cell -> [CellFace]
		visibleFaces coord c = filter visible . cellFaces $ c
			where
				visible :: CellFace -> Bool
				visible (CellFace t i _) =
					-- i.e., don't generate faces for air cells
					not (isTransparent t) &&
						-- True/False here determines if unloaded cells are considered to be solid for geometry-generation purposes. On the whole they shouldn't be (thus True) but sometimes it's useful to see just where the loaded map stops
						(not . maybe True (\(Cell t) -> isOpaque t) $ adjacent m coord i)
		cellFaces :: Cell -> [CellFace]
		cellFaces (Cell t) = CellFace t <$> [0..11] <*> pure (V4 0 0 0 0)

lighting :: Map CellCoordinate [CellFace] -> Map CellCoordinate [CellFace]
lighting cs = Map.mapWithKey update cs
	where
		lights =
			fmap (second (filter isEmitting)) .
				Map.toList .
					Map.filter (any isEmitting) $
						cs
		isEmitting = (\(V4 _ _ _ l) -> l /= 0) . emittance
		update :: CellCoordinate -> [CellFace] -> [CellFace]
		update coord = fmap faceUpdate
			where
				faceUpdate :: CellFace -> CellFace
				faceUpdate c@(CellFace t f _) = CellFace t f . sum $ totalIrradiance (coord, c)
				totalIrradiance :: (CellCoordinate, CellFace) -> [V4 Float]
				totalIrradiance = ap (liftM uncurry (lightFrom cs <$> lights)) . pure

-- idk how to really deal with how this might be lit by multiple faces of the same cell at one time. increasing the number of light sources also increases the power of the lighting. one thing to maybe see if you can include: the angle of the lit _face_ vs. the target face. right now it compares the face normal of the target with the _angle_ of the light, totally ignoring the face normal of the _source_.
lightFrom :: Map CellCoordinate [CellFace] -> (CellCoordinate, [CellFace]) -> CellCoordinate -> CellFace -> V4 Float
lightFrom w (lc, lfs) c f@(CellFace t i irradiance) =
		if lc == c -- don't bother calculating self-shadowing; it doesn't end well (there are zero-length rays when raycasting)
			then 0
			else sum $ fmap light lfs
		where
			light l@(CellFace lt li _) =
				-- cast from the light to the target (p.s. i don't think the normalization is needed)
				case collide 50 lightCentroid diff (hitVisibleFace w) of
					Nothing -> 0
					Just f@(Face hit _ _ _, _)
						|	hit == c -> d * θ *^ emittance l
						|	otherwise -> 0 {-error $ unwords
							[	show lc
							,	show c
							,	show $ normalize diff
							,	show lightCentroid
							,	show targetCentroid
							,	show f
							,	show $ Map.lookup hit w
							]-}
				where
					lightCentroid = (lattice lc +) . centroid . points . fst $ faces !! li
					-- ignore light hits on the back of the face (where the dot product is positive)
					θ = (min 1 . abs . min 0) $ n `dot` normalize diff
					diff = targetCentroid - lightCentroid -- from the light, towards the target
					-- 4 here is a world space -> coord space lighting transform constant (since each rhodec is 4~ wide)
					d = 4 * (1 / (dist * dist))
					dist = distance targetCentroid lightCentroid
			n = ns !! i
			targetCentroid = (lattice c +) . centroid . points . fst $ faces !! i
