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
    * something like 1/4th of the total lighting time/memory is taken up by
      calls to `turns` and `pointInQuad`
    * also lighting is horrifically slow (as one might expect)
    * consider doing a flood rayout out from each lit face instead of having
      every single visible face in the world raycast to every light
  * GEOMETRY
    * subdivide visible cells (from 4 pts to 9 pts, at least) and deform each point based on the surrounding cell types (currently this happens but only for 4pt quads, whereas i'd like to change the fundamental geometry to a radial triangle fan)
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
import Data.List (sortBy, group, groupBy, mapAccumR, transpose, elemIndex, genericLength)
import Data.Maybe (catMaybes, mapMaybe, catMaybes, isNothing, fromMaybe, listToMaybe)
import Data.Ratio ((%))
import Data.Monoid
import Data.Map (Map)
import qualified Data.Map as Map

import Graphics.Rendering.OpenGL (($=), GLfloat, GLubyte, GLmatrix(..), PrimitiveMode(..), ClearBuffer(..), ComparisonFunction(..), Capability(Enabled, Disabled), MatrixMode(..), Color4(..), Light(..), Vertex4(..), MatrixComponent(..), MatrixOrder(RowMajor))
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW (Key(..), KeyButtonState (Release, Press))
import qualified Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL.GL.VertexSpec (Normal3(..))

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
import Lattice

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

ijk :: Num a => V3 a -> V3 a
ijk (V3 i j k) =
	sum [i *^ iaxis, j *^ jaxis, k *^ kaxis]
	where
		iaxis = V3 1 1 (-1)
		jaxis = V3 1 (-1) 1
		kaxis = V3 (-1) 1 1

toVertex :: V3 a -> GL.Vertex3 a
toVertex (V3 x y z) = GL.Vertex3 x y z

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

-- the point of this: to have a place to wedge in a controller-based move scheme later
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

-- todo: i'd like to isolate this so that both rotation and movement work relative to some (varying) "up vector", so that 1. pitch stops when the front sight vector is along the up vector (i.e., the dot product is one or negative one) and 2. movement happens orthogonal to the gravity vector, along the vector that's like, the dual cross product from the view vector (i.e., up `cross` (sight `cross` up) or whichever ordering is right there)

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

distanceSort :: Integral a => WorldSpace -> [(V3 a, b)] -> [(V3 a, b)]
distanceSort p =
	reverse . sortBy (compare `on` distance
		(fmap fromIntegral .
			unlattice $ p) .
		fmap fromIntegral .
			fst)

actionApply :: UpdateAction -> GameState -> GameState
actionApply (AdjustViewAngle d) s = camera %~ cameraReorient d $ s
	where
		cameraReorient :: (RealFloat a) => V2 a -> Camera a -> Camera a
		cameraReorient d (Camera q p) = Camera (reorient d q) p
actionApply (ManualImpel d) s = -- update camera and optionally resort geometry
	world . storedGeometry %~
		(if unlattice op /= unlattice np
			then distanceSort np
			else id) $ cus
	where
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

{-
interpolate :: (RealFloat a) => Camera a -> Camera a -> a -> Camera a
interpolate (Camera aq av) (Camera bq bv) i =
	Camera
		(slerp aq bq i)
		(lerp i av bv)
-}

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
				fmap (quads $ w ^. cells) $
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
	GL.clearColor $= Color4 0.6 1.0 0.0 0

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
	let (baseMap, _) = genWorld r
	-- from starting camera position, currently V3 0 0 0
	let baseWorld = World
		baseMap
		(distanceSort (V3 0 0 0) . Map.toList . (!! 1) . iterate lighting . geometry $ baseMap)
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
	where
		genWorld = runRand $ do
			g <- getSplit
			let (hull, g') = rect 8 8 8
				[	(Dirt, 1 % 2)
				,	(Rock, 1 % 2)
				,	(Gold, 1 % 32)
				] g
			let (inside, g'') = rect 6 6 6
				[	(Air, 1 % 1)
				] g'
			let (pillar, _) = rect 8 1 1
				[	(RedCell, 1 % 3)
				,	(WhiteCell, 2 % 3)
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
