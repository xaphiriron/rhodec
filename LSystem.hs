{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

-- todo: modules
module LSystem where

import Control.Arrow ((&&&))
import Control.Applicative (pure)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Monoid (Monoid(..), mempty)
import Data.List (mapAccumR)

import Control.Lens

import Linear.V2
import Linear.Vector
import Linear.Metric
import Linear.Epsilon

-- types: expansion rules, render commands, render state
data LSystem e r s = LSystem (e -> [e]) (s -> e -> (s, r))

data Turtle a = Turtle
	{	_position :: a
	,	_direction :: a
	,	_pen :: Bool
	,	_stack :: [(a, a)]
	}
	deriving (Show)

makeLenses ''Turtle

-- Turtle (getSum mempty) (getSum mempty) False []

class Multiplicative a where
	(^*^) :: a -> a -> a

instance (Num a) => Multiplicative (V2 a) where
	(^*^) (V2 x y) base = (x *^ perp base) ^+^ (y *^ base)


combExpand :: Char -> [Char]
combExpand 'F' = "F^^BvF"
combExpand 'B' = "BBB"
combExpand i = pure i

kochExpand :: Char -> [Char]
kochExpand 'F' = "F-F++F-F"
kochExpand i = pure i

sierpinskiExpand :: Char -> [Char]
sierpinskiExpand 'A' = "B-A-B"
sierpinskiExpand 'B' = "A+B+A"
sierpinskiExpand i = pure i

sierpinski2Expand :: Char -> [Char]
sierpinski2Expand 'F' = "F-G+F+G-F"
sierpinski2Expand 'G' = "GG"
sierpinski2Expand i = pure i

dragonExpand :: Char -> [Char]
dragonExpand 'X' = "X+YF"
dragonExpand 'Y' = "FX-Y"
dragonExpand i = pure i

bExpand :: Char -> [Char]
bExpand 'F' = "FF"
bExpand 'B' = "F[-B]+B"
bExpand x = pure x

renderText :: a -> Char -> (a, Char)
renderText = (,)

angle :: (Floating a) => a -> V2 a
angle deg = uncurry V2 . (sin &&& cos) $ deg / 180 * pi

renderLines :: (Floating a, Epsilon a) => a -> Turtle (V2 a) -> Char -> (Turtle (V2 a), Maybe (V2 a, V2 a))
renderLines _ t x
	|	x `elem` "FGAB" =
		(	nt
		,	if t ^. pen
				then Just (t ^. position, nt ^. position)
				else Nothing
		)
			where
				nt = position %~ (+ (localV t $ V2 0 0.3)) $ t
renderLines a t '-' = (nt, Nothing)
	where nt = direction .~ (localV t $ angle (-a)) $ t
renderLines a t '+' = (nt, Nothing)
	where nt = direction .~ (localV t $ angle a) $ t
renderLines _ t '[' = (nt, Nothing)
	where nt = stack %~ ((t ^. position, t ^. direction) :) $ t
renderLines _ t ']' = (nt, Nothing)
	where
		nt = (stack %~ drop 1) . (position .~ pos) . (direction .~ dir) $ t
		(pos, dir) = fromMaybe (error "stack empty") $ listToMaybe (t ^. stack)
renderLines _ t '^' = ((pen .~ False) $ t, Nothing)
renderLines _ t 'v' = ((pen .~ True) $ t, Nothing)
renderLines _ t _ = (t, Nothing)

localV :: (Multiplicative a) => Turtle a -> a -> a
localV t = (^*^ (t ^. direction))
{-

catMaybes . snd . mapAccumR renderLines (Turtle 0 (V2 1 0) True []) . reverse
$
head . drop 3 . iterate ((=<<) bExpand) $ "B"

class (LSystem e r s) a where
	expand :: a -> e -> [e]
	render :: a -> s -> e -> (s, r)


--drawExpansion :: ((LSystem e r s) a) => a -> [e] -> [e]
drawExpansion ls s = (=<<) (expand ls) $ s
-}


data TurtleRule b a
	=	Turn a
	|	Move b a a a
	|	StackPush
	|	StackPop
	deriving (Eq, Show, Read)

btExpand :: (Num a) => TurtleRule Bool a -> [TurtleRule Bool a]
btExpand (Move True s f l) = pure $ Move True (s*2) (f*2) (l*2)
btExpand (Move False s f l) =
	[	Move True s f l
	,	StackPush
	,	Turn (-45)
	,	Move False s f l
	,	StackPop
	,	Turn (45)
	,	Move False s f l
	]
btExpand x = pure x


{-
kochExpand :: Fractional a => TurtleRule a -> [TurtleRule a]
kochExpand (Move x) = [m3, tl, m3, tr, m3, tl, m3]
	where
		m3 = Move $ x / 3
		tl = Turn (-60)
		tr = Turn 60
kochExpand i = pure i




liftAccum :: (a -> b -> b) -> a -> b -> (a, b)
liftAccum f a b = (a, f a b)

lineDraw :: (Num a) => TurtleInfo a -> TurtleRule a -> (TurtleInfo a, Maybe (V2 a, V2 a))
lineDraw t (Move x) =
	let
		nt = position %~ (+) x * (t ^. direction) $ t
	in
		(nt, Just $ (t ^. position, nt ^. position))
lineDraw t (Turn x) =
	(direction %~ (* x) $ t, Nothing)
lineDraw t _ = (t, Nothing)
-}
