module Material
	(	Material (..)
	,	color
	,	light
	,	variance
	,	push
	,	isOpaque
	,	isTransparent
	,	isTranslucent
	) where

import Linear.V3
import Linear.V4

data Material
	= Void
	| Air
	| Dirt
	| Clay
	| Rock
	| Crystal Float (V3 Float)
	| Wood
	| Fibre
	| Gold
	| WhiteCell | RedCell
	deriving (Show, Read, Eq, Ord)

color :: Material -> V4 Float
color Air = V4 0 0 0 0
color Dirt = V4 0.6 0.4 0.1 1.0
color Clay = V4 1.0 0.9 0.8 1.0
color Rock = V4 0.7 0.6 0.5 1.0
color (Crystal _ (V3 r g b)) = V4 r g b 0.4
color Wood = V4 0.5 0.5 0.2 1.0
color Fibre = V4 0.9 1.0 0.9 1.0
color Gold = V4 0.9 0.8 0.4 1.0
color WhiteCell = V4 1.0 1.0 1.0 1.0
color RedCell = V4 1.0 0.0 0.0 1.0

light :: Material -> Float
light x = case x of
	Crystal i _ -> i
	_ -> 0


push :: Material -> Float
push Air = 0.5
push Dirt = 0.7
push Clay = 0.75
push Rock = 0.8
push (Crystal _ _) = 1
push Wood = 0.6
push Fibre = 0.5
push Gold = 0.9
push _ = 1

variance :: Material -> Float
variance Air = 0.25
variance Dirt = 0.25
variance Clay = 0.25
variance Rock = 0.1
variance (Crystal _ _) = 0.1
variance Wood = 0.25
variance Fibre = 0.3
variance Gold = 0.1
variance _ = 0

isOpaque :: Material -> Bool
isOpaque t = case color t of
	(V4 _ _ _ a)
		|	a < 1.0 -> False
		|	otherwise -> True

isTransparent :: Material -> Bool
isTransparent t = case color t of
	(V4 _ _ _ a)
		|	a <= 0.0 -> True
		|	otherwise -> False

isTranslucent :: Material -> Bool
isTranslucent t = case color t of
	(V4 _ _ _ a)
		| a >= 1.0 -> False
		| otherwise -> True
