module Shapes where

data Point = Point Float Float

class Moveable a where
    nudge :: a -> Point -> a

instance Moveable Point where
    nudge (Point x y) (Point x' y') = Point (x+x') (y+y')


-- shapes

class Shape a where
    surface :: a -> Float

data Rect = Rect Point Point

instance Moveable Rect where
    nudge (Rect p1 p2) p' = Rect (nudge p1 p') (nudge p2 p')

instance Shape Rect where
    surface (Rect p1 p2) = abs (px p1 - px p2) * abs (py p1 * py p2)
        where
            px (Point x _) = x
            py (Point _ y) = y

data Circ = Circ Point Float

instance Moveable Circ where
    nudge (Circ p r) p' = Circ (nudge p p') r

instance Shape Circ where
    surface (Circ _ r) = r*r * pi

fig1 = Rect (Point 2.0 2.0) (Point 5.0 5.0)
fig2 = Circ (Point 1.5 2.5) 5

testShapes :: IO ()
testShapes = do
    print $ surface fig1
    print $ surface fig2
