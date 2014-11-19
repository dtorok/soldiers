import Debug
import Mouse

-- types
data Point = Point Float Float
data Line = Line Float Float
data Segment = Segment Point Point Float Float  -- p0 p1 m length

-- type constructors
distance : Point -> Point -> Float
distance (Point x0 y0) (Point x1 y1) = 
    let dx = x1 - x0
        dy = y1 - y0
    in sqrt(dx*dx + dy*dy)

seg : Point -> Point -> Segment
seg p0 p1 = 
    case p0 of Point x0 y0 ->
    case p1 of Point x1 y1 ->
        let m = (y1 - y0) / (x1 - x0)
            len = distance p0 p1
        in Segment p0 p1 m len

lin : Point -> Float -> Line
lin (Point x y) m = 
    let y0 = y - x * m
    in Line y0 m

linY : Line -> Float -> Float
linY (Line y0 m) x = y0 + x * m

seg2lin : Segment -> Line
seg2lin (Segment p0 _ m _) = lin p0 m

-- helpers
filterMap : (a -> Maybe b) -> [a] -> [b]
filterMap f aa = 
    let mapper a = case (f a) of
        Just x -> [x]
        Nothing -> []
    in concat <| map mapper aa

-- 2d algebra

around : Float -> Float -> Bool
around a b = 
    let d = a - b
        ε = 0.000001
    in (d < ε) && (d > (-ε))

between : Float -> Float -> Float -> Bool
between a x b = 
    let a' = min a b
        b' = max a b
    in b' - x <= b' - a'

isOnLeft : Point -> Segment -> Bool
isOnLeft p v =
    case v of Segment (Point x0 y0) (Point x1 y1) _ _ ->
    case p of Point x y -> 
        let val = (x1 - x0) * (y - y0) - (y1 - y0) * (x - x0)
        in val < 0

polygon2segments : [Point] -> [Segment]
polygon2segments ps = case ps of
    [] -> []
    (x::xs) -> zipWith seg ps (xs ++ [x])

polygonContains : [Point] -> Point -> Bool
polygonContains ps p = 
    let 
        sides = polygon2segments ps
        onLeftBools = map (isOnLeft p) sides
        onRights = filter (not . id) onLeftBools
    in 
        length onRights == 0

linIntersect : Line -> Line -> Point
linIntersect (Line y0 m0) (Line y1 m1) = 
    let x = (y1 - y0) / (m0 - m1)
        y = linY (Line y0 m0) x
    in Point x y

pointOnSegment : Segment -> Point -> Bool
pointOnSegment s p = 
    case p of Point x y -> 
    case s of Segment (Point x0 y0) (Point x1 y1) _ _ ->
        let l = seg2lin s
            y' = linY l x
            ons = (between x0 x x1) && (between y0 y y1)
        in ons  && (y `around` y')

segIntersect : Segment -> Segment -> Maybe Point
segIntersect s1 s2 = 
    let l1 = seg2lin s1
        l2 = seg2lin s2
        p = linIntersect l1 l2
        pOns1 = pointOnSegment s1 p
        pOns2 = pointOnSegment s2 p
    in if pOns1 && pOns2 then Just p else Nothing

segAngle : Segment -> Segment -> Float
segAngle s0 s1 = 
    case s0 of Segment _ p01 _ l0 ->
    case s1 of Segment _ p11 _ l1 ->
        let (Segment _ _ _ l2) = seg p01 p11
            tmp = (l0*l0 + l1*l1 - l2*l2) / (2*l0*l1)
        in acos tmp

distance : Point -> Point -> Float
distance (Point x0 y0) (Point x1 y1) = 
    let dx = x1 - x0
    let dy = y1 - y0
    in sqrt (dx*dx + dy*dy)


-- calculate line segments
-- 1. go line by line
-- 2. check if all is inside -> points added
-- 3. check if A is inside -> count intersection -> (A, B')
-- 4. check if B is inside -> count intersection -> (A', B)


-- rendering

-- take all the lines
-- count intersection with the sight triangle
-- if there is 2 of them, 
    -- then ok
-- if there is only one, then
    -- find which one is inside of the triangle
    -- create a line of 


p2t : Point -> (Float, Float)
p2t p = case p of
    Point x y -> (x, y)

renderLine : Line -> Form
renderLine l =
    let p0 = Point (-999) (linY l (-999))
        p1 = Point (1000) (linY l (1000))
        s = seg p0 p1
    in renderSegment s

renderSegment : Segment -> Form
renderSegment (Segment p0 p1 _ _) = 
    [p2t p0, p2t p1] |> polygon
                     |> outlined defaultLine

renderSightView : [Point] -> Form
renderSightView ps = ps |> map p2t 
                        |> polygon 
                        |> filled (rgba 0 0 255 0.7)

renderPoint : Point -> Bool -> Form
renderPoint p marked = 
    let f = if marked then filled
                      else outlined . dotted
    in case p of 
        Point x y -> circle 8.0 |> f (rgba 0 0 0 1)
                                 |> move (x, y)

renderWall : Segment -> Form
renderWall w = case w of
    Segment p0 p1 _ _ -> [p2t p0, p2t p1] |> polygon
                                          |> outlined defaultLine

--main = testPolygonContains <~ Mouse.position
main = tests


------ tests

tests = collage 800 800 
                [ move (-150, -30) testView
                --, move (-200, -200) testLineIntesection
                --, move (50, 100) testSegmentIntesection
                ]

reduceWall : [Point] -> Segment -> [Segment]
reduceWall ps s = case s of
    Segment p0 p1 _ _ -> 
        let p0in = polygonContains ps p0
            p1in = polygonContains ps p1
        in if p0in && p1in then
            [s]
        else 
            let sides = polygon2segments ps
                intersects = filterMap (segIntersect s) sides
                ss = polygon2segments <| case intersects of
                        [] -> []
                        [p] -> if p0in then [p0, p] else [p1, p]
                        otherwise -> intersects
            in ss

testView : Form
testView = 
    let walls = [ seg (Point 0 100) (Point 30 100)
                , seg (Point -50 0) (Point 0 400)
                , seg (Point 20 280) (Point 150 50)
                , seg (Point 0 -100) (Point 30 -100)
                ]
        view = [ Point 0 0
               , Point -130 300
               , Point 130 300
               ]

        reducedWalls = concat <| map (reduceWall view) walls
        --tmp = Debug.log "angle: " (segAngle 
        --                            (seg (Point 0 0) (Point -130 300))
        --                            (seg (Point 0 0) (Point 130 300)))
        --marked = polygonContains pol p
        --renderPoint p marked :: 
    in
        group ([renderSightView view] ++ (map renderWall reducedWalls))

testSegmentIntesection : Form
testSegmentIntesection =
    let s0 = seg (Point 0 0) (Point 100 100)
        s1 = seg (Point 120 0) (Point 0 100)
        s2 = seg (Point 50 20) (Point 150 20)
        p01 = segIntersect s0 s1
        p02 = segIntersect s0 s2
        p12 = segIntersect s1 s2
        rendered p = case p of
            Just p' -> [renderPoint p' True]
            otherwise -> []
    in group <| [ renderSegment s0
                , renderSegment s1
                , renderSegment s2] 
                ++ (rendered p01) 
                ++ (rendered p02) 
                ++ (rendered p12)

testLineIntesection : Form
testLineIntesection =
    let l0 = Line 0 0.5
        l1 = Line 60 (-2)
        p = linIntersect l0 l1
    in group [ renderLine l0
             , renderLine l1
             , renderPoint (Point -50 (linY l0 -50)) False
             , renderPoint (Point 150 (linY l0 150)) False
             , renderPoint (Point -50 (linY l1 -50)) False
             , renderPoint (Point 150 (linY l1 150)) False
             , renderPoint p True]

testPolygonContains : (Int, Int) -> Element
testPolygonContains (x, y) = 
    let p = Point (toFloat x-400) (toFloat (400 - y))
        pol = [Point 0 0, Point -120 300, Point 120 300]
        marked = polygonContains pol p
    in
        collage 800 800 (renderSightView pol :: renderPoint p marked :: [])
