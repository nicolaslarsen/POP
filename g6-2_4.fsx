type point = int * int
type colour = int * int * int

type figure =
    | Circle of point * int * colour
    // defined by center, radius, and colour
    | Rectangle of point * point * colour
    // defined by bottom-left corner, top-right corner, and colour
    | Mix of figure * figure
    // combine figures with mixed colour at overlap
    | Twice of figure * (int * int)

// finds colour of figure at point
let rec colourAt (x,y) figure =
    match figure with
    | Circle ((cx,cy), r, col) ->
        if (x-cx)*(x-cx)+(y-cy)*(y-cy) <= r*r
        // bruger Pythagoras sætning til at finde afstand til centrum
        then Some col else None
    | Rectangle ((x0,y0), (x1,y1), col) ->
        if x0<=x && x <= x1 && y0 <= y && y <= y1 // indenfor hjørnerne
        then Some col else None
    | Mix (f1, f2) ->
        match (colourAt (x,y) f1, colourAt (x,y) f2) with
        | (None, c) -> c // overlapper ikke
        | (c, None) -> c // ditto
        | (Some (r1,g1,b1), Some (r2,g2,b2)) ->
            Some ((r1+r2)/2, (g1+g2)/2, (b1+b2)/2) // gennemsnitsfarve    
    // g6.3
    | Twice (f,(x1,y1)) ->
        match (colourAt (x,y) f, colourAt (x-x1,y-y1) f) with
        | (None, c) -> c // overlapper ikke
        | (c, None) -> c // ditto
        | (c1, c2) -> c2 // use colour of the second

/// <summary> Uses colourAt to return the colour of a point,
/// given a figure</summary>
/// <params name="figure"> A figure to pass along with the coordinates</params>
/// <params name="x"> The x coordinate for colourAt to use</params>
/// <params name="y"> The y coordinate for colourAt to use</params>
/// <returns> The colour of the figure if (x,y) is contained in it.
/// otherwise returns a grey colour </returns>
let draw figure (x,y) =
    match (colourAt (x,y) figure ) with
    | Some (r,g,b) -> (r,g,b)
    | None -> (128,128,128) // Grey

// g6.2
let c = Circle ((50,50),45,(255,0,0))
let b = Rectangle ((40,40),(90,110),(0,0,255))
let g61 = Twice((Mix (b,c)),(50,70))

// g6.4
makeBMP.makeBMP "g63" 150 200  (draw g61)
