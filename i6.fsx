type point = int * int // a point (x, y) in the plane
type colour = int * int * int // (red, green, blue), 0..255 each
type figure =
| Circle of point * int * colour
// defined by center, radius, and colour
| Rectangle of point * point * colour
// defined by bottom-left corner, top-right corner, and colour
| Mix of figure * figure
// combine figures with mixed colour at overlap

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

let colouring (x,y) figure =
    match (colourAt (x,y) figure) with
    | Some (r,g,b) ->
        (r,g,b)
    | None ->
        (255,255,255)
let circle = Circle((50,50), 45, (255,0,0))
let square = Rectangle((40,40), (90,110), (0,0,255))
let mix = Mix(circle, square)

makeBMP.makeBMP "test" 100 120 (fun (x,y) -> colouring (x,y) mix)
