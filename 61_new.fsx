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
    | Twice (f,(x1,y1)) ->
        match (colourAt (x,y) f, colourAt (x-x1,y-y1) f) with
        | (None, c) -> c // overlapper ikke
        | (c, None) -> c // ditto
        | (c1, c2) -> c2 // use colour of the second
  

let rec checkFigure figure = 
    let checkColour (r,g,b) =
        if 0 <= r && r <= 255 && 0 <= g && g <= 255 && 0 <= b && b <= 255 then
            // check if all r, g, and b are between 0 and 255
            true
        else    
            false
    match figure with
    | Circle ((cx,cy), r, col) ->
        if r >= 0 && (checkColour col) then
            //check if r is not negative and check color
            true
        else 
            false
    | Rectangle ((x0,y0), (x1,y1), col) ->
        if x0 <= x1 && y0 <= y1 && (checkColour col) then
            //check if x0 and y0, does not come before x1 and y1. and check color.
            true
        else
            false
    | Mix (f1,f2) ->
        if (checkFigure f1) && (checkFigure f2) then
            //if both figures, check separately, are true then the combined figure have to be true
            true
        else
            false
    | Twice (f,(x1,y1)) ->
        //only need to check if that figure is true
        checkFigure f
    
let rec boundingBox figure =
    let comparePoints a b =
        //helper to check two sets of points
        let rec highest v1 v2 =
            //helper to check which number is higher
            if v1 > v2 then
                v1
            else 
                v2
        let ((f1x1,f1y1),(f1x2,f1y2)) = a
        let ((f2x1,f2y1),(f2x2,f2y2)) = b
        //break the dobbelt tuple into separate componets.
        
        let lowPoint = (-(highest -f1x1 -f2x1),-(highest -f1y1 -f2y1))
        //using highest to find the lowest of the to points
        //by using the negative of highest, with negativ inputs, it will return the number closest to zero
        
        let highPoint = ((highest f1x2 f2x2),(highest f1y2 f2y2))
        //use highest to find the highest of the two... dah
        
        (lowPoint,highPoint)
        
    match figure with
    | Circle ((cx,cy), r, col) -> 
        ((cx-r,cy-r),(cx+r,cy+r))
        //subtract the radius from the center, to get the lowest point, and add the radius to find the highest
        
    | Rectangle ((x0,y0), (x1,y1), col) ->
        ((x0,y0),(x1,y1))
        //simply use the two points given when creating a rectangle
        
    | Mix (f1,f2) ->
        comparePoints (boundingBox f1) (boundingBox f2)
        //get the bounding box of the two, and compare those two.
        
    | Twice (f, (x1,y1)) ->
        let ((f1x1,f1y1),(f1x2,f1y2)) = (boundingBox f)
        comparePoints ((f1x1,f1y1),(f1x2,f1y2)) ((f1x1+x1,f1y1+y1),(f1x2+x1,f1y2+y1))
        //same as Mix, but instead the offset is added to the second
                
let draw figure (x,y) =
    match (colourAt (x,y) figure ) with
    | Some (r,g,b) -> (r,g,b)
    
    | None -> (128,128,128)

let c = Circle ((50,50),45,(255,0,0))
let b = Rectangle ((40,40),(90,110),(0,0,255))
    
makeBMP.makeBMP "o61" 256 256  (draw (Twice ((Mix (b,c)),(50,70))))


let fb = Rectangle ((60,60),(20,20),(-5,300,-8))
printfn "%A" (checkFigure((Twice ((Mix (b,c)),(50,70)))))
printfn "%A" (boundingBox((Twice ((Mix (b,c)),(50,70)))))


type weekday = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday

let dayToNumber = function
    | Monday -> 1
    | Tuesday -> 2
    | Wednesday -> 3
    | Thursday -> 4
    | Friday -> 5
    | Saturday -> 6
    | Sunday -> 7


let NumberToDay = function
    | 1 -> Some Monday
    | 2 -> Some Tuesday
    | 3 -> Some Wednesday
    | 4 -> Some Thursday
    | 5 -> Some Friday
    | 6 -> Some Saturday
    | 7 -> Some Sunday
    | _ -> None

printfn "%A" (Option.get(NumberToDay 5))
