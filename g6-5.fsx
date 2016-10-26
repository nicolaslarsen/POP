(* g6.5. *)
printfn "               g6.5."

//types
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
    
///<summary>
///Funktionen checkFigure kan se om forholdene for en figur passer.
///F.eks. ville det ikke være muligt at lave cirkel med negativ radius.
///Den tjekker farven for alle figure og individuelle forhold.
///Funktionen kan tjekke en cirkel, rektangel, mix og twice.
///</summary>
///<params name="figure">
///figure er en type som kan indeholde en cirkel, rektangel, mix eller twice.
///</params>
///<returns>
///Funktionen returnere true, hvis forholdene er rigtige og false hvis ikke.
///</returns>

let rec checkFigure figure = 
    let checkColour (r,g,b) =
        (0 <= r && r <= 255 && 0 <= g && g <= 255 && 0 <= b && b <= 255)
        // check if all r, g, and b are between 0 and 255

    match figure with
    | Circle ((cx,cy), r, col) ->
        (r >= 0 && (checkColour col))
        //check if r is not negative and check color
        
    | Rectangle ((x0,y0), (x1,y1), col) ->
        (x0 <= x1 && y0 <= y1 && (checkColour col))
        //check if x0 and y0, does not come before x1 and y1. and check color.
        
    | Mix (f1,f2) ->
        ((checkFigure f1) && (checkFigure f2))
        //if both figures, checked separately, are true then the combined figure have to be true
        
    | Twice (f,(x1,y1)) ->
        checkFigure f
        //only need to check if that figure is true

printfn "               checkFigure"
//tests
let falseRect = Rectangle((50,30),(10,10),(255,300,-100))
let falseCirc = Circle((45,45),-25,(0,300,-255))
let rectangle = Rectangle((10,10),(50,30),(255,0,0))
let circle = Circle((45,45),25,(0,0,255))

printfn "Test1: %b" ((checkFigure circle) = true)
printfn "Test2: %b" ((checkFigure rectangle) = true)
printfn "Test3: %b" ((checkFigure (Mix (circle,rectangle))) = true)
printfn "Test4: %b" ((checkFigure (Twice(circle,(50,50)))) = true)

printfn "Test5: %b" ((checkFigure falseCirc) = false)
printfn "Test6: %b" ((checkFigure falseRect) = false)
printfn "Test7: %b" ((checkFigure (Mix (falseCirc,falseRect))) = false)
printfn "Test8: %b" ((checkFigure (Twice(falseCirc,(50,50)))) = false)
        
///<summary>
///Funktionen boundingBox finder afgrænsningsramme for en givet figur,
///Funktionen kan tjekke en cirkel, rektangel, mix og twice.
///</summary>
///<params name="figure">
///figure er en type som kan indeholde en cirkel, rektangel, mix eller twice.
///</params>
///<returns>
///Funktionen returnere to set koordinater.
///Den første er det nederste venstre hjørne og den anden
///er øverst højre hjørne.
///</returns>        
    
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
        
printfn "               boundingBox"
//tests
printfn "Test1: %b" ((boundingBox circle) = ((20,20),(70,70)))
printfn "Test2: %b" ((boundingBox rectangle) = ((10,10),(50,30)))
printfn "Test3: %b" ((boundingBox (Mix (circle,rectangle))) = ((10,10),(70,70)))
printfn "Test4: %b" ((boundingBox (Twice(circle,(50,50)))) = ((20,20),(120,120)))
