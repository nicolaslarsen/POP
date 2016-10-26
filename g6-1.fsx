(* g6.1. *)

(* Kommentarer til løsningen af denne opgave skrives her *)

///<summary>
///numberToDay giver en weekday option på en dag, fra Monday til Sunday 
///Ved at få nummeret på dagen. F.eks 3 giver Wednesday.
///</summary>
///<params name="n">
///Selv om den ikke er skrevet i funktionen, er n variablen 
///som bliver "match" med de 7 dage og den sidste None, 
///hvis den ikke passer med nogen af dem.
///</params>
///<returns>
///Funktionen returnere weekday option. Some [Monday...Sunday] og 
///None hvis den ikke findes
///</returns>

type weekday = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
    
let numberToDay = function
    | 1 -> Some Monday
    | 2 -> Some Tuesday
    | 3 -> Some Wednesday
    | 4 -> Some Thursday
    | 5 -> Some Friday
    | 6 -> Some Saturday
    | 7 -> Some Sunday
    | _ -> None

let dayToNumber = function
    | Monday -> 1
    | Tuesday -> 2
    | Wednesday -> 3
    | Thursday -> 4
    | Friday -> 5
    | Saturday -> 6
    | Sunday -> 7
//dayToNumber bliver brugt til test   
    
printfn "               g6.1."
printfn "Test1: %b" (Option.get(numberToDay 4) = Thursday)
printfn "Test2: %b" (Option.get(numberToDay 7) = Sunday)
printfn "Test3: %b" (numberToDay 8 = None)
printfn "Test4: %b" (dayToNumber(Option.get(numberToDay 4)) = 4)
