(* Opgave 4g
 *
 * Group:
 *   Mathias Friis Rasmussen
 *   Nicolas Ringsmose Larsen
 *   
 *)


(* 4g.1 *)
printfn "             OPGAVE 4g.1"
(*HR 4.11 *)
(* 1 *)

(*Kommentarer til løsningen af denne opgave skrives her *)

///<summary>
///Denne funktion tæller antallet af gange en bestemt værdi dukker op i liste af samme type.
///Den gør dette ved at tjekke hvert element i listen igennem rekursivt og hvis
///elementet har samme værdi som x, bliver der lagt 1 til, hvorefter den køre 
///igen med resten af listen, indtil den er tom.
///</summary>
///<params name="A">
///Denne parameter indholder en list som skal tjekkes igennem.
///</params>
///<params name="x">
///Denne parameter er værdien der ledes efter i listen.
///</params>
///<returns>
///Funktionen returnere et heltal som er antallet af gange værdien dukkede op.
///</returns>

let rec count A x =
    //counts the number of times x appers in A
    match A with
    | [] -> 0
    | x1::xs -> 
        if (x1 = x) then
            1 + (count xs x)
        else
            (count xs x)

printfn "               HR 4.11(1)"
printfn "Test1: %b" ((count [1] 1) = 1) //make your tests here
printfn "Test2: %b" ((count [1;2;5;2] 2) = 2)
printfn "Test3: %b" ((count [7;7;7;9] 9) = 1)
printfn "Test4: %b" ((count [6;8;4;1;2;5;8;6] 8) = 2)
printfn "Test5: %b" ((count [7;6;4;8;4;4] 4) = 3)
printfn "Test6: %b" ((count [6;6;6;6;6;6;6] 6) = 7)

(* 2 *)

(* Kommentarer til løsningen af denne opgave skrives her *)

///<summary>
///Denne funktion indsætter et element i en sorteret liste, 
///så den stadig er sorteret.
///Den gør dette ved at tjekke om hvert element er større end det
///den er kommet til, hvis den er bliver det nye element indsat før, det den 
///er kommet til, og hvis ikke bliver elementet i listen sat ind.
///Den tjekker også om det sidste element i listen er mindre end, det der
///der skal indsættes, og hvis det er, bliver det indsat som det sidste.
///</summary>
///<params name="xs">
///Er listen af sorterede tal som det nye skal indsættes i.
///</params>
///<params name="x">
///Er tallet det skal indsættes.
///</params>
///<returns>
///En ny sorteret liste med den nye tal i.
///</returns>

let rec insert xs x =
    match xs with 
    | [] -> [x]
    | [y1] -> 
        if x >= y1 then
            [y1;x]
        else
            [x;y1]
    | y1::ys ->
        if x < y1 then
            [x;y1] @ ys
        else 
            [y1] @ (insert ys x)

printfn "               HR 4.11(2)"
printfn "Test1: %b" ((insert [] 1) = [1]) //make your tests here
printfn "Test2: %b" ((insert [4;5;6] 7) = [4;5;6;7])
printfn "Test3: %b" ((insert [7;8;9] 6) = [6;7;8;9])
printfn "Test4: %b" ((insert [1;3;4] 2) = [1;2;3;4])


(* 3 *)

(* Kommentarer til løsningen af denne opgave skrives her *)

///<summary>Gets the intersection of two lists</summary>
///<params name="list1">the first list of which to get the intersection</params>
///<params name="list2">the second list of which to get the intersection</params>
///<returns>A list containing the intersecting elements in each list</returns>
let rec intersect list1 list2 =
    // For each element in list1 we check if it exists in list2
    match list1 with
    | [] -> []
    | x::xs ->
        match list2 with
        | [] -> []
        | y::ys ->
            // if they're the same, we pick one and continue
            if y = x then
                y :: intersect xs ys
            // if y is smaller than x we continue through list2
            elif y < x then
                intersect list1 ys
            // if x is smaller than y, x does not exist in y
            else
                intersect xs list2

printfn "               HR 4.11(3)"
//Test from the book
printfn "Test1: %b" (intersect [1;1;1;2;2] [1;1;2;4] = [1;1;2])
//make more tests here
printfn "Test2: %b" (intersect [1; 1; 5; 6; 9] [1; 2; 5; 9] = [1;5;9])
printfn "Test3: %b" (intersect [] [1] = [])
printfn "Test4: %b" (intersect [1] [] = [])

(* 4 *)

(* Kommentarer til løsningen af denne opgave skrives her *)

///<summary>adds list1 to list2 by emptying list1 into list2
///one element at a time using the insert function.
///When list1 is empty, list2 is returned</summary>
///<params name="list1">the first list</params>
///<params name="list2">the second list</params>
///<returns>list2 after inserting the elements from list1</returns>
let rec plus list1 list2 =
    match list1 with
    | [] -> list2
    | x::xs ->
        plus xs (insert list2 x)


printfn "               HR 4.11(4)"
//Test from the book
printfn "Test1: %b" (plus [1;1;2] [1;2;4] = [1;1;1;2;2;4])
//make more tests here
printfn "Test2: %b" (plus [4;5;6] [1;2;3] = [1;2;3;4;5;6])
printfn "Test3: %b" (plus [] [1] = [1])
printfn "Test4: %b" (plus [1] [] = [1])

(* 5 *)

(* Kommentarer til løsningen af denne opgave skrives her *)

///<summary>
///Denne funktion fjerner elementer i den første liste hvis de er i
///den anden liste. 
///Den gør dette ved at tjekke om værdien i l1 er i l2, hvis den er fjernes 
///begge elementer, hvis ikke sættes værdien i l1 ind i listen der returneres.
///Hvis værdien i l2 er mindre end den i l1, fjernes den fra listen i l2.
///</summary>
///<params name="l1">
///Start listen som indholder tal der skal tjekkes.
///</params>
///<params name="l2">
///Listen som l1 skal tjekkes for.
///</params>
///<returns>
///En liste af tal som ikke findes i l2
///</returns>

let rec minus l1 l2 =
    match l1 with 
    | [] -> []
    | x1::xs -> 
        match l2 with
        | [] -> l1
        | y1::ys ->
            if x1 = y1 then
                minus xs ys
            elif x1 > y1 then             
                minus l1 ys
            else              
                x1::(minus xs l2)

printfn "               HR 4.11(5)"
//Test from the book
printfn "Test1: %b" ((minus [1;1;1;2;2] [1;1;2;3]) = [1;2])
printfn "Test2: %b" ((minus [1;2;2;3;4] [1;1;2;2;3]) = [4]) 
printfn "Test3: %b" ((minus [1;1;2;2;4] [1;1;2;3]) = [2;4])
printfn "Test4: %b" ((minus [1;1;2;3] [1;1;1;2;2]) = [3])
//make more tests here

(* HR 4.15 *)

(* Kommentarer til løsningen af denne opgave skrives her *)

///<summary>
///Denne funktion vender rækkefølgen for en liste af lister, samt rækkefølgen
/// af de indre lister.
///Den gør dette ved at sætte, det element den er kommet til, bag ved resten af
///listen, som sendes videre, for at få gjort det samme.
///For at vende den indre liste, bruger den en hjælpefunktion, som gør det
///som hoved funktionen.
///</summary>
///<params name="A">
///Dette er listen som skal vendes
///</params>
///<returns>
///Den vendte liste.
///</returns>

let rec revrev A =
    let rec revrevFlip O =
        match O with
        | [] -> O
        | x1::xs -> (revrevFlip xs) @ [x1]
    match A with
    | [] -> A
    | x1::xs -> 
        (revrev xs) @ [(revrevFlip x1)]

printfn "               HR 4.15"
//Test from the book
printfn "Test1: %b" (revrev [[1;2];[3;4;5]] = [[5;4;3];[2;1]])
printfn "Test2: %b" (revrev [[1;2;7;4;];[3]] = [[3];[4;7;2;1]])
printfn "Test3: %b" (revrev [[1;2;3];[4;5;6]] = [[6;5;4];[3;2;1]])
printfn "Test4: %b" (revrev [[1;2];[3]] = [[3];[2;1]])
//make more tests here


(* 4g.1 *)
printfn "             OPGAVE 4g.1"
(* Remove duplicates *)

(* Kommentarer til løsningen af denne opgave skrives her *)

///<summary>Removes any duplicates from a list</summary>
///<params name="list">list to remove duplicates from</params>
///<returns>The list with all duplicates removed</returns>
let rec removeDuplicates list =
    // removes the element "elem" from the list "list"
    let rec removeElem elem list =
        match list with
        | [] -> []
        | x::xs ->
            // if x = elem, we need to remove x
            if x = elem then removeElem elem xs
            // otherwise keep x and continue
            else x :: removeElem elem xs

    match list with
    | [] -> []
    // call removeElem using each element of the list
    | x::xs -> x :: (removeDuplicates (removeElem x xs))


printfn "               Remove Dublicates"
//Test from the book
printfn "Test1: %b" (removeDuplicates [1;2;1;3;2] = [1;2;3])
//make more tests here
printfn "Test2: %b" (removeDuplicates [9;9;9] = [9])
printfn "Test3: %b" (removeDuplicates [] = [])
printfn "Test4: %b" (removeDuplicates [1;3;5;2;4;6;1;3;7;5] = [1;3;5;2;4;6;7])
