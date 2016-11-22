open System

type codeColor =
    Red | Green | Yellow | Purple | White | Black
type code = codeColor list
type answer = int * int
type board = ( code * answer ) list
type player = Human | Computer

// The guesser gets 10 attempts
let maxGuesses = 10

/// <summary> Converts a string to a color. Fails if the color
/// does not exist in the game with an exception Unknowncolor </summary>
/// <params> The string to be converted </params>
/// <returns> The color from the string </returns>
let stringToColor = function
    | "red"    -> Red
    | "green"  -> Green
    | "yellow" -> Yellow
    | "purple" -> Purple
    | "white"  -> White
    | "black"  -> Black
    | _ -> failwith "UnknownColor"
                        
/// <summary> Converts a color to a string. Fails if the color </summary>
/// <params> The color to be converted </params>
/// <returns> The colorstring </returns>
let colorToString = function
    | Red    -> "Red"
    | Green  -> "Green"
    | Yellow -> "Yellow"
    | Purple -> "Purple"
    | White  -> "White"
    | Black  -> "Black"

/// <summary> Converts a code to a string. </summary>
/// <params name="code"> The code to be converted </params>
/// <returns> The codeString </returns>
let codeToString code =
    List.fold (fun acc elem -> acc + sprintf "%-8s " (colorToString elem)) "" code

/// <summary> By numbering the colors we can use random number generators etc.
/// This function just maps integers 0..5 to a color </summary>
/// <params> An integer </params>
/// <returns> A color </returns>
let intToColor = function
    | 0 -> Black
    | 1 -> White
    | 2 -> Red
    | 3 -> Green
    | 4 -> Yellow
    | 5 -> Purple
    | _ -> failwith "unknownColorNumber"

/// <summary> Generates a random color and returns this </summary>
/// <params> A dummy value used for nothing at all </params>
/// <returns> The random color </returns>
let getRandomColor _ =
    let rand = System.Random()
    intToColor (rand.Next(0,6))

/// <summary> Removes the first occurence of elem from the list </summary>
/// <parameters name="elem"> The element to be removed from the list </parameters>
/// <parameters name="list"> The list to remove the element from </parameters>
/// <returns> the list, where the first occurence of elem has been removed </returns>
let rec removeElem elem list =
    match list with
    | [] -> []
    | x::xs ->
        if x = elem then
            xs
        else
            x :: removeElem elem xs

/// <summary> Returns true if elem is contained in the list, otherwise false </summary>
/// <params name="elem"> The element to check the list for </params>
/// <params name="list"> The list to go through </params>
/// <remarks> I think this function is build-in in F# 3.5, but I am running 3.0</remarks>
let rec listContains elem list =
    match list with
    | [] -> false
    | x::xs ->
        if x = elem then
            true
        else
            listContains elem xs

/// <summary> Returns the number of elements that occur in both lists </summary>
/// <params name="a"> The first list </params>
/// <params name="b"> The second list </params>
/// <returns> The number of elements that occur in both lists </returns>
let rec numberOfMatches a b =
    match a with
    | [] -> 0
    | x::xs ->
        // If x is contained in list b, we add 1 to the number of matches,
        // remove x from list b, and run recursively on the rest of list a
        if listContains x b then
            1 + numberOfMatches xs (removeElem x b)
        // Otherwise we just continue for the rest of list a without doing anything.
        else
            numberOfMatches xs b

/// <summary> Returns the number of elements that are in the same position,
/// with the same color </summary>
/// <parameters name="guess"> The guess </parameters>
/// <parameters name="colorCode"> The code </parameters>
/// <returns> The number of elements that are in the correct position
/// with the correct color </returns>
let rec findBlacks guess colorCode =
    match guess with
    | [] -> 0
    | x::xs ->
        match colorCode with
        | [] -> 0
        | y::ys ->
            if x = y then
                1 + findBlacks xs ys
            else
                findBlacks xs ys


/// <summary> Generates a list of all codes available </summary>
/// <returns> The list of codes </returns>
let buildAllCodes () =
    let mutable codes = []
    for i in 0..5 do
        let c1 = intToColor i
        for j in 0..5 do
            let c2 = intToColor j
            for k in 0..5 do
                let c3 = intToColor k
                for l in 0..5 do
                    let c4 = intToColor l
                    let newCode = c1 :: c2 :: c3 :: [c4]
                    codes <- codes @ [newCode]
    codes : code list

/// <summary> Uses numberOfMatches to find the number of colors contained
/// in both codes. These colors are only counted once.
/// Then uses findblacks to find the number of elements that have the right
/// color in the right place. The number of white pins must then be the number of
/// matching colors minus the number of black pins.
/// Lastly the number of white and black pins are returned </summary>
/// <params name="guess"> The guess given </params> 
/// <params name="colorCode"> The code to guess </params>
/// <returns> A touple containing the number of white and black pins,
/// i.e. the number of correct colors in the wrong position, and the number
/// of correct colors in the correct position </returns>
let validate(guess:code, colorCode:code) =
    let colorMatches = numberOfMatches guess colorCode
    let blacks = findBlacks guess colorCode
    let whites = colorMatches - blacks
    (whites,blacks) : answer

    
/// <summary> Creates a new code given a player type.
/// If the player is human, the human is prompted for the code,
/// if it is a computer player, the code is generated</summary>
/// <params> A player type </params>
/// <returns> A colorCode</returns>
let rec makeCode = function
    | Computer ->
        let dummy = ["random";"random";"random";"random"]
        List.map getRandomColor dummy : code
    | Human ->
        printfn "\nThe colors available are: Red, Green, Black, White, Purple, Yellow\n"
        printfn "An example of a code is \"Red Purple Black White\"\n"
        printf "Enter a new color-code: "
        let codeString = System.Console.ReadLine().ToLower()
        // Splits the string into an array of words
        let colors = codeString.Split()
        // If 4 colors were chosen we check if the colors exist
        if colors.Length = 4 then
            try
                // If they do we convert them to the code type
                Array.fold (fun acc elem -> acc @ [stringToColor elem]) [] colors
            with
                Failure msg ->
                    printfn "\ncode failed with: %A, try again.\n" msg
                    // Run again
                    makeCode Human
        else
            printfn "\nMake sure you have 4 colors in your code\n"
            makeCode Human

/// <summary> Removes the codes that does not match the previous validations </summary>
/// <parameters name="playingBoard"> the board in play. This board will be used to remove
/// invalid codes from the codes list</parameters>
/// <parameters name="codes"> The list of available codes. These will be filtered using the
/// answers from the board </parameters>
/// <returns> The list of remaining codes after being filtered </returns> 
let rec removeCodes playingBoard codes =
    match playingBoard with
    | [] ->
        codes
    | x::xs ->
        let (xCode, xAnswer) = x
        // removes invalid codes by filtering on codes that would validate to the same answer as
        // the answer received in the board. Then removing the guess used as well.
        let newCodes = removeElem xCode (List.filter (fun elem -> (validate(elem,xCode)) = xAnswer) codes)
        removeCodes xs newCodes
       

/// <summary> Creates a new guess given a player type and a board.
/// If the player is human, the human is prompted for the code,
/// if it is a computer player, the code is generated</summary>
/// <params name="playerType"> A player type </params>
/// <params name="playingBoard"> The board of a game </params>
/// <returns> A colorCode </returns>
let rec guess(playerType:player, playingBoard:board) =
    match playerType with
    | Computer ->
        let allCodes = buildAllCodes ()
        
        match playingBoard with
        | [] ->
            // Make a random code containing only 2 colors
            let c1 = getRandomColor ()
            let mutable c2 = getRandomColor ()
            while (c1 = c2) do
                c2 <- getRandomColor ()
            let newCode = [c1;c1;c2;c2]
            printfn "Guess is: %s" (codeToString newCode)
            newCode : code
        | xs ->
            let guesses = removeCodes playingBoard allCodes
            match guesses with
            // There should always be at least one guess left unless the game is over
            | [] -> failwith "SanityCheckFail"
            | x::xs ->
                x
    | Human ->
        printfn "\nThe colors available are: Red, Green, Black, White, Purple, Yellow\n"
        printfn "An example of a guess is \"Red Purple Black White\"\n"
        printf "Enter a new guess: "
        let codeString = System.Console.ReadLine().ToLower()
        // Splits the string into an array of words, we use filter to make sure
        // the user did not enter too many spaces
        let colors = Array.filter (fun elem -> elem <> "") (codeString.Split())
        // If 4 colors were chosen we check if the colors exist
        if colors.Length = 4 then
            try
                // If they do we convert them to the code type
                Array.fold (fun acc elem -> acc @ [stringToColor elem]) [] colors
            with
                Failure msg ->
                    printfn "\nguess failed with: %A, try again.\n" msg
                    // Run again
                    guess(playerType, playingBoard)
        else
            printfn "Make sure you have 4 colors in your guess\n"
            makeCode Human


/// <summary> Converts a string to a touple of Player types. Fails if the string
/// cannot be recognized </summary>
/// <parameters name="answer"> the string to be turned into player types </parameters>
/// <returns> A touple of player types used to determe the type of the game </returns>
let parseGameType answer =
    match answer with
    | "pvu" -> (Computer,Human)
    | "uvu" -> (Human,Human)
    | "uvp" -> (Human,Computer)
    | "pvp" -> (Computer,Computer)
    | _ -> failwith "unknownAnswer"

/// <summary> Prompts the user for the type of game they wish to play </summary>
/// <returns> A touple containing two playertypes, i.e. the gametype</returns>
let getGameType () =
    printfn "\nSelect gametype by typing:\n\nPvU - Program vs User (guessing)"
    printfn "UvU - User vs User"
    printfn "UvP - User vs Program (guessing)"
    printfn "PvP - Program vs Program\n"
    printf "type in one of these options and hit enter here: "

    let mutable finished = false
    // Just to set some initial value
    let mutable gameType   = (Human,Human) 
    
    // While we have no answer
    while not finished do
        try
            // Read the input, convert it to lowercase and remove whitespaces
            let answer   = System.Console.ReadLine().ToLower().Replace(" ", "")
            gameType <- parseGameType answer
            finished <- true
        with
            | Failure msg ->
                printf "\nFailed with: %s\n\ntry again: " msg
    gameType

/// <summary> Plays a turn of the game
/// by using the different functions of the program </summary>
/// <parameters name="code"> The code for the player to guess </parameters>
/// <parameters name="board"> The board in play </parameters>
/// <parameters name="player"> The player that tries to guess the code </parameters>
/// <returns> Returns the board at the end of the game </returns>
let rec playGame code board player =
    let guess = guess(player,board)
    let result = validate(guess, code)
    let newBoard = board @ [(guess,result)]
    Console.Clear()
    // Just to make it look a litle better
    printfn "\n\n\n\n\n\n\n\n\n\n"
    
    printfn "Current board:\n"
    // Aligning the text
    printfn "%15s %49s\n" "Guesses:" "Answers:"
    for i in 0..newBoard.Length-1 do
        let (guesses, (whites,blacks)) = newBoard.[i]

        // You cannot format with %A, so we need the colors as a string
        let colorString = codeToString guesses
        printfn "%5i: %s \t | \t %-3i White pins \t %-3i Black pins" (i+1) colorString whites blacks

    // Just to make it look a litle better
    printfn "\n\n\n\n\n\n\n\n\n\n"
    if result = (0,4) || newBoard.Length = maxGuesses then
        newBoard
    else
        playGame code newBoard player
                       
/// <summary> Starts a game up by prompting the user for the gametype and code.
/// Then displays the result at the end of the game </summary>
let game () =
    Console.Clear()
    let (player1, player2) = getGameType()
    let code  = makeCode player1
    let codeString = codeToString code 
    Console.Clear()
    let board = playGame code [] player2
    let (lastGuess,lastResult) = board.[board.Length-1]
    if lastResult = (0,4) then
        printfn "\nYOU WON FAM\n"
        printfn "\nCode was: %s\n" codeString 
    else
        printfn "\nYour moves are weak!\n"
        printfn "\nCode is: %s\n" codeString
game()
