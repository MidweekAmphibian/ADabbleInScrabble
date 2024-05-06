module internal Util

open MultiSet
open ScrabbleUtil

type Direction =
    | Up
    | Right
    | Down
    | Left


//This was meant to be somewhat more generic but Scrabble.fs got unfashionably long so a lot of the functions from there ended up in here as well.

let listOfPiecesToPlainString (word: (uint32 * (char * int)) list) =
    word |> List.map (fun (_, (letter, _)) -> string letter) |> String.concat ""

let checkIfTileIsFree coord lettersOnBoard =
    match Map.tryFind coord lettersOnBoard with
    | Some _ -> false
    | None -> true


let getAdjacentCoord directionToLook (x, y) =
    match directionToLook with
    | Up -> (x, y - 1)
    | Down -> (x, y + 1)
    | Left -> (x - 1, y)
    | Right -> (x + 1, y)

let directionsToLookForAdjacentPieces direction =
    match direction with
    | Down
    | Up -> (Left, Right)
    | Right
    | Left -> (Up, Down)

let checkIfCoordIsFree nextCoord lettersOnBoard =
    match Map.tryFind nextCoord lettersOnBoard with
    | Some _ -> false
    | None -> true

let checkAdjacentTilesFree (x, y) direction lettersOnBoard =
    let (firstDirection, secondDirection) = directionsToLookForAdjacentPieces direction

    let adjacentTileInFirstDirectionCoord = getAdjacentCoord firstDirection (x, y)

    let adjacentTileInSecondDirectionCoord = getAdjacentCoord secondDirection (x, y)

    let adjacentFirstDirectionEmpty =
        checkIfCoordIsFree adjacentTileInFirstDirectionCoord lettersOnBoard

    let adjacentSecondDirectionEmpty =
        checkIfCoordIsFree adjacentTileInSecondDirectionCoord lettersOnBoard

    //The tile should be automatically free if alreadyIncorporated is true, otherwise only if both adjacent directions are empty
    let tileIsFree = adjacentFirstDirectionEmpty && adjacentSecondDirectionEmpty
    tileIsFree





let rec lookInDirectionRec
    (lettersOnBoard: Map<coord, (uint32 * (char * int))>)
    (coord: coord)
    (direction: Direction)
    (foundWordAcc: (uint32 * (char * int)) list)
    =
    // This function is used when we go through the what is already on the board when trying to find out where to start the optimal move
    // And also when we are building a word from some starting position and encounter an adjacent piece on the board.

    // The idea is to keep moving in some direction from some start coordinate and keep going until there are no more pieces,
    // then return the pieces encountered, which will be the word.

    let (x, y) = coord

    //First find the next coord in the direction
    let newCoord =
        match direction with
        | Down -> (x, y + 1)
        | Up -> (x, y - 1)
        | Right -> (x + 1, y)
        | Left -> (x - 1, y)

    //Look up the new coord and see if anything is there or not.
    match Map.tryFind newCoord lettersOnBoard with
    | None -> //If nothing is there, we are done "exploring" the word on the board and we return the word found (the accumulator).
        foundWordAcc
    | Some(pieceId, (letter, pointValue)) ->
        //, if we hit a letter, we recursively move to the next letter witht he new letter added to the accumulator
        // OBS IS THIS RIGHT? We want the word to be in the right order no matter (top to bottom or left to right no matter what direction we are looking)
        match direction with
        | Down
        | Right ->
            lookInDirectionRec lettersOnBoard newCoord direction (foundWordAcc @ [ (pieceId, (letter, pointValue)) ])
        | Up
        | Left ->
            lookInDirectionRec lettersOnBoard newCoord direction ([ (pieceId, (letter, pointValue)) ] @ foundWordAcc)

let getOppositeDirection (direction: Direction) =
    match direction with
    | Down -> Up
    | Right -> Left
    | Left -> Right
    | Up -> Down

let convertWordToMoveString
    (word: (uint32 * (char * int)) list)
    (startCoord: int * int)
    (direction: Direction)
    lettersOnBoard
    =
    //In order to convert the word we go over it in a recursive function, updating the string output as we go. We keep track of x and y as well as how much we have left.
    let rec convertWordToMoveStringRec acc x y lettersOnBoard restOfWord =
        match restOfWord with
        | [] -> acc //If there is nothing left to look at return the string
        | (pieceId, (letter, pointValue)) :: rest ->
            //Else, first look up the piece with the updated coordinates on the board, since we can incorporate letters that are already on the board into the construction of our word.
            match lettersOnBoard |> Map.tryFind (x, y) with
            | Some _ ->
                //If there is something on the board in this spot, it is not our responsibility to add that piece as part of our move. We skip it but update the coords
                convertWordToMoveStringRec
                    acc
                    (if direction = Right then x + 1 else x)
                    (if direction = Down then y + 1 else y)
                    lettersOnBoard
                    rest
            | None -> // Else we carry on as normal adding ids, letters, point values with coords for each piece,
                let coord = sprintf "%d %d %d%c%d" x y pieceId letter pointValue
                let separator = if acc = "" then "" else " " //We avoid separator for the first element here
                let newAcc = acc + separator + coord

                convertWordToMoveStringRec //Call recursively
                    newAcc
                    (if direction = Right then x + 1 else x)
                    (if direction = Down then y + 1 else y)
                    lettersOnBoard
                    rest

    let move =
        convertWordToMoveStringRec "" (fst startCoord) (snd startCoord) lettersOnBoard word

    move


let movesSortedByPoints moves =
    //This is used to determine what moves to play. Right now it doesn't take into account the points of any of the words around it, the starting point or any amplifying factors on the board
    //So really, length might be a better heuristic for good moves than this.

    //First get a list of moves with cummulative points
    let getMovesWithAccumulatedPoints (moveList: ((Direction * coord) * (uint32 * (char * int)) list) list) =

        //Go through the list of moves
        moveList
        |> List.fold
            (fun
                (movesWithPoints: (int * ((Direction * coord) * (uint32 * (char * int)) list)) list)
                (move: ((Direction * coord) * (uint32 * (char * int)) list)) ->

                let (_, moveElements) = move

                //For each move go through each of the peaces
                let movePoints =
                    moveElements
                    |> List.fold
                        (fun (accPoints: int) piece ->
                            //Get out the point value and add it to the accumulator
                            let (_, (_, pointValue)) = piece
                            accPoints + pointValue)
                        (0)
                //Return a list of moves with associated point values
                movesWithPoints @ [ (movePoints, move) ])
            ([])

    let bestMovesWithPoints = getMovesWithAccumulatedPoints moves

    //Sort the list by points
    let sortedBestMoves =
        bestMovesWithPoints |> List.sortByDescending (fun (int, _) -> int)

    sortedBestMoves

//Sort by lentgth instead
let getMovesSortedByLength (moveList: ((Direction * coord) * (uint32 * (char * int)) list) list) =
    moveList |> List.sortBy (fun (_, moveElements) -> moveElements |> List.length)

let getFullInformationMultiSet (availablePieceIds: MultiSet.MultiSet<uint32>) (pieces: Map<uint32, Set<char * int>>) =
    //go through the piece ids multiset
    availablePieceIds

    //The fold function has an accumulator which keeps track of the new multiset with more information than before
    //The fold function also takes pieceId (This is the element it gets from the fold) and the count of that pieceId
    |> MultiSet.fold
        (fun (foldAccumulator: MultiSet<uint32 * (Set<char * int>)>) (pieceId: uint32) pieceCount ->

            //In the fold function, we try to look up the pieceID in the map pieces, which returns (char * int), that is, the letter and character.
            match Map.tryFind pieceId pieces with

            //if we find some info, we add it to the MultiSet accumulator, otherwise we are done and return the accumulator.
            | Some(infoSet: Set<char * int>) -> MultiSet.add (pieceId, infoSet) pieceCount foldAccumulator
            | None -> foldAccumulator)
        MultiSet.empty
