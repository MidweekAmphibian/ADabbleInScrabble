namespace ScrabbleBot

open ScrabbleUtil
open ScrabbleUtil.ServerCommunication

open System.IO

open ScrabbleUtil.DebugPrint
open Util

// The RegEx module is only used to parse human input. It is not used for the final product.

module RegEx =
    open System.Text.RegularExpressions

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)

        if m.Success then
            Some(List.tail [ for g in m.Groups -> g.Value ])
        else
            None

    let parseMove ts =
        let pattern = @"([-]?[0-9]+[ ])([-]?[0-9]+[ ])([0-9]+)([A-Z]{1})([0-9]+)[ ]?"

        Regex.Matches(ts, pattern)
        |> Seq.cast<Match>
        |> Seq.map (fun t ->
            match t.Value with
            | Regex pattern [ x; y; id; c; p ] -> ((x |> int, y |> int), (id |> uint32, (c |> char, p |> int)))
            | _ -> failwith "Failed (should never happen)")
        |> Seq.toList

module Print =

    let printHand pieces hand =
        hand
        |> MultiSet.fold (fun _ x i -> forcePrint (sprintf "%d -> (%A, %d)\n" x (Map.find x pieces) i)) ()

module State =
    // Make sure to keep your state localised in this module. It makes your life a whole lot easier.
    // Currently, it only keeps track of your hand, your player numer, your board, and your dictionary,
    // but it could, potentially, keep track of other useful
    // information, such as number of players, player turn, etc.

    let boardMap = Map<coord, (uint32 * (char * int))> //We aren't using the parser for the board, we just use a simple coordinate system represented by a map.

    type state =
        { lettersOnBoard: Map<coord, (uint32 * (char * int))>
          dict: ScrabbleUtil.Dictionary.Dict
          playerNumber: uint32
          hand: MultiSet.MultiSet<uint32>
          numOfPlayers: uint32
          playerTurn: uint32
          timeout: uint32 option }

    let mkState
        (boardMap: Map<coord, (uint32 * (char * int))>)
        newDict
        newPlayerNumber
        newHand
        newNumOfPlayers
        newPlayerTurn
        timeout
        =
        { lettersOnBoard = boardMap
          dict = newDict
          playerNumber = newPlayerNumber
          hand = newHand
          numOfPlayers = newNumOfPlayers
          playerTurn = newPlayerTurn
          timeout = timeout }

    let lettersOnBoard st = st.lettersOnBoard
    let dict st = st.dict
    let playerNumber st = st.playerNumber
    let numOfPlayers st = st.numOfPlayers
    let hand st = st.hand
    let playerTurn st = st.playerTurn


    let updateHand move st newPieces =

        // UPDATE THE HAND:
        // Extract piece IDs from each move
        let playedPieceIds = move |> List.map (fun (_, (uid: uint32, (_, _))) -> uid)

        // make new list with played pieces from pieces on hand
        let handWithRemovedPieces =
            playedPieceIds
            |> List.fold (fun hand (pieceId: uint32) -> MultiSet.removeSingle pieceId hand) (hand st)
        // add the new pieces to this list
        let handWithAddedAndRemovedPieces =
            newPieces
            |> List.fold
                (fun handWithRemovedPieces (pieceID: uint32, count: uint32) ->
                    MultiSet.add pieceID count handWithRemovedPieces)
                (handWithRemovedPieces)
        // At the end, set the State hand to this list.
        handWithAddedAndRemovedPieces

    let updatePiecesOnBoard move st =
        move
        |> List.fold (fun acc (coord, ((char, points))) -> Map.add coord (char, points) acc) st.lettersOnBoard


module FindWord =
    open ScrabbleUtil
    open MultiSet
    open System.Threading
    open System.Threading.Tasks
    open Util

    type Direction =
        | Up
        | Right
        | Down
        | Left

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
                lookInDirectionRec
                    lettersOnBoard
                    newCoord
                    direction
                    (foundWordAcc @ [ (pieceId, (letter, pointValue)) ])
            | Up
            | Left ->
                lookInDirectionRec
                    lettersOnBoard
                    newCoord
                    direction
                    ([ (pieceId, (letter, pointValue)) ] @ foundWordAcc)

    let getStartingPoints lettersOnBoard =

        let checkForStartingPointsAtCoord
            (lettersOnBoard: Map<coord, (uint32 * (char * int))>)
            (coord: coord)
            (boardPiece: (uint32 * (char * int)))
            =
            //This function is called while going over all the pieces on the board to find places where we can start a word.
            //In order to do this, we check the coordinates adjacent to this tile. If there is a free tile on some direction, that means we could potentially continue a word in that direction.
            //But we need to check everything that comes before in order to do that.
            //When we have done that we get a list of starting points for our new word which we can then go over.


            //debugPrint ("\nFINDING STARTING POINTS!\n")
            let (x, y) = coord

            let directions = [ Down; Right ]

            let startingPointsFoundAtCoord =
                directions
                |> List.fold
                    (fun
                        (foundStartingPointsAcc: ((Direction * coord) * (uint32 * (char * int)) list) list)
                        (directionToLook: Direction) ->
                        // The accumulator has type since it keeps a list of starting points which are like words (and therefore are themselves also lists)

                        //debugPrint ("NOW LOOKING FOR A FREE SPOT IN THIS DIRECTION: ")
                        //debugPrint ((string) directionToLook)
                        //debugPrint ("\n")

                        let checkAdjacentPiece =
                            match directionToLook with
                            | Down -> (x, y + 1)
                            | Right -> (x + 1, y)
                            | _ -> failwith "Invalid direction" //Shouldn't happen...

                        match Map.tryFind checkAdjacentPiece lettersOnBoard with
                        | None ->
                            // If there is no adjacent piece in this direction, we might be able to continue a word in this direction
                            // We keep looking in the opposite direction and return everything that comes before (word (or reverse word....) or just a letter) as a starting point along with a direction and coord
                            //debugPrint ("\nTHERE IS A FREE SPOT: THAT MEANS WE CAN BUILD A WORD FROM HERE ")
                            //Get opposite direction:
                            let getOppositeDirection (direction: Direction) =
                                match direction with
                                | Down -> Up
                                | Right -> Left
                                | _ -> direction //Shoudln't happen...

                            let oppositeDirection = getOppositeDirection directionToLook
                            //debugPrint ("\n GIVEN THAT WE FOUND A FREE TILE IN THE DIRECTION ")
                            //debugPrint ((string) directionToLook)
                            //debugPrint ("\n WE NOW CHECK HOW MANY ELEMENTS ARE IN THE OPPOSITE DIRECTION: ")
                            //debugPrint ((string) oppositeDirection)
                            //debugPrint ("\n")

                            let startingPointInDir: (uint32 * (char * int)) list =
                                lookInDirectionRec lettersOnBoard coord oppositeDirection [ boardPiece ]

                            //debugPrint ("\nTHERE ARE THIS MANY ELEMENTS IN THE FOUND STARTING POINT:  ")
                            //debugPrint ((string) startingPointInDir.Length)
                            //debugPrint ((string) ((directionToLook, coord), startingPointInDir))
                            foundStartingPointsAcc @ [ ((directionToLook, coord), startingPointInDir) ]
                        | Some _ ->
                            // If we do enounter an adjacent letter in the direction we are looking in, then we can't play a word in that direction from this point.
                            foundStartingPointsAcc)
                    []

            startingPointsFoundAtCoord

        lettersOnBoard
        |> Map.fold
            (fun
                (foundStartingPointsAcc: ((Direction * coord) * (uint32 * (char * int)) list) list)
                (coord: coord)
                (boardPiece: uint32 * (char * int)) ->
                let startingPointsAtCoord: ((Direction * coord) * (uint32 * (char * int)) list) list =
                    checkForStartingPointsAtCoord lettersOnBoard coord boardPiece

                foundStartingPointsAcc @ startingPointsAtCoord)
            []




    let getFullInformationMultiSet
        (availablePieceIds: MultiSet.MultiSet<uint32>)
        (pieces: Map<uint32, Set<char * int>>)
        =
        //go through the piece ids multiset
        availablePieceIds

        //The fold function has an accumulator which keeps track of the new multiset with more information than before
        //The fold function also takes pieceId (This is the element it gets from the fold) and the count of that pieceId
        |> MultiSet.fold
            (fun (foldAccumulator: MultiSet<uint32 * (Set<char * int>)>) (pieceId: uint32) pieceCount ->

                //In the fold function, we try to look up the pieceID in the map pieces, which returns (char * int), that is, the letter and character.
                match Map.tryFind pieceId pieces with

                //if we find some info, we
                | Some(infoSet: Set<char * int>) -> MultiSet.add (pieceId, infoSet) pieceCount foldAccumulator
                | None -> foldAccumulator)
            MultiSet.empty


    let convertWordToMoveString (word: (uint32 * (char * int)) list) (startCoord: int * int) (direction: Direction) =
        let rec auxMove acc x y =
            function
            | [] -> acc
            | (pieceId, (letter, pointValue)) :: rest ->
                let coord = sprintf "%d %d %d%c%d" x y pieceId letter pointValue
                let separator = if acc = "" then "" else " " //We avoid seperator for the first element here
                let newAcc = acc + separator + coord
                auxMove newAcc (if direction = Right then x + 1 else x) (if direction = Down then y + 1 else y) rest

        let move = auxMove "" (fst startCoord) (snd startCoord) word

        move


    let rec findMoveRecursive
        (piecesOnHandWithInfo: MultiSet.MultiSet<uint32 * (Set<char * int>)>)
        //(builtWord: (uint32 * (char * int)) list)
        (lettersPlayedSoFar: (uint32 * (char * int)) list)
        (dict: ScrabbleUtil.Dictionary.Dict)
        (coord: coord)
        (direction: Direction)
        (lettersOnBoard: Map<coord, (uint32 * (char * int))>)
        (pieces: Map<uint32, Set<char * int>>)
        (isContinuation: bool)
        =
        //if isContinuation then
        //debugPrint ("\n")
        //debugPrint ("NOW CONTINUING FROM A STARTING POINT")
        //debugPrint ((string) builtWord)
        //debugPrint ("THERE ARE THIS MANY PIECES ON THE HAND: ")
        //let handList = piecesOnHandWithInfo |> MultiSet.toList
        //debugPrint ((string) handList.Length)
        //debugPrint ("\n")
        //else
        //debugPrint ("NOW STARTING FROM NOTHING")
        //Todo: Understand how we are meant to use the direction and coord in here? Especially direction....
        //Right now we fold through availablePiecesWithInfo. I wonder if we should

        //First, check what is at the current coordinate. If there is something there, we need to use that letter instead of the letters we have on our hand.
        let (letterAlreadyOnTile: (uint32 * (char * int)) option) =

            //debugPrint ("\nNOW LOOKING UP THE FOLLOWING COORD: ")
            //debugPrint ((string) coord)
            Map.tryFind coord lettersOnBoard

        let (availablePieces: MultiSet<uint32 * Set<char * int>>) =
            match letterAlreadyOnTile with
            | Some(pieceId, (letter, pointValue)) ->
                MultiSet.empty
                |> MultiSet.addSingle (pieceId, Set.singleton (letter, pointValue)) //Create a new multiset with just one element of the right format....
            | None -> piecesOnHandWithInfo

        //Fold over the available pieces, whether this is just one letter from a piece already on the board, or the pieces on our hand
        availablePieces
        |> MultiSet.fold
            (fun (isWord, longestWord: (uint32 * (char * int)) list) (element: uint32 * (Set<char * int>)) count ->
                //We look at the piece id and the charInfoSet (The set of chatacters (with point values) associated with a piece) seperately.
                //The reason is that for each piece, we also need to fold through the set of chars on that piece (1, or all the letters in the wildcard case)

                match element with
                | ((pieceId: uint32), (charInfoSet: Set<char * int>)) ->

                    //Fold through the set of characters and get the longest word that we find through the recursive calls for each letter the piece can be.
                    let (didIFindAWordInCharSet, longestWordInCharSet) =
                        charInfoSet
                        |> Set.fold
                            (fun
                                (isWord, longestWord: (uint32 * (char * int)) list)
                                ((letter: char), (pointValue: int)) ->

                                //Step into the dictionary with the letter we are currently looking at.
                                match (Dictionary.step letter dict) with
                                //If there is some letter, that means we have found a new node in the dict below.
                                //In other words, we can built a longer word from this point using our letters on hand.
                                //We get back a boolian isWord and a newDict.
                                | Some(isWord: bool, newDict) ->
                                    //debugPrint ("\nIT WAS POSSIBLE TO STEP DOWN INTO THE DICT WITH THE LETTER: ")
                                    //debugPrint ((string) letter)
                                    let updatedAvailablePieces = MultiSet.removeSingle element piecesOnHandWithInfo
                                    //debugPrint ("\nTHIS IS THE NEW HAND: ")
                                    //let newHandList = updatedAvailablePieces |> MultiSet.toList
                                    //debugPrint ((string) newHandList.Length)

                                    //let updatedBuiltWord = builtWord @ [ (pieceId, (letter, pointValue)) ]
                                    //debugPrint ("\nTHE UPDATED BUILT WORD IS: ")
                                    //debugPrint ((string) updatedBuiltWord)

                                    let updatedPlayedLetters =
                                        lettersPlayedSoFar @ [ (pieceId, (letter, pointValue)) ]


                                    let (x, y) = coord

                                    let newCoord =
                                        match direction with
                                        | Down -> (x, y + 1)
                                        | Right -> (x + 1, y)
                                        | _ -> failwith "Invalid direction"

                                    //OBS: We need to check if there is anything next to this coordinate on either side

                                    let directionsToLookForAdjacentPieces =
                                        match direction with
                                        | Down -> (Left, Right)
                                        | Right -> (Up, Down)
                                        | _ -> failwith "Invalid direction" //Shouldn't happen ...

                                    let getAdjacentCoordinate (x, y) direction =
                                        match direction with
                                        | Up -> (x, y - 1)
                                        | Down -> (x, y + 1)
                                        | Left -> (x - 1, y)
                                        | Right -> (x + 1, y)


                                    let (firstDirection, secondDirection) = directionsToLookForAdjacentPieces

                                    let adjacentTileInFirstDirectionCoord =
                                        getAdjacentCoordinate (x, y) firstDirection

                                    let adjacentTileInSecondDirectionCoord =
                                        getAdjacentCoordinate (x, y) secondDirection



                                    let adjacentFirstDirectionEmpty =
                                        match Map.tryFind adjacentTileInFirstDirectionCoord lettersOnBoard with
                                        | Some _ -> false
                                        | None -> true

                                    let adjacentSecondDirectionEmpty =
                                        match Map.tryFind adjacentTileInSecondDirectionCoord lettersOnBoard with
                                        | Some _ -> false
                                        | None -> true

                                    let tileIsFree = adjacentFirstDirectionEmpty && adjacentSecondDirectionEmpty

                                    if not tileIsFree then
                                        (false, [])
                                    else

                                        // let prefix =
                                        //     match Map.tryFind adjacentTileInFirstDirectionCoord lettersOnBoard with
                                        //     | Some _ ->
                                        //         debugPrint ("\nFOUND ADJACENT PIECE IN PREFIX\n")

                                        //         let foundPrefix =
                                        //             lookInDirectionRec lettersOnBoard coord firstDirection [] //OBS: SHOULD THIS BE COORD OR THE NEXT COORD?

                                        //         debugPrint ("\nI LOOKED FOR A PREFIX: THIS IS WHAT I FOUND: ")
                                        //         debugPrint ((string) foundPrefix)

                                        //         foundPrefix
                                        //     | None -> []




                                        // let suffix =
                                        //     match Map.tryFind adjacentTileInSecondDirectionCoord lettersOnBoard with
                                        //     | Some _ ->
                                        //         debugPrint ("\nFOUND ADJACENT PIECE IN SUFFIX\n")

                                        //         lookInDirectionRec lettersOnBoard coord secondDirection [] //OBS: SHOULD THIS BE COORD OR THE NEXT COORD?
                                        //     | None ->
                                        //         // debugPrint ("NOTHING ADJACENT IN COORD: ")
                                        //         // debugPrint ((string) adjacentTileInSecondDirectionCoord)
                                        //         // debugPrint ("\n")
                                        //         []

                                        //debugPrint ("\nTHIS IS THE PREFIX: ")
                                        //debugPrint ((string) prefix)
                                        //debugPrint ("THIS IS THE SUFFIX")
                                        //debugPrint ((string) suffix)

                                        // if not prefix.IsEmpty || not suffix.IsEmpty then

                                        //     let pieceWithAdjacentPrefixAndSuffix =
                                        //         prefix @ [ (pieceId, (letter, pointValue)) ] @ suffix

                                        //     debugPrint (
                                        //         "\n\n THIS IS THE LETTER THAT HAD SOME ADJACENT STUFF WITH ALL THE ADJACENT STUff : "
                                        //     )

                                        //     debugPrint ((string) pieceWithAdjacentPrefixAndSuffix)

                                        //     //Next we check if this whole thing is a word. If so, we are ok to carry on from here, otherwise we need to indicate that this move is invalid.

                                        //     let getLetters (word: (uint32 * (char * int)) list) =
                                        //         word
                                        //         |> List.map (fun (_, (letter, _)) -> string letter)
                                        //         |> String.concat ""

                                        //     let pieceWithAdjacentPrefixAndSuffixAsString =
                                        //         getLetters pieceWithAdjacentPrefixAndSuffix

                                        //     let (isWord) =
                                        //         dict |> Dictionary.lookup pieceWithAdjacentPrefixAndSuffixAsString

                                        //     if not isWord then
                                        //         debugPrint (
                                        //             "\nI FOUND A PIECE WITH ADJACENT STUFF THAT DOESN'T MAKE A WORD: "
                                        //         )

                                        //         debugPrint (pieceWithAdjacentPrefixAndSuffixAsString)
                                        //         debugPrint ("\n")
                                        //         (false, [])
                                        //     else
                                        //         debugPrint ("\nI FOUND A PIECE WITH ADJACENT STUFF THAT MAKES A WORD: ")
                                        //         debugPrint (pieceWithAdjacentPrefixAndSuffixAsString)
                                        //         debugPrint ("\n")

                                        //         let (childIsAWord, childWord) =
                                        //             findMoveRecursive
                                        //                 updatedAvailablePieces
                                        //                 updatedPlayedLetters
                                        //                 newDict
                                        //                 newCoord
                                        //                 direction
                                        //                 lettersOnBoard
                                        //                 pieces
                                        //                 isContinuation


                                        //         if childIsAWord then
                                        //             if List.length childWord > List.length longestWord then
                                        //                 (true, childWord)
                                        //             else
                                        //                 (true, longestWord)
                                        //         else if isWord then
                                        //             (true, updatedPlayedLetters)
                                        //         else
                                        //             (false, [])
                                        //else
                                        //debugPrint ("Suffix and prefix should both be empty")

                                        //OBS: And also in the same direction that we are already going in case we end a word right before another character is there...)
                                        // debugPrint ("\n PREPARING TO RECURSIVELY CALL MYSELF ...")



                                        let (childIsAWord, childWord) =
                                            findMoveRecursive
                                                updatedAvailablePieces
                                                updatedPlayedLetters
                                                newDict
                                                newCoord
                                                direction
                                                lettersOnBoard
                                                pieces
                                                isContinuation


                                        let nextCoord =
                                            match direction with
                                            | Down -> (x, y + 2)
                                            | Right -> (x + 2, y)
                                            | _ -> failwith "Invalid direction"

                                        let checkIfTheNextCoordIsFreeBeforeRetutningFinishedWord =
                                            match Map.tryFind nextCoord lettersOnBoard with
                                            | Some _ ->
                                                debugPrint ("THERE IS A TILE IN THE NEW COORD!")
                                                false
                                            | None -> true


                                        if childIsAWord then
                                            if List.length childWord > List.length longestWord then
                                                if checkIfTheNextCoordIsFreeBeforeRetutningFinishedWord then
                                                    (true, childWord)
                                                else
                                                    debugPrint ("THERE IS A TILE IN THE NEW COORD! SCRAPPING WORD :(")

                                                    (false, [])
                                            else
                                                (true, longestWord)
                                        else if isWord then
                                            (true, updatedPlayedLetters)
                                        else
                                            (false, [])

                                | None -> (false, longestWord)) //OBS: SHOULD THIS BE TRUE?
                            (false, [])

                    if didIFindAWordInCharSet then
                        if List.length longestWordInCharSet > List.length longestWord then
                            (true, longestWordInCharSet)
                        else
                            (true, longestWord)
                    else
                        (isWord, longestWord)

            )

            (false, [])

    let continueFromStartingPoint
        (hand: MultiSet<uint32>)
        (startingPoint: ((Direction * coord) * (uint32 * (char * int)) list))
        (lettersOnBoard: Map<coord, (uint32 * (char * int))>)
        (dict: Dictionary.Dict)
        (pieces: Map<uint32, Set<char * int>>)
        =

        let piecesToPlayAfterStartingPoint =

            let ((startingPointDirection, (x, y)), piecesInStartingPoint: (uint32 * (char * int)) list) =
                startingPoint

            //We need to get the correct dict for continuing from the starting point.
            // That means we have to step down for each of the letters in the starting point and update the dict
            // debugPrint ("NOW I AM IN THE CONTINUE FUNCTION: THE LIST OF STARTING POINTS HAS THIS MANY ELEMENTS: ")
            // debugPrint ((string) piecesInStartingPoint.Length)

            let (startingPointLength, newWordDict) =

                piecesInStartingPoint
                |> List.fold
                    (fun (index, dict) piece ->
                        // debugPrint ("FOLD")
                        let (_, (letter, _)) = piece
                        let nextStep = dict |> Dictionary.step letter

                        if Option.isSome nextStep then
                            (index + 1, nextStep.Value |> snd)
                        else
                            (index, dict))
                    (0, dict)


            // debugPrint ("Got this far")

            let availablePiecesWithInfo = getFullInformationMultiSet hand pieces

            let coordToCarryOnFrom =
                match startingPointDirection with
                | Up -> (x, y - startingPointLength)
                | Down ->
                    // debugPrint ("\nTHIS IS THE STARTING POINT : ")
                    // debugPrint ((string) piecesInStartingPoint)
                    // debugPrint ("\nTHIS IS THE DIRECTION: ")
                    // debugPrint ((string) startingPointDirection)
                    // debugPrint ("\nTHIS IS THE LENGTH OF THE STARTING POINT: ")
                    // debugPrint ((string) startingPointLength)
                    // debugPrint ("\nTHIS IS THE X AND Y OF THE STARTING POINT: ")
                    // debugPrint ((string (x, y)))
                    // debugPrint ("\nTHIS IS THE MODIFIED X AND Y OF THE STARTING POINT: ")
                    // debugPrint ((string (x, y + startingPointLength)))
                    // debugPrint ("\n\n")

                    (x, y + 1)
                | Left -> (x - startingPointLength, y)
                | Right ->
                    // debugPrint ("\nTHIS IS THE STARTING POINT : ")
                    // debugPrint ((string) piecesInStartingPoint)
                    // debugPrint ("\nTHIS IS THE DIRECTION: ")
                    // debugPrint ((string) startingPointDirection)
                    // debugPrint ("\nTHIS IS THE LENGTH OF THE STARTING POINT: ")
                    // debugPrint ((string) startingPointLength)
                    // debugPrint ("\nTHIS IS THE X AND Y OF THE STARTING POINT: ")
                    // debugPrint ((string (x, y)))
                    // debugPrint ("\nTHIS IS THE MODIFIED X AND Y OF THE STARTING POINT: ")
                    // debugPrint ((string (x, y + startingPointLength)))
                    // debugPrint ("\n\n")
                    (x + 1, y)

            // debugPrint ("\n\nI SHOULD now be finding a new move ...")

            //debugPrint ("\nCONTINUING FROM STARTING POINT: THIS IS THE HAND:")
            //debugPrint ((string) hand)
            // debugPrint ("\nTHIS IS THE STARTING POINT LETTERS:")
            // debugPrint ((string) piecesInStartingPoint)
            // debugPrint ("\nTHIS IS THE STARTING POINT DIRECTION:")
            // debugPrint ((string) startingPointDirection)
            // debugPrint ("\nTHESE ARE THE AVAILABLE PIECES WITH INFO:")
            // debugPrint ((string) availablePiecesWithInfo)
            // debugPrint ("\nTHESE ARE THE COORDS TO CARRY ON FROM :")
            // debugPrint ((string) coordToCarryOnFrom)
            // debugPrint ("\n\n\n\n\n")

            findMoveRecursive
                availablePiecesWithInfo
                []
                newWordDict
                coordToCarryOnFrom
                startingPointDirection
                lettersOnBoard
                pieces
                true

        //DELETE v ONLY FOR DEBUG PRINTING
        let ((startingPointDirection, (x, y)), piecesInStartingPoint: (uint32 * (char * int)) list) =
            startingPoint

        let (isWord, pieces) = piecesToPlayAfterStartingPoint
        let fullword = piecesInStartingPoint @ pieces

        if isWord then
            debugPrint ("\n\nTHIS IS THE STARTING POINT: ")

            let piecesInStartingPointString =
                Util.listOfPiecesToPlainString piecesInStartingPoint

            debugPrint (piecesInStartingPointString)
            debugPrint ("\nTHIS IS THE WORD WE ARE PLAYING WITH THE STARTING POINT: ")
            let fullWordString = Util.listOfPiecesToPlainString fullword
            debugPrint (fullWordString)
            debugPrint ("\n\n")

            piecesToPlayAfterStartingPoint
        else
            piecesToPlayAfterStartingPoint

    let findMove
        (hand: MultiSet<uint32>)
        (availablePiecesWithInfo: MultiSet<uint32 * Set<char * int>>)
        //(builtWord: (uint32 * (char * int)) list)
        (dict: ScrabbleUtil.Dictionary.Dict)
        (lettersOnBoard: Map<coord, (uint32 * (char * int))>)
        (pieces: Map<uint32, Set<char * int>>)
        =

        //Convert the list to a string representation for printing
        let listToString (lst) =
            lst |> List.map string |> String.concat "; "

        debugPrint ("\nTHIS IS THE HAND: ")
        debugPrint (listToString (hand |> MultiSet.toList))
        debugPrint (" Number of pieces: ")
        debugPrint ((string) (hand |> MultiSet.toList).Length)
        debugPrint ("\n")


        if lettersOnBoard.IsEmpty then
            // debugPrint ("FOUND FIRST MOVE")

            let ((isWord: bool), starterWord: (uint32 * (char * int)) list) =
                findMoveRecursive availablePiecesWithInfo [] dict (0, 0) Right lettersOnBoard pieces false

            let starterMove = convertWordToMoveString starterWord (0, 0) Right
            starterMove
        else

            //There are already some pieces on the board. This means we need to continue something already on the board.
            //We find possible starting points

            let startingPoints: ((Direction * coord) * (uint32 * (char * int)) list) list =
                getStartingPoints lettersOnBoard


            let move =

                let rec findBesttValidMoveForEachStartingPoint

                    possibleStartingPoint
                    (startingpointsToLookThrough: ((Direction * coord) * (uint32 * (char * int)) list) list)
                    (accBestMoves: ((Direction * coord) * (uint32 * (char * int)) list) list)

                    =

                    //Split up the starting point we are considering into direction, coord, and pieces to play
                    let ((possibleStartingPointDirection, possibleStartingPointCoord),
                         possibleStartingPointPiecesToPlay: (uint32 * (char * int)) list) =
                        possibleStartingPoint

                    //split coord into x and y
                    let (x, y) = possibleStartingPointCoord

                    let startingPointLength = possibleStartingPointPiecesToPlay.Length

                    //Since we want the move to continue from the end of the starting point, we find the coord to start from by adding the length of the list of pieces in the starting point to the coordinate.
                    // OBS WHY DOES +1 WORK INSTEAD .... ? TRY TO UNDERSTAND
                    let nextCoord =
                        match possibleStartingPointDirection with
                        | Down -> (x, y + 1)
                        | Right -> (x + 1, y)
                        | _ -> failwith ("BAH")

                    //When going looking for words we move through a dict tree structure. We need to continue down the dict from node we would be at
                    // if we had played the whole starting point as a normal word.
                    //We call continueFromStartingPoint. the function returns a boolean indicating if it found a word, and the word it found.
                    let (isWord, word: (uint32 * (char * int)) list) =
                        continueFromStartingPoint hand possibleStartingPoint lettersOnBoard dict pieces

                    //If we found a word from the starting point, we need to return it as a move.
                    match isWord with
                    | true ->
                        //debugPrint ("\ FOUND A WORD!: ")
                        //debugPrint ((string) word)

                        let updatedAccBestMoves =
                            accBestMoves @ [ ((possibleStartingPointDirection, nextCoord), word) ]

                        match startingpointsToLookThrough with
                        | [] -> accBestMoves
                        | nextElement :: remainingStartingpointsToLookThrough ->
                            findBesttValidMoveForEachStartingPoint
                                nextElement
                                remainingStartingpointsToLookThrough
                                updatedAccBestMoves


                    | false ->
                        debugPrint ("FAILED TO FIND A POSSIBLE WORD FOR THE STARTING POINT : ")
                        debugPrint ((string) possibleStartingPointPiecesToPlay)

                        match startingpointsToLookThrough with
                        | [] -> accBestMoves
                        | nextElement :: remainingStartingpointsToLookThrough ->
                            findBesttValidMoveForEachStartingPoint
                                nextElement
                                remainingStartingpointsToLookThrough
                                accBestMoves


                let bestMoves: ((Direction * coord) * (uint32 * (char * int)) list) list =

                    findBesttValidMoveForEachStartingPoint startingPoints.Head startingPoints.Tail []

                if bestMoves.Length = 0 then
                    ""
                else

                    let bestMovesWithPoints =

                        debugPrint ("THESE ARE ALL THE BEST MOVES WE CAN MAKE")

                        bestMoves
                        |> List.fold
                            (fun () move ->
                                let (_, elements) = move
                                let wordString = listOfPiecesToPlainString elements
                                debugPrint (wordString)
                                debugPrint ("; "))
                            ()

                        let getMovesWithAccumulatedPoints
                            (moveList: ((Direction * coord) * (uint32 * (char * int)) list) list)
                            =

                            moveList
                            |> List.fold
                                (fun
                                    (movesWithPoints: (int * ((Direction * coord) * (uint32 * (char * int)) list)) list)
                                    (move: ((Direction * coord) * (uint32 * (char * int)) list)) ->

                                    let (_, moveElements) = move

                                    let movePoints =
                                        moveElements
                                        |> List.fold
                                            (fun (accPoints: int) piece ->
                                                let (_, (_, pointValue)) = piece
                                                accPoints + pointValue)
                                            (0)

                                    movesWithPoints @ [ (movePoints, move) ])
                                ([])

                        getMovesWithAccumulatedPoints bestMoves

                    let sortedBestMoves =
                        bestMovesWithPoints |> List.sortByDescending (fun (int, _) -> int)

                    debugPrint ("\n THESE ARE THE SORTED BEST MOVES: ")
                    debugPrint ((string) sortedBestMoves)

                    let (_, bestMove) = sortedBestMoves.Head

                    let ((moveDirection, (moveCoord)), word) = bestMove
                    let (x, y) = moveCoord

                    convertWordToMoveString word moveCoord moveDirection

            move


module Scrabble =
    open System.Threading
    open FindWord

    let playGame cstream (pieces: Map<uint32, Set<char * int>>) (st: State.state) =

        let rec aux (st: State.state) (thisPlayersTurn: bool) =

            Async.Sleep(2000) |> Async.RunSynchronously

            debugPrint ("\n############################ v PLAYER TURN: v ############################\n")
            debugPrint ((string) st.playerTurn)
            debugPrint ("\n############################ ^ PLAYER TURN: ^ ############################\n")

            if (thisPlayersTurn) then

                //Print.printHand pieces (State.hand st)
                // remove the force print when you move on from manual input (or when you have learnt the format)
                //forcePrint
                //    "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"
                debugPrint (
                    sprintf " #############  It's your turn, player %d. #################\n\n" (State.playerNumber st)
                )

                // In order to construct a move, we'll first make a new MultiSet of the pieces we have on hand which has more info than just the pieceId.
                // This is to help make construct words easier. It has a lot of perks, but also means some stuff is kind of cumbersome...
                let allTheInfoAboutAvailablePieces = getFullInformationMultiSet st.hand pieces

                //Next we find a word to play. We get the input as string and then parse it with regex as we would manual input. This is just because this approach seemed
                //easier when we started.
                let word =
                    findMove st.hand allTheInfoAboutAvailablePieces st.dict st.lettersOnBoard pieces

                //let input = System.Console.ReadLine()
                let move: (coord * (uint32 * (char * int))) list = RegEx.parseMove word

                if word.Length > 0 then

                    debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
                    send cstream (SMPlay move)
                else
                    send cstream SMPass


            else
                debugPrint (
                    sprintf " ############### It is player %s's turn ############## \n\n" ((string) State.playerTurn)
                )

            let msg = recv cstream
            //debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

            match msg with
            | RCM(CMPlaySuccess(move, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)

                // UPDATE THE HAND:
                let updatedHand = State.updateHand move st newPieces

                //UDPDATE THE PLAYER NUMBER:
                let nextPlayer = (st.playerNumber % st.numOfPlayers) + 1u //OBS! Should there be +1u here?

                // UPDATE THE PIECES ON THE BOARD:
                let updatedPiecesOnBoard = State.updatePiecesOnBoard move st

                // UPDATE THE STATE WITH THE NEW INFO
                let st' =
                    { st with
                        hand = updatedHand
                        playerTurn = nextPlayer
                        lettersOnBoard = updatedPiecesOnBoard

                    } // Update the state with the new hand, board, player turn

                // Continue the game with the updated state
                aux st' (st.playerTurn = st.playerNumber)

            | RCM(CMPlayed(playerID, move, points)) ->
                (* Successful play by other player. Update your state *)

                //UDPDATE THE PLAYER NUMBER:
                let nextPlayer = (st.playerNumber % st.numOfPlayers) + 1u //OBS! Should there be +1u here?

                //UPDATE THE BOARD:
                let updatedPiecesOnBoard = State.updatePiecesOnBoard move st

                let st' =
                    { st with
                        playerTurn = nextPlayer
                        lettersOnBoard = updatedPiecesOnBoard } // Update the state with the new board, player turn

                aux st' (st.playerTurn = st.playerNumber)

            | RCM(CMPassed(pid)) ->
                debugPrint ("\n\n\n\n ################## PLAYER PASSED ###################### \n\n\n\n")
                let nextPlayer = (st.playerNumber % st.numOfPlayers) + 1u //OBSOBSOBSOBSOBS.. THIS DOESN'T MAKE SENSE. Different for different players ....
                let st' = { st with playerTurn = nextPlayer } // Update the state with the new board, player turn

                aux st' (st.playerTurn = st.playerNumber)

            | RCM(CMPlayFailed(pid, ms)) ->
                (* Failed play. Update your state *)
                //UDPDATE THE PLAYER NUMBER:

                let nextPlayer = (st.playerNumber % st.numOfPlayers) + 1u //OBS! Should there be +1u here?
                let st' = { st with playerTurn = nextPlayer } // Update the state with the new board, player turn

                aux st' (st.playerTurn = st.playerNumber)
            | RCM(CMGameOver _) -> ()
            | RCM a -> failwith (sprintf "not implmented: %A" a)
            | RGPE err ->
                printfn "Gameplay Error:\n%A" err
                aux st (st.playerTurn = st.playerNumber)


        aux st (st.playerTurn = st.playerNumber)

    let startGame
        (boardP: boardProg)
        (dictf: bool -> Dictionary.Dict)
        (numPlayers: uint32)
        (playerNumber: uint32)
        (playerTurn: uint32)
        (hand: (uint32 * uint32) list)
        (tiles: Map<uint32, tile>)
        (timeout: uint32 option)
        (cstream: Stream)
        =
        debugPrint (
            sprintf
                "Starting game!
                      number of players = %d
                      player id = %d
                      player turn = %d
                      hand =  %A
                      timeout = %A\n\n"
                numPlayers
                playerNumber
                playerTurn
                hand
                timeout
        )

        //let dict = dictf true // Uncomment if using a gaddag for your dictionary
        let dict = dictf false // Uncomment if using a trie for your dictionary

        let lettersOnBoard = Map.empty

        let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand

        fun () ->
            playGame
                cstream
                tiles
                (State.mkState lettersOnBoard dict playerNumber handSet numPlayers playerTurn timeout)
