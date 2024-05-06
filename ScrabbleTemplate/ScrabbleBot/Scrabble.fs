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

    type state =
        { lettersOnBoard: Map<coord, (uint32 * (char * int))>
          dict: ScrabbleUtil.Dictionary.Dict
          playerNumber: uint32
          hand: MultiSet.MultiSet<uint32>
          numOfPlayers: uint32
          playerTurn: uint32
          lastSeenRemainingRemotePiecesStatus: bool }

    let mkState
        (boardMap: Map<coord, (uint32 * (char * int))>)
        newDict
        newPlayerNumber
        newHand
        newNumOfPlayers
        newPlayerTurn
        lastSeenRemainingRemotePiecesStatus
        =
        { lettersOnBoard = boardMap
          dict = newDict
          playerNumber = newPlayerNumber
          hand = newHand
          numOfPlayers = newNumOfPlayers
          playerTurn = newPlayerTurn
          lastSeenRemainingRemotePiecesStatus = lastSeenRemainingRemotePiecesStatus }

    let lettersOnBoard st = st.lettersOnBoard
    let dict st = st.dict
    let playerNumber st = st.playerNumber
    let numOfPlayers st = st.numOfPlayers
    let hand st = st.hand
    let playerTurn st = st.playerTurn

    let updateHand move st newPieces =
        //This function is for updating the hand when we play a word.
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
        //return the new multiset (the updated hand)
        handWithAddedAndRemovedPieces

    let removeFirstElementOnHand st =
        //This function is used when updating the hand after having swapped a piece. By removing the first element, we ensure that it is the same piece we remove,
        //both when we send the change piece message and when we update the state based on the message response we get back (since this happens in a different context... Not very elegant but it works.)
        let handAsList = MultiSet.toList (hand st)

        match handAsList with
        | [] -> MultiSet.empty<uint32> //If there are no elements to remove, just return an empty multiset.
        | head :: tail -> //If the list is not empty remove the first element and create a new MultiSet
            let updatedList = tail
            let updatedHand = MultiSet.ofList updatedList
            updatedHand

    let getOutFirstElementOnHand st =
        //When swapping a piece, we need to specify which piece in the change piece message we send. We always get out the first piece on the hand
        let handAsList = MultiSet.toList (hand st)

        match handAsList with
        | [] -> failwith "This shouldn't happen..." //Could this not happen? Add a check if the hand is empty before swapping....
        | head :: tail -> head

    let exchangePieceHand hand newPiece =
        //This function removes an the first element of the hand and then adds the new element to be swapped in.
        let handWithTheFirstElementRemoved = removeFirstElementOnHand hand

        let handWithExchangedPiece =
            handWithTheFirstElementRemoved |> MultiSet.addSingle newPiece

        handWithExchangedPiece

    let updatePiecesOnBoard move st =
        move //Go through the move using fold and add all the parts of the move to the map of pieces on the board with their coords nad information. Returns the new map when done
        |> List.fold (fun acc (coord, ((char, points))) -> Map.add coord (char, points) acc) st.lettersOnBoard

module FindWord =
    open ScrabbleUtil
    open MultiSet
    open System.Threading
    open System.Threading.Tasks
    open Util

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
            let (x, y) = coord

            let directions = [ Down; Right ] //Every starting point is a prefix in our version...

            let startingPointsFoundAtCoord =
                //For each direction ....
                directions
                |> List.fold
                    (fun
                        (foundStartingPointsAcc: ((Direction * coord) * (uint32 * (char * int)) list) list)
                        (directionToLook: Direction) ->

                        //Get the adjacent tile in the direction and look it up in the board map
                        let adjacentPiece = getAdjacentCoord directionToLook (x, y)

                        match Map.tryFind adjacentPiece lettersOnBoard with
                        | None ->
                            // If there is no adjacent piece in this direction, we might be able to continue a word in this direction
                            // We keep looking in the opposite direction and return everything that comes before (word or just a letter) as a starting point along with a direction and coord
                            //Get opposite direction
                            let oppositeDirection = Util.getOppositeDirection directionToLook
                            //Get the starting point which is the list of everything connected in the opposite direction of where we found a free tile.
                            let startingPointInDir: (uint32 * (char * int)) list =
                                Util.lookInDirectionRec lettersOnBoard coord oppositeDirection [ boardPiece ]
                            //Append the found starting point to the accumulator.
                            foundStartingPointsAcc @ [ ((directionToLook, coord), startingPointInDir) ]
                        | Some _ ->
                            // If we do enounter an adjacent letter in the direction we are looking in, then we can't play a word in that direction from this point.
                            foundStartingPointsAcc)
                    []

            startingPointsFoundAtCoord

        //Fold through every piece on the board and check in both directions if it can be (part of) a starting point
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

        //First, check what is at the current coordinate. If there is something there, we need to use that letter instead of the letters we have on our hand.
        let (letterAlreadyOnTile: (uint32 * (char * int)) option) =
            Map.tryFind coord lettersOnBoard

        let alreadyIncorporated =
            match letterAlreadyOnTile with
            //There's already something on the board where we are looking to create a word, but insead of scrapping the word, we can see if we can incorporate it. We set a boolean which we will use later....
            | Some(pieceId, (letter, pointValue)) -> true
            | None -> false

        //If there is something already on the tile, , we make the letter that is already on the board at the tile the only piece we have available for this recursive iteration.
        let (availablePieces: MultiSet<uint32 * Set<char * int>>) =
            match letterAlreadyOnTile with
            | Some(pieceId, (letter, pointValue)) ->
                //Create a new Multiset with a single piece with a single letter.
                MultiSet.empty
                |> MultiSet.addSingle (pieceId, Set.singleton (letter, pointValue)) //Create a new multiset with just one element of the right format....
            | None -> piecesOnHandWithInfo //Else, if there is no piece we just carry on with the pieces from our hand

        //Go over the available pieces
        availablePieces
        |> MultiSet.fold
            (fun (isWord, longestWord: (uint32 * (char * int)) list) (element: uint32 * (Set<char * int>)) count ->
                //We look at the piece id and the charInfoSet (The set of characters (with point values) associated with a piece) seperately.
                //The reason is that for each piece, we also need to fold through the set of chars on that piece (1, or all the letters in the wildcard case)
                //This means that we fold through both the pieces and the characters for each piece.
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
                                //In other words, we can potentially build a longer word from this point using our letters on hand.
                                //We get back a boolian isWord and a newDict.
                                | Some(isWord: bool, newDict) ->
                                    //Update the availablePieces and playedLetters, since we are carrying on from here ...
                                    let updatedAvailablePieces = MultiSet.removeSingle element piecesOnHandWithInfo

                                    let updatedPlayedLetters =
                                        lettersPlayedSoFar @ [ (pieceId, (letter, pointValue)) ]

                                    let (x, y) = coord

                                    let newCoord = getAdjacentCoord direction (x, y)

                                    //We need to check if there is anything adjacent on either side of the direction we are moving.
                                    //Ideally we would look if we make a new valid word along with the adjacent stuff before we abandon an attempt at making a word here
                                    //But we couldn't quite get that to work, and since we the the standard Dict which makes only words with odd lengths it is very unlikely to make valid words anyhow...
                                    //Instead, we just drop everything if the adjacent tiles are not free.

                                    let tileIsFree =
                                        if alreadyIncorporated then //if the tile is already a part of another word on the board and we are enountering it and trying to incorporate it, it will have adjacent pieces, but this is fine, since it is already a part of a valid word on the board. therefore set tileIsFree to true
                                            true
                                        else
                                            //Else check the adjacent
                                            checkAdjacentTilesFree (x, y) direction lettersOnBoard


                                    if not tileIsFree then
                                        //The tile is not free, so we can't continue. We return false (no word) and an empty list as the word.
                                        (false, [])
                                    else
                                        //The tile is free. That means we can carry on from here.

                                        // RECURSION! RECURSION! RECURSION! Going deeper

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

                                        // RECURSION! RECURSION! RECURSION! Coming back now

                                        //We now know what the longest word we have found below this node in the dict is given we have stepped down in the dict with the letter we have (at the start of this fold...)
                                        //Before we continue we need to consider the what comes after this. Specifically the case where the child returns a valid word but has not taken into account that there is something right in front of it, since it has not gotten to a recursive step where that tile is checked....

                                        let nextCoord =
                                            match direction with
                                            | Down -> (x, y + 2)
                                            | Right -> (x + 2, y)
                                            | _ -> failwith "Invalid direction"

                                        let checkIfTheNextCoordIsFreeBeforeReturningFinishedWord =
                                            checkIfTileIsFree nextCoord lettersOnBoard

                                        //If the child returns is a word, that means there is a longer word we can make by going deeper into the Dict Trie with the available pieces from where we are.
                                        //At this "node" in the recursive call "structure" we need to compare what is coming up from the nodes in the dict below with the different steps we make from here.
                                        if childIsAWord then
                                            // We need to compare the word of the child for this letter in the fold with the longest word we have seen so far.
                                            if List.length childWord > List.length longestWord then
                                                //If it is longer, update it.
                                                if checkIfTheNextCoordIsFreeBeforeReturningFinishedWord then
                                                    //The next tile is free, we are good to keep this word.
                                                    (true, childWord)
                                                else
                                                    // We have to scrap this word since the next tile is occupied...
                                                    (false, [])
                                            else
                                                // The word is not longer than the longest word we have seen so far. We don't return it.
                                                (false, [])
                                        else if isWord then
                                            //If I am myself a word (for this step in the fold), but my children aren't, then I can return my own word instead.
                                            (true, updatedPlayedLetters)
                                        else
                                            //Else, nothing is returned. We couldn't find anything valid from this step, although the step itself was valid :(
                                            (false, [])

                                | None -> (false, []))
                            //We're stepping out into territory where no words exist..
                            (false, [])

                    if didIFindAWordInCharSet then
                        //In the end, we need to compare between the two folds and find the longest word
                        if List.length longestWordInCharSet > List.length longestWord then
                            (true, longestWordInCharSet)
                        else
                            (true, longestWord)
                    else
                        (isWord, longestWord))
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

            //We need to get the correct dict for continuing from the starting point, in order to make sure that the words we try to create exist
            // That means we have to step down for each of the letters in the starting point and update the dict

            let (startingPointLength, newWordDict) =
                //Go through the pieces i nthe starting point to get its length as well as the new dict.
                piecesInStartingPoint
                |> List.fold
                    (fun (index, dict) piece ->
                        // debugPrint ("FOLD")
                        let (_, (letter, _)) = piece
                        //Step down with the letter
                        let nextStep = dict |> Dictionary.step letter


                        if Option.isSome nextStep then
                            (index + 1, nextStep.Value |> snd)
                        else
                            //Does this happen?
                            (index, dict))
                    (0, dict)

            //Get the available pieces with extra info
            let availablePiecesWithInfo = getFullInformationMultiSet hand pieces

            //Find the coord to continue from (why are we just adding one?? It works though.....)
            let coordToCarryOnFrom =
                match startingPointDirection with
                | Up -> (x, y - 1)
                | Down ->

                    (x, y + 1)
                | Left -> (x - 1, y)
                | Right ->

                    (x + 1, y)

            //Call the findMoveRecursive with the new dict, starting point and the poeces in the starting point and the direction of the startingPoint as arguments.
            findMoveRecursive
                availablePiecesWithInfo
                []
                newWordDict
                coordToCarryOnFrom
                startingPointDirection
                lettersOnBoard
                pieces
                true

        piecesToPlayAfterStartingPoint


    let rec findBesttValidMoveForEachStartingPoint
        possibleStartingPoint
        (startingpointsToLookThrough: ((Direction * coord) * (uint32 * (char * int)) list) list)
        (accBestMoves: ((Direction * coord) * (uint32 * (char * int)) list) list)
        (hand: MultiSet<uint32>)
        (dict: ScrabbleUtil.Dictionary.Dict)
        (lettersOnBoard: Map<coord, (uint32 * (char * int))>)
        (pieces: Map<uint32, Set<char * int>>)
        =

        //Recursively go through starting points

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
            | _ -> failwith ("Shouldn't happen ...")

        //When going looking for words we move through a dict tree structure. We need to continue down the dict from node we would be at
        // if we had played the whole starting point as a normal word.
        //We call continueFromStartingPoint. the function returns a boolean indicating if it found a word, and the word it found.
        let (isWord, word: (uint32 * (char * int)) list) =
            continueFromStartingPoint hand possibleStartingPoint lettersOnBoard dict pieces

        //If we found a word from the starting point, we need to return it as a move.
        match isWord with
        | true ->

            //We update the list of the moves we find
            let updatedAccBestMoves =
                accBestMoves @ [ ((possibleStartingPointDirection, nextCoord), word) ]

            //Recursive call,
            match startingpointsToLookThrough with
            | [] -> accBestMoves
            | nextElement :: remainingStartingpointsToLookThrough ->
                findBesttValidMoveForEachStartingPoint
                    nextElement
                    remainingStartingpointsToLookThrough
                    updatedAccBestMoves
                    hand
                    dict
                    lettersOnBoard
                    pieces

        | false ->
            //Recursive call without updating the accumulator since we found no word...
            match startingpointsToLookThrough with
            | [] -> accBestMoves
            | nextElement :: remainingStartingpointsToLookThrough ->
                findBesttValidMoveForEachStartingPoint
                    nextElement
                    remainingStartingpointsToLookThrough
                    accBestMoves
                    hand
                    dict
                    lettersOnBoard
                    pieces


    let findMove
        (hand: MultiSet<uint32>)
        (availablePiecesWithInfo: MultiSet<uint32 * Set<char * int>>)
        //(builtWord: (uint32 * (char * int)) list)
        (dict: ScrabbleUtil.Dictionary.Dict)
        (lettersOnBoard: Map<coord, (uint32 * (char * int))>)
        (pieces: Map<uint32, Set<char * int>>)
        =

        //First check if this is the first move. If so, we start in the center and we don't have to continue from any starting point
        if lettersOnBoard.IsEmpty then
            //Call findMoveRecursive with the coords 0,0 and an empty list for the starting point. We always go right on the first move.
            let ((isWord: bool), starterWord: (uint32 * (char * int)) list) =
                findMoveRecursive availablePiecesWithInfo [] dict (0, 0) Right lettersOnBoard pieces false

            //Get the word as a move string command and return it
            let starterMove =
                Util.convertWordToMoveString starterWord (0, 0) Right lettersOnBoard

            starterMove
        else
            //There are already some pieces on the board. This means we need to continue something already on the board.

            //We find possible starting points
            let startingPoints: ((Direction * coord) * (uint32 * (char * int)) list) list =
                getStartingPoints lettersOnBoard

            //Now we determine what words we can play from these starting points
            let move =
                //Get the best possible word (if any) for each starting point in a list

                let bestMoves: ((Direction * coord) * (uint32 * (char * int)) list) list =

                    findBesttValidMoveForEachStartingPoint
                        startingPoints.Head
                        startingPoints.Tail
                        []
                        hand
                        dict
                        lettersOnBoard
                        pieces

                // If there are no moves, return an empty string
                if bestMoves.Length = 0 then
                    debugPrint ("\n *** I CAN'T FIND ANY VALID MOVES ***\n")
                    ""
                else
                    // Else sort the found moves by points (or length?) and return the head of the sorted list.
                    let sortedBestMoves = movesSortedByPoints bestMoves

                    let (_, bestMove) = sortedBestMoves.Head

                    let ((moveDirection, (moveCoord)), word) = bestMove
                    let (x, y) = moveCoord

                    //Convert the move to a string format to use RegEx to parse it... Seemed easier when we were starting.
                    convertWordToMoveString word moveCoord moveDirection lettersOnBoard

            move

module Scrabble =
    open System.Threading
    open FindWord
    open System
    open State

    let playGame cstream (pieces: Map<uint32, Set<char * int>>) (st: State.state) =

        let getNextPlayer =
            match (st.playerTurn) >= st.numOfPlayers with
            | true -> 1u
            | false -> (st.playerTurn + 1u)

        let rec aux (st: State.state) (thisPlayersTurn: bool) =

            Async.Sleep(100) |> Async.RunSynchronously //We keep a small break here to make it easier to see what happens

            // debugPrint (sprintf " ############# Player turn %d. #################\n\n" (State.playerTurn st))

            if (thisPlayersTurn) then

                //Print.printHand pieces (State.hand st)
                // remove the force print when you move on from manual input (or when you have learnt the format)
                //forcePrint
                //    "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"
                // debugPrint (
                //     sprintf " #############  It's your turn, player %d. #################\n\n" (State.playerNumber st)
                // )

                // In order to construct a move, we'll first make a new MultiSet of the pieces we have on hand which has more info than just the pieceId.
                let allTheInfoAboutAvailablePieces = getFullInformationMultiSet st.hand pieces

                //Next we find a word to play. We get the input as string and then parse it with regex as we would manual input. This is just because this approach seemed
                //easier when we started.
                let word =
                    findMove st.hand allTheInfoAboutAvailablePieces st.dict st.lettersOnBoard pieces

                //let input = System.Console.ReadLine()
                let move: (coord * (uint32 * (char * int))) list = RegEx.parseMove word

                if word.Length > 0 then
                    //We have found a word. We play it.
                    // debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
                    send cstream (SMPlay move)
                else if st.lastSeenRemainingRemotePiecesStatus then
                    //We can't find a word with the letters we have on our hand.
                    //If there are still pieces to swap, we try swap one of them
                    send cstream (SMChange([ getOutFirstElementOnHand st ]))
                else
                    //Else we pass.
                    send cstream SMPass

            else
                debugPrint (
                    sprintf
                        "\n\n#############  It is not your turn, player %d. #################\n\n"
                        (State.playerNumber st)
                )

            let msg = recv cstream
            //debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.

            match msg with
            | RCM(CMPlaySuccess(move, points, newPieces: (uint32 * uint32) list)) ->

                //####################################################################################################
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)
                //####################################################################################################

                // get updated hand
                let updatedHand = updateHand move st newPieces

                // Get updated player turn
                let nextPlayer = getNextPlayer

                // Get updated pieces on board
                let updatedPiecesOnBoard = State.updatePiecesOnBoard move st

                // Update the state with the new values
                let st' =
                    { st with
                        hand = updatedHand
                        playerTurn = nextPlayer
                        lettersOnBoard = updatedPiecesOnBoard }

                // Call the function to continue the game with the new state. We use a boolean to indicate if it is this player's turn in the next loop of aux
                aux st' (nextPlayer = st.playerNumber)

            | RCM(CMPlayed(playerID, move, points)) ->
                //######################################################
                (* Successful play by other player. Update your state *)
                //######################################################

                // Get updated player turn
                let nextPlayer = getNextPlayer

                //Get updated board
                let updatedPiecesOnBoard = State.updatePiecesOnBoard move st

                //Update the state with new values
                let st' =
                    { st with
                        playerTurn = nextPlayer
                        lettersOnBoard = updatedPiecesOnBoard }

                aux st' (nextPlayer = st.playerNumber)

            | RCM(CMPassed(pid)) ->
                //debugPrint ("\n\n\n\n ################## PLAYER PASSED ###################### \n\n\n\n")
                //############################################################################################
                (* The player passed. This happens when a word can't be found and we can't swap any pieces. *)
                //############################################################################################
                // Get updated player turn
                let nextPlayer =
                    match (st.playerTurn) >= st.numOfPlayers with
                    | true -> 1u
                    | false -> (st.playerTurn + 1u)

                //Update the state with new values
                let st' = { st with playerTurn = nextPlayer }
                aux st' (nextPlayer = st.playerNumber)

            | RCM(CMChangeSuccess(newPieces)) ->
                //debugPrint ("\n\n\n\n ################## PLAYER CHANGED HAND ###################### \n\n\n\n")
                //############################################################################################################################
                (* The plater changed a piece on their hand. This happens when we can't find any words with the pieces already on the hand. *)
                //############################################################################################################################
                // Get updated player turn
                let nextPlayer = getNextPlayer

                //The CMCHangeSuccess message returns (uint32, uint32) of the piece ID and count of different tiles given to the player after the swap
                // Since we only ever swap one piece at a time, we can just ignore the count and focus on the piece id.
                let (pieceToSwapIn, amount) = newPieces.Head
                //Update the hand by exchanging the first piece on the hand for the piece returned from the success message.
                let updatedHand = exchangePieceHand st pieceToSwapIn
                //Update the state
                let st' =
                    { st with
                        playerTurn = nextPlayer
                        hand = updatedHand }

                aux st' (nextPlayer = st.playerNumber)


            | RCM(CMPlayFailed(pid, ms)) ->
                (* Failed play. Update your state *)
                //UDPDATE THE PLAYER NUMBER:

                let nextPlayer = getNextPlayer

                let st' = { st with playerTurn = nextPlayer }

                aux st' (nextPlayer = st.playerNumber)
            | RCM(CMGameOver _) -> ()
            | RCM a -> failwith (sprintf "not implmented: %A" a)
            | RGPE err ->

                //First update the turn,
                let nextPlayer = getNextPlayer

                //If we end up here, it might be because we try to swap a piece when there are no pieces left to swap the ones on our hand with.

                //First we check if we have recieved a GPENotEnoughPieces error
                let hasNotEnoughPiecesError: GameplayError option =
                    List.tryFind
                        (function
                        | GPENotEnoughPieces(_, _) -> true
                        | _ -> false)
                        err

                //If we have, that means there are no more pieces to swap. We update a boolean on the state indicating this.
                //(I've just realised as I wrote this comment that I can just look up how many pieces are in scrabble, and that a cleaner solution could have been just keeping a count on how many pieces have been played in total (which we already kind of have with the lettersOnBoard map.)
                match hasNotEnoughPiecesError with
                | Some(GPENotEnoughPieces(attemptedExchangedPieces, availablePieces)) ->
                    //There are no pieces available, so we can't extchange our hand. We set the boolean value. This means that next time we want to swap pieces, we'll pass instead, since there is nothing else to do.
                    let st' =
                        { st with
                            lastSeenRemainingRemotePiecesStatus = false }

                    aux st' (nextPlayer = st.playerNumber)
                | _ ->
                    //IS _ too broad...? This shouldn't happen, but not great....
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
            playGame cstream tiles (State.mkState lettersOnBoard dict playerNumber handSet numPlayers playerTurn true)
