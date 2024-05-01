namespace ScrabbleBot

open ScrabbleUtil
open ScrabbleUtil.ServerCommunication

open System.IO

open ScrabbleUtil.DebugPrint

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
          playerTurn: uint32 }

    let mkState (boardMap: Map<coord, (uint32 * (char * int))>) d pn h ps pt =
        { lettersOnBoard = boardMap
          dict = d
          playerNumber = pn
          hand = h
          numOfPlayers = ps
          playerTurn = pt }

    let lettersOnBoard st = st.lettersOnBoard
    let dict st = st.dict
    let playerNumber st = st.playerNumber
    let numOfPlayers st = st.numOfPlayers
    let hand st = st.hand
    let playerTurn st = st.playerTurn

module FindWord =
    open ScrabbleUtil
    open MultiSet

    let charFromUint32 (charIndex: uint32) =
        let list = [ 'a' .. 'z' ]
        let listWithWildcard = [ '_' ] @ list
        listWithWildcard.[int (charIndex)]
    let rec stepRecursively (availableLetters: MultiSet.MultiSet<uint32>) (playedLetters: MultiSet.MultiSet<uint32>) (dict: ScrabbleUtil.Dictionary.Dict) =
        availableLetters 
        |> MultiSet.fold (fun _ charId charAmount -> 
            match Dictionary.step (charFromUint32 charId) dict with
            | None -> None
            | Some (isWord, newDict) ->
                match isWord with 
                | true -> Some playedLetters
                | false -> 
                    let newAvailableLetters =  availableLetters |> MultiSet.removeSingle charId
                    let newPlayedLetters = playedLetters |> MultiSet.addSingle charId
                    stepRecursively newAvailableLetters newPlayedLetters newDict) (None) 
    
        // Define the move type
    type Move = (coord * (uint32 * (char * int))) list

     // Define the function to generate a move
    let generateMove (hand: MultiSet.MultiSet<uint32>) (dict: ScrabbleUtil.Dictionary.Dict) (board: Map<coord, (uint32 * (char * int))>) =
        let playedLetters = MultiSet<uint32>.empty
        let move = []
        let word = stepRecursively hand playedLetters dict  
        match word with 
        | None -> failwith "Couldnt find word"
        | Some (foundWord: MultiSet.MultiSet<uint32>) -> 
            word
            |> MultiSet.fold (fun coordAcc charId _ ->
                coordAcc (charId()) ((0,0)))

          


    
            
                


module Scrabble =
    open System.Threading

    let playGame cstream pieces (st: State.state) =

        let rec aux (st: State.state) (thisPlayersTurn: bool) =

            if (thisPlayersTurn) then

                // ##### THIS PLAYER'S TURN #####

                // First we check if this is the first move



                Print.printHand pieces (State.hand st)
                // remove the force print when you move on from manual input (or when you have learnt the format)
                forcePrint
                    "Input move (format '(<x-coordinate> <y-coordinate> <piece id><character><point-value> )*', note the absence of space between the last inputs)\n\n"

                debugPrint (sprintf "It's your turn, player %d. \n" (State.playerNumber st))


            let input = System.Console.ReadLine()
            let move: (coord * (uint32 * (char * int))) list = FindWord.

            debugPrint (sprintf "Player %d -> Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.
            send cstream (SMPlay move)

            let msg = recv cstream
            debugPrint (sprintf "Player %d <- Server:\n%A\n" (State.playerNumber st) move) // keep the debug lines. They are useful.


            match msg with
            | RCM(CMPlaySuccess(ms, points, newPieces)) ->
                (* Successful play by you. Update your state (remove old tiles, add the new ones, change turn, etc) *)

                // UPDATE THE HAND:
                // Extract piece IDs from each move
                let playedPieceIds = ms |> List.map (fun (_, (uid: uint32, (_, _))) -> uid)

                // make new list with played pieces from pieces on hand
                let handWithRemovedPieces =
                    playedPieceIds
                    |> List.fold (fun hand (pieceId: uint32) -> MultiSet.removeSingle pieceId hand) (State.hand st)
                // add the new pieces to this list
                let handWithAddedAndRemovedPieces =
                    newPieces
                    |> List.fold
                        (fun handWithRemovedPieces (pieceID: uint32, count: uint32) ->
                            MultiSet.add pieceID count handWithRemovedPieces)
                        (handWithRemovedPieces)
                // At the end, set the State hand to this list.

                //UPDATE THE BOARD:
                let playedPieces = ms |> List.map (fun (coord, (_, (char, _))) -> (coord, char))

                let updatedBoard =
                    List.fold
                        (fun acc (coord, ((char, points))) -> Map.add coord (char, points) acc)
                        st.lettersOnBoard
                        ms

                //printfn "Board State:"
                //updatedBoard
                //|> Map.iter (fun coord (pieceId, (char, points)) ->
                //    printfn "Coordinate (%s): %c (%d points)" (coord.ToString()) (char: char) points)


                //UDPDATE THE PLAYER NUMBER:
                let nextPlayer = (st.playerNumber % st.numOfPlayers) + 1u //OBS! Should there be +1u here?

                // UPDATE THE STATE WITH THE NEW INFO
                let st' =
                    { st with
                        hand = handWithAddedAndRemovedPieces
                        playerTurn = nextPlayer
                        lettersOnBoard = updatedBoard

                    } // Update the state with the new hand, board, player turn

                // Continue the game with the updated state
                aux st' (st.playerTurn = st.playerNumber)

            | RCM(CMPlayed(pid, ms, points)) ->
                (* Successful play by other player. Update your state *)

                //UPDATE THE BOARD:
                let playedPieces = ms |> List.map (fun (coord, (_, (char, _))) -> (coord, char))

                let updatedBoard =
                    List.fold
                        (fun acc (coord, ((char, points))) -> Map.add coord (char, points) acc)
                        st.lettersOnBoard
                        ms

                //UDPDATE THE PLAYER NUMBER:
                let nextPlayer = (st.playerNumber % st.numOfPlayers) + 1u //OBS! Should there be +1u here?

                let st' =
                    { st with
                        playerTurn = nextPlayer
                        lettersOnBoard = updatedBoard } // Update the state with the new board, player turn

                aux st' (st.playerTurn = st.playerNumber)
            | RCM(CMPlayFailed(pid, ms)) ->
                (* Failed play. Update your state *)
                //UDPDATE THE PLAYER NUMBER:
                let nextPlayer = (st.playerNumber % st.numOfPlayers) + 1u //OBS! Should there be +1u here?
                let st' = { st with playerTurn = nextPlayer } //Update the player turn

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

        fun () -> playGame cstream tiles (State.mkState lettersOnBoard dict playerNumber handSet numPlayers playerTurn)
