module internal Util

let listOfPiecesToPlainString (word: (uint32 * (char * int)) list) =
    word |> List.map (fun (_, (letter, _)) -> string letter) |> String.concat ""

let charFromUint32 (charIndex: uint32) =
    let list = [ 'a' .. 'z' ]
    let listWithWildcard = '_' :: list
    listWithWildcard.[int (charIndex)]

let uint32FromChar (char: char) =
    let list = [ 'a' .. 'z' ]
    let listWithWildcard = '_' :: list

    // Check if the character exists
    let index = List.tryFindIndex (fun x -> x = char) listWithWildcard
    index

//This funtion adds all the letters of the alphabet to a MultiSet of characters. it is used to update the available letters when we have a wildcard piece on hand.
// This function is a waste of time, just set the available letters to the set of all letters. (MultiSet<char>)
let addWildcardLetters () =
    let letters = [ 'A' .. 'Z' ]

    letters
    |> List.fold (fun (acc: MultiSet.MultiSet<char>) letter -> MultiSet.addSingle letter acc) MultiSet.empty
