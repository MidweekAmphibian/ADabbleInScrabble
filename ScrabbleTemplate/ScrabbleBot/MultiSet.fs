// Insert your MultiSet.fs file here. All modules must be internal

module internal MultiSet

type MultiSet<'a when 'a: comparison> = MS of Map<'a, uint32>

let empty = MS(Map.empty)

let isEmpty (MS(s)) = Map.isEmpty (s)

let size (MS(s): MultiSet<'a>) =
    Map.fold (fun acc _ value -> acc + value) 0u s

let contains (a: 'a) (MS(s): MultiSet<'a>) = s |> Map.containsKey a

let numItems (a: 'a) (MS(s): MultiSet<'a>) =
    Map.tryFind a s |> Option.defaultValue (0u)

let add (a: 'a) (n: uint32) (MS(s): MultiSet<'a>) = MS(s |> Map.add a n)

let addSingle (a: 'a) (MS(s): MultiSet<'a>) = MS(s |> Map.add a 1u)

let remove (a: 'a) (n: uint32) (MS(s): MultiSet<'a>) : MultiSet<'a> =
    let occurrences = numItems a (MS(s))

    if (occurrences > n) then
        MS(s |> Map.add a (occurrences - n))
    else
        MS(s |> Map.remove a)

let removeSingle (a: 'a) (MS(s): MultiSet<'a>) : MultiSet<'a> = remove a 1u (MS(s))


let fold (fold: 'b -> 'a -> uint32 -> 'b) (x: 'b) (MS(s): MultiSet<'a>) = Map.fold fold x s
let foldBack (foldback: 'a -> uint32 -> 'b -> 'b) (MS(s): MultiSet<'a>) (x: 'b) = Map.foldBack foldback s x

let ofList (_: 'a list) : MultiSet<'a> = MS(Map.empty)
let toList (_: MultiSet<'a>) : 'a list = []


let map (_: 'a -> 'b) (_: MultiSet<'a>) : MultiSet<'b> = MS(Map.empty)

let union (_: MultiSet<'a>) (_: MultiSet<'a>) : MultiSet<'a> = MS(Map.empty)
let sum (_: MultiSet<'a>) (_: MultiSet<'a>) : MultiSet<'a> = MS(Map.empty)

let subtract (_: MultiSet<'a>) (_: MultiSet<'a>) : MultiSet<'a> = MS(Map.empty)

let intersection (_: MultiSet<'a>) (_: MultiSet<'a>) : MultiSet<'a> = MS(Map.empty)
