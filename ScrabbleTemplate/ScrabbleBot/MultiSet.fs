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

let add (a: 'a) (n: uint32) (MS(s): MultiSet<'a>) =
    match Map.tryFind a s with
    | Some count -> MS(Map.add a (count + n) s)
    | None -> MS(Map.add a n s)


let addSingle (a: 'a) (MS(s): MultiSet<'a>) =
    match Map.tryFind a s with
    | Some count -> MS(Map.add a (count + 1u) s)
    | None -> MS(Map.add a 1u s)

let remove (a: 'a) (n: uint32) (MS(s): MultiSet<'a>) : MultiSet<'a> =
    let occurrences = numItems a (MS(s))

    if (occurrences > n) then
        MS(s |> Map.add a (occurrences - n))
    else
        MS(s |> Map.remove a)

let removeSingle (a: 'a) (MS(s): MultiSet<'a>) : MultiSet<'a> = remove a 1u (MS(s)) //OBS?


let fold f acc (MS(s)) = Map.fold f acc s
let foldBack (foldback: 'a -> uint32 -> 'b -> 'b) (MS(s): MultiSet<'a>) (x: 'b) = Map.foldBack foldback s x

let ofList list =
    List.fold (fun acc element -> addSingle element acc) empty list

let toList multiSet =
    foldBack (fun element count acc -> List.init (int32 count) (fun _ -> element) @ acc) multiSet []


let map (_: 'a -> 'b) (_: MultiSet<'a>) : MultiSet<'b> = MS(Map.empty)

let union (_: MultiSet<'a>) (_: MultiSet<'a>) : MultiSet<'a> = MS(Map.empty)
let sum (_: MultiSet<'a>) (_: MultiSet<'a>) : MultiSet<'a> = MS(Map.empty)

let subtract (_: MultiSet<'a>) (_: MultiSet<'a>) : MultiSet<'a> = MS(Map.empty)

let intersection (_: MultiSet<'a>) (_: MultiSet<'a>) : MultiSet<'a> = MS(Map.empty)
