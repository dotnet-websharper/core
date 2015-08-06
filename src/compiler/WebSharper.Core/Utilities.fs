[<AutoOpen>]
module WebSharper.Core.Utilities
                  

//[<RequireQualifiedAccess; CustomEquality; CustomComparison>]
//type Address =
//    | Empty
//    | Cons of string * Address * int
//
//    override this.GetHashCode() =
//        match this with
//        | Empty -> 0
//        | Cons (_, _, h) -> h
//
//    override this.Equals(other: obj) : bool =
//        match other with
//        | :? Address as o ->
//            let rec equals l1 l2 =
//                obj.ReferenceEquals(l1, l2) || (
//                    match l1 with
//                    | Cons (h1, t1, s1) ->
//                        match l2 with
//                        | Cons (h2, t2, s2) ->
//                            s1 = s2 && (h1 = h2 && equals t1 t2)
//                        | _ -> false
//                    | _ -> false
//                )
//            equals this o
//        | _ -> failwith "invalid equality check"
//
//    interface System.IComparable with
//        member this.CompareTo (other: obj) =
//            match other with
//            | :? Address as o ->
//                match this, o with
//                | Empty, Empty -> 0
//                | Empty, _ -> -1
//                | _, Empty -> 1
//                | Cons (h1, t1, _), Cons (h2, t2, _) ->
//                    match compare h1 h2 with
//                    | 0 -> compare t1 t2
//                    | c -> c  
//            | _ -> failwith "invalid comparison"
//
//    override this.ToString() =
//        match this with
//        | Empty -> ""
//        | Cons(v, Empty, _) -> v 
//        | Cons(v, t, _) -> t.ToString() + "." + v
//    
//    member this.Head =
//        match this with
//        | Empty -> ""
//        | Cons(v, _, _) -> v
//
//    static member Singleton v =
//        Cons (v, Empty, hash v)
//        
//    static member Create (v, t) =
//        Cons (v, t, hash v + 5 * hash t)
//
//    static member FromList l =
//        let rec addr l =
//            match l with 
//            | [] -> Empty
//            | h :: t -> Address.Create(h, addr t)
//        l |> List.rev |> addr
//
//type CachedRecords<'T when 'T : equality>() =
//    let cache = System.Collections.Generic.Dictionary<Hashed<'T>, Hashed<'T>>()
//
//    member this.Get value =
//        let h = hashed value
//        match cache.TryGetValue h with
//        | true, x -> x
//        | _ ->
//            cache.Add(h, h)
//            h

//type IValue<'T> =
//    abstract Value : 'T with get, set
//
//type DictEntry<'K, 'V>(dict : System.Collections.Generic.IDictionary<'K, 'V>, key) =
//    interface IValue<'V> with
//        member this.Value 
//            with get() = dict.[key]
//            and  set v = dict.[key] <- v 
//
//type ReadOnlyValue<'T>(value) =
//    interface IValue<'T> with
//        member this.Value 
//            with get() = value
//            and  set v = failwith "This value is read-only."
//
module Seq =
    open System.Linq

    let inline equals (s1: _ seq) (s2: _ seq) = s1.SequenceEqual(s2)

module Option =
    let fill value option = defaultArg option value
    let ofObj value = if obj.ReferenceEquals(value, null) then None else Some value
    let toObj option = match option with Some x -> x | _ -> null
//    let fallback getValue option = match option with Some x -> x | _ -> getValue()
//    let mapFill mapping value option = match option with Some x -> mapping x | _ -> value
//    let mapFallback mapping getValue option = match option with Some x -> mapping x | _ -> getValue()

open System
open System.Globalization

type System.Collections.Generic.IDictionary<'K,'V> with
    member this.TryFind(key) =
        let mutable value = Unchecked.defaultof<'V>
        if this.TryGetValue(key, &value) then Some value else None

type System.Int32 with
    static member FromString text =
        let ic = CultureInfo.InvariantCulture
        match Int32.TryParse(text, NumberStyles.Any, ic) with
        | true, x -> Some x
        | _ -> None

type System.Int64 with
    static member FromString text =
        let ic = CultureInfo.InvariantCulture
        match Int64.TryParse(text, NumberStyles.Any, ic) with
        | true, x -> Some x
        | _ -> None

type System.Double with
    static member FromString text =
        let ic = CultureInfo.InvariantCulture
        match Double.TryParse(text, NumberStyles.Any, ic) with
        | true, x -> Some x
        | _ -> None
