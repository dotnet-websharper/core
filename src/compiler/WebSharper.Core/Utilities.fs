[<AutoOpen>]
module WebSharper.Core.Utilities
                  
//[<CustomEquality; CustomComparison; Struct>]
//[<System.Diagnostics.DebuggerDisplay("{Value}")>]
type Hashed<'T when 'T : equality and 'T : comparison> =
    val Value : 'T
    val Hash : int 

    new v = { Value = v; Hash = hash v }

    override this.GetHashCode() =
        this.Hash

//    override this.ToString() = this.Value.ToString()
    
    override this.Equals(other: obj) : bool =
        match other with
        | :? Hashed<'T> as o ->
            obj.ReferenceEquals(this, o) || (
                this.Hash = o.Hash && (
                    let v1 = this.Value
                    let v2 = o.Value
                    obj.ReferenceEquals(v1, v2) || v1 = v2
                )
            )
        | _ -> failwith "invalid equality check"

    interface System.IComparable with
        member this.CompareTo (other: obj) =
            match other with
        | :? Hashed<'T> as o ->
            compare this.Value o.Value
        | _ -> failwith "invalid comparison"        

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
    let tryHead (s: _ seq) = 
        use e = s.GetEnumerator()
        if e.MoveNext() then Some e.Current else None

module Option =
    let fill value option = defaultArg option value
    let fallback getValue option = match option with Some x -> x | _ -> getValue()
    let ofObj value = if obj.ReferenceEquals(value, null) then None else Some value
    let toObj option = match option with Some x -> x | _ -> null
//    let mapFill mapping value option = match option with Some x -> mapping x | _ -> value
//    let mapFallback mapping getValue option = match option with Some x -> mapping x | _ -> getValue()

module List =
    let mapFold folder state list =
        let mutable state = state
        let mutable res = []
        for item in list do
            let mapped, stNext = folder state item
            state <- stNext
            res <- mapped :: res
        List.rev res, state

open System
open System.Globalization

type System.Collections.Generic.IDictionary<'K,'V> with
    member this.TryFind(key) =
        let mutable value = Unchecked.defaultof<'V>
        if this.TryGetValue(key, &value) then Some value else None

type Int32 with
    static member FromString text =
        let ic = CultureInfo.InvariantCulture
        match Int32.TryParse(text, NumberStyles.Any, ic) with
        | true, x -> Some x
        | _ -> None

type Int64 with
    static member FromString text =
        let ic = CultureInfo.InvariantCulture
        match Int64.TryParse(text, NumberStyles.Any, ic) with
        | true, x -> Some x
        | _ -> None

type Double with
    static member FromString text =
        let ic = CultureInfo.InvariantCulture
        match Double.TryParse(text, NumberStyles.Any, ic) with
        | true, x -> Some x
        | _ -> None
    
open System.Collections.Generic

module Dict =

    let addToMulti (d: IDictionary<_,_>) k v =
        match d.TryGetValue k with
        | true, p -> d.[k] <- v :: p
        | _ -> d.Add(k, [v])    

    let getFromMulti (d: IDictionary<_,_>) k =
        match d.TryGetValue k with
        | true, vs -> vs
        | _ -> []

    let union (dicts: seq<IDictionary<_,_>>) =
        let d = Dictionary() :> IDictionary<_,_>
        for s in dicts do
            for i in s do d.Add(i)
        d 
  
    let swap (d: IDictionary<_,_>) =
        let r = Dictionary() :> IDictionary<_,_>
        for KeyValue(k, v) in d do   
            r.Add(v, k)
        r
    
    let tryFindSameIn2 (d1: IDictionary<_, _>) (d2: IDictionary<_, _>) key =
        let mutable value = Unchecked.defaultof<_>
        if d1.TryGetValue(key, &value) || d2.TryGetValue(key, &value) then
            Some value
        else None     

    let tryFindIn2 (d1: IDictionary<_, _>) (d2: IDictionary<_, _>) key =
        let mutable value1 = Unchecked.defaultof<_>
        let mutable value2 = Unchecked.defaultof<_>
        if d1.TryGetValue(key, &value1) then
            Some (Choice1Of2 value1)
        elif d2.TryGetValue(key, &value2) then
            Some (Choice2Of2 value2)
        else None     


open System.Linq  
      
let inline private notSupported() = raise (System.NotSupportedException())
             
type MergedDictionary<'TKey, 'TValue when 'TKey: equality>(orig: IDictionary<'TKey, 'TValue>, current: IDictionary<'TKey, 'TValue>) =   
    
    new orig = MergedDictionary(orig, Dictionary())

    member x.Add(key: 'TKey, value: 'TValue): unit = 
        current.Add(key, value)

    member x.ContainsKey(key: 'TKey): bool = 
        orig.ContainsKey key || current.ContainsKey key

    member x.Count: int = 
        orig.Count + current.Count
 
    member x.Item
        with get (key: 'TKey): 'TValue = 
            match orig.TryGetValue key with
            | true, value -> value
            | _ ->
            match current.TryGetValue key with
            | true, value -> value
            | _ -> raise (KeyNotFoundException())
        and set (key: 'TKey) (v: 'TValue): unit = 
            if orig.ContainsKey key then
                invalidArg "key" "Key is found in immutable part of MergedDictionary"
            else current.[key] <- v
 
    member x.Keys: seq<'TKey> = 
        Seq.append orig.Keys current.Keys

    member x.TryGetValue(key: 'TKey, value: 'TValue byref): bool = 
        orig.TryGetValue(key, &value) || current.TryGetValue(key, &value)

    member this.TryFind(key: 'TKey) =
        let mutable value = Unchecked.defaultof<'TValue>
        if this.TryGetValue(key, &value) then Some value else None

    member x.Values: seq<'TValue> = 
        Seq.append orig.Values current.Values

    member this.Original = orig
    member this.Current = current

    interface IDictionary<'TKey, 'TValue> with
        member x.Add(key: 'TKey, value: 'TValue): unit = 
            x.Add(key, value)
        
        member x.Add(item: KeyValuePair<'TKey,'TValue>): unit = 
            current.Add(item)
        
        member x.Clear(): unit = 
            notSupported()
        
        member x.Contains(item: KeyValuePair<'TKey,'TValue>): bool = 
            orig.Contains item || current.Contains item
        
        member x.ContainsKey(key: 'TKey): bool = 
            x.ContainsKey key
        
        member x.CopyTo(array: KeyValuePair<'TKey,'TValue> [], arrayIndex: int): unit = 
            notSupported()
        
        member x.Count: int = 
            x.Count
        
        member x.GetEnumerator(): IEnumerator<KeyValuePair<'TKey,'TValue>> = 
            (Seq.append orig current).GetEnumerator()
        
        member x.GetEnumerator(): System.Collections.IEnumerator = 
            (Seq.append orig current :> System.Collections.IEnumerable).GetEnumerator()
        
        member x.IsReadOnly: bool = 
            false
        
        member x.Item
            with get (key: 'TKey): 'TValue = 
                x.[key]
            and set (key: 'TKey) (v: 'TValue): unit = 
                x.[key] <- v
        
        member this.Keys: ICollection<'TKey> = 
            { new ICollection<'TKey> with
                  member x.Add(item: 'TKey): unit = 
                      notSupported()
                  
                  member x.Clear(): unit = 
                      notSupported()
                  
                  member x.Contains(item: 'TKey): bool = 
                      this.ContainsKey item
                  
                  member x.CopyTo(array: 'TKey [], arrayIndex: int): unit = 
                      notSupported()
                  
                  member x.Count: int = 
                      this.Count
                  
                  member x.GetEnumerator(): IEnumerator<'TKey> = 
                      this.Keys.GetEnumerator()
                  
                  member x.GetEnumerator(): System.Collections.IEnumerator = 
                      (this.Keys :> System.Collections.IEnumerable).GetEnumerator()
                  
                  member x.IsReadOnly: bool = 
                      true
                  
                  member x.Remove(item: 'TKey): bool = 
                      notSupported()
            }
        
        member x.Remove(key: 'TKey): bool = 
            notSupported()
        
        member x.Remove(item: KeyValuePair<'TKey,'TValue>): bool = 
            notSupported()
        
        member x.TryGetValue(key: 'TKey, value: byref<'TValue>): bool = 
            x.TryGetValue(key, &value)
        
        member this.Values: ICollection<'TValue> = 
            { new ICollection<'TValue> with
                  member x.Add(item: 'TValue): unit = 
                      notSupported()
                  
                  member x.Clear(): unit = 
                      notSupported()
                  
                  member x.Contains(item: 'TValue): bool = 
                      this.Values.Contains(item)
                  
                  member x.CopyTo(array: 'TValue [], arrayIndex: int): unit = 
                      notSupported()
                  
                  member x.Count: int = 
                      this.Count
                  
                  member x.GetEnumerator(): IEnumerator<'TValue> = 
                      this.Values.GetEnumerator()
                  
                  member x.GetEnumerator(): System.Collections.IEnumerator = 
                      (this.Values :> System.Collections.IEnumerable).GetEnumerator()
                  
                  member x.IsReadOnly: bool = 
                      true
                  
                  member x.Remove(item: 'TValue): bool = 
                      notSupported()
            }
