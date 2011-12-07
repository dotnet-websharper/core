// $begin{copyright}
// 
// This file is part of WebSharper
// 
// Copyright (c) 2008-2011 IntelliFactory
// 
// GNU Affero General Public License Usage
// WebSharper is free software: you can redistribute it and/or modify it under
// the terms of the GNU Affero General Public License, version 3, as published
// by the Free Software Foundation.
//
// WebSharper is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
// for more details at <http://www.gnu.org/licenses/>.
//
// If you are unsure which license is appropriate for your use, please contact
// IntelliFactory at http://intellifactory.com/contact.
//
// $end{copyright}

module IntelliFactory.WebSharper.Sitelets.UrlEncoding

let isUnreserved c =
    match c with
    | '-' | '_' | '.' -> true
    | c when c >= 'A' && c <= 'Z' -> true
    | c when c >= 'a' && c <= 'z' -> true
    | c when c >= '0' && c <= '9' -> true
    | _ -> false

let writeEscaped (w: System.Text.StringBuilder) c =
    let k = int c
    if isUnreserved c then w.Append c
    elif k < 256 then w.AppendFormat("~{0:x2}", k)
    else w.AppendFormat("~u{0:x4}", k)
    |> ignore

exception NoFormatError of System.Type with
    override this.ToString() =
        System.String.Format(
            "Could not derive URL formatting for: {0}", this.Data0)

let inline ( ++ ) (a: int) (b: int) = (a <<< 4) + b

[<Literal>]
let EOF = -1

[<Literal>]
let ERROR = -2

let readEscaped (r: System.IO.TextReader) =
    let hex x =
        match x with
        | x when x >= int '0' && x <= int '9' -> x - int '0'
        | x when x >= int 'a' && x <= int 'f' -> x - int 'a' + 10
        | x when x >= int 'A' && x <= int 'F' -> x - int 'A' + 10
        | _ -> ERROR
    match r.Read() with
    | x when x = int '~' ->
        match r.Read() with
        | x when x = int 'u' ->
            let a = r.Read()
            let b = r.Read()
            let c = r.Read()
            let d = r.Read()
            if a >= 0 && b >= 0 && c >= 0 && d >= 0 then
                hex a ++ hex b ++ hex c ++ hex d
            else ERROR
        | x ->
            let y = r.Read()
            if x >= 0 && y >= 0 then
                hex x ++ hex y
            else ERROR
    | x ->
        x

type System.Text.StringBuilder with

    member this.Add(c: char) =
        this.Append c |> ignore

    member this.Add(s: string) =
        this.Append s |> ignore

    member this.Flush() =
        let s = this.ToString()
        this.Remove(0, this.Length) |> ignore
        s

type Reflection.UnionCaseInfo with
    member this.CustomizedName =
        let aT = typeof<CompiledNameAttribute>
        match this.GetCustomAttributes aT with
        | [| :? CompiledNameAttribute as attr |] -> attr.CompiledName
        | _ -> this.Name

let flags =
    System.Reflection.BindingFlags.Public
    ||| System.Reflection.BindingFlags.NonPublic
    ||| System.Reflection.BindingFlags.Static
    ||| System.Reflection.BindingFlags.Instance

type ISequenceProcessor =
    abstract member ToSequence : obj -> seq<obj>
    abstract member FromSequence : seq<obj> -> obj

type ListProcessor<'T>() =
    interface ISequenceProcessor with
        member this.ToSequence (x: obj) = Seq.map box (x :?> list<'T>)
        member this.FromSequence (s: seq<obj>) = box [for x in s -> x :?> 'T]

type S = System.Text.StringBuilder -> obj -> bool

let getS (getS: System.Type -> S) (t: System.Type) : S =
    let writeTuple r (ss: S []) : S =
        fun w x ->
            let xs : _ [] = r x
            if xs.Length >= 1 then
                ss.[0] w xs.[0] &&
                seq { 1 .. xs.Length - 1 }
                |> Seq.forall (fun  i ->
                    w.Add '/'
                    ss.[i] w xs.[i])
            else
                true
    if t = typeof<bool> || t = typeof<int> || t = typeof<float> then
        fun w x -> w.Add (string x); true
    elif t = typeof<string> then
        fun w x ->
            if x <> null then
                for c in string x do
                    writeEscaped w c
                true
            else false
    elif t = typeof<System.DateTime> then
        fun w x -> w.Add((x :?> System.DateTime).ToString "o"); true
    elif t.IsArray then
        let eT = t.GetElementType()
        let eS = getS eT
        if t.GetArrayRank() = 1 then
            fun w x ->
                let s = x :?> System.Array
                w.Add (string s.Length)
                Seq.cast s
                |> Seq.forall (fun x ->
                    w.Add '/'
                    eS w x)
        else
            raise (NoFormatError t)
    elif t.IsGenericType &&
         t.GetGenericTypeDefinition() = typedefof<list<_>> then
        let eT = t.GetGenericArguments().[0]
        let eS = getS eT
        fun w x ->
            let s : seq<obj> = Seq.cast (x :?> _)
            w.Add (string (Seq.length s))
            s
            |> Seq.forall (fun x ->
                w.Add '/'
                eS w x)
    elif Reflection.FSharpType.IsTuple t then
        let e = Reflection.FSharpType.GetTupleElements t
        let r = Reflection.FSharpValue.PreComputeTupleReader t
        writeTuple r (Array.map getS e)
    elif Reflection.FSharpType.IsRecord(t, flags) then
        let r  = Reflection.FSharpValue.PreComputeRecordReader(t, flags)
        let fs = Reflection.FSharpType.GetRecordFields t
        writeTuple r [| for f in fs -> getS f.PropertyType |]
    elif Reflection.FSharpType.IsUnion(t, flags) then
        let tR = Reflection.FSharpValue.PreComputeUnionTagReader(t, flags)
        let cs = Reflection.FSharpType.GetUnionCases(t, flags)
        let uR x = Reflection.FSharpValue.PreComputeUnionReader(x, flags)
        let ss =
            [|
                for c in cs ->
                    writeTuple (uR c) [|
                        for f in c.GetFields() ->
                            getS f.PropertyType
                    |]
            |]
        let ns = [| for c in cs -> c.CustomizedName |]
        let es = [| for c in cs -> c.GetFields().Length > 0 |]
        fun w x ->
            let t = tR x
            w.Add ns.[t]
            if es.[t] then
                w.Add '/'
                ss.[t] w x
            else
                true
    else
        raise (NoFormatError t)

type D = (unit -> option<string>) -> option<obj>

let getD (getD: System.Type -> D) (t: System.Type) : D =
    let tryParse p : D =
        fun r ->
            match r () with
            | Some s ->
                match p s with
                | true, x -> Some (x :> obj)
                | _ -> None
            | None -> None
    let parseTuple mk ds : D =
        let k = Array.length ds
        fun r ->
            let xs : obj [] = Array.create k null
            let rec loop x =
                match x with
                | i when i = k -> Some (mk xs)
                | i ->
                    match ds.[i] r with
                    | Some x ->
                        xs.[i] <- x
                        loop (i + 1)
                    | None -> None
            loop 0
    let parseArray eT (eD: D) : D =
        fun r ->
            match tryParse System.Int32.TryParse r with
            | Some (:? int as k) ->
                let data = System.Array.CreateInstance(eT, k)
                let rec loop x =
                    match x with
                    | i when i = k -> Some (box data)
                    | i ->
                        match eD r with
                        | Some obj ->
                            data.SetValue(obj, i)
                            loop (i + 1)
                        | None -> None
                if k >= 0 then loop 0 else None
            | _ -> None
    if t = typeof<bool> then
        tryParse System.Boolean.TryParse
    elif t = typeof<int> then
        tryParse System.Int32.TryParse
    elif t = typeof<float> then
        tryParse System.Double.TryParse
    elif t = typeof<string> then
        let buf = System.Text.StringBuilder()
        fun r ->
            let s = r ()
            match s with
            | Some s ->
                use i = new System.IO.StringReader(s)
                let rec loop () =
                    match readEscaped i with
                    | ERROR -> None
                    | EOF -> Some (box (buf.Flush()))
                    | x -> buf.Add (char x); loop ()
                loop ()
            | None -> None
    elif t = typeof<System.DateTime> then
        let rT = System.Globalization.DateTimeStyles.RoundtripKind
        tryParse <| fun x -> System.DateTime.TryParse(x, null, rT)
    elif t.IsArray then
        if t.GetArrayRank() > 1 then
            raise (NoFormatError t)
        let eT = t.GetElementType()
        parseArray eT (getD eT)
    elif t.IsGenericType &&
         t.GetGenericTypeDefinition() = typedefof<list<_>> then
        let eT = t.GetGenericArguments().[0]
        let eD = getD eT
        let sP =
            typedefof<ListProcessor<_>>.MakeGenericType(eT)
            |> System.Activator.CreateInstance :?> ISequenceProcessor
        fun f ->
            parseArray eT eD f
            |> Option.map (fun x -> sP.FromSequence (x :?> _))
    elif Reflection.FSharpType.IsTuple t then
        let e = Reflection.FSharpType.GetTupleElements t
        let c = Reflection.FSharpValue.PreComputeTupleConstructor t
        parseTuple c (Array.map getD e)
    elif Reflection.FSharpType.IsRecord(t, flags) then
        let c = Reflection.FSharpValue.PreComputeRecordConstructor(t, flags)
        let fs = Reflection.FSharpType.GetRecordFields t
        parseTuple c [| for f in fs -> getD f.PropertyType |]
    elif Reflection.FSharpType.IsUnion(t, flags) then
        let cs = Reflection.FSharpType.GetUnionCases(t, flags)
        let uC x = Reflection.FSharpValue.PreComputeUnionConstructor(x, flags)
        let d = System.Collections.Generic.Dictionary()
        let ds =
            [|
                for c in cs ->
                    d.[c.CustomizedName] <- d.Count
                    parseTuple (uC c) [|
                        for f in c.GetFields() ->
                            getD f.PropertyType
                    |]
            |]
        fun r ->
            let name = r ()
            match name with
            | Some name ->
                match d.TryGetValue name with
                | false, _ -> None
                | true, k -> ds.[k] r
            | None -> None
    else
        raise (NoFormatError t)

let memoFix delay f =
    let cache = System.Collections.Generic.Dictionary()
    let rec g x =
        if cache.ContainsKey x then
            match cache.[x] with
            | None -> delay (fun () -> cache.[x].Value)
            | Some x -> x
        else
            cache.[x] <- None
            let r = f g x
            cache.[x] <- Some r
            r
    g

type Format<'T> =
    {
        read : string -> option<obj>
        show : obj -> option<string>
    }

    member this.Read x =
        match this.read x with
        | Some (:? 'T as r) -> Some r
        | _ -> None

    member this.Show(x: 'T) =
        this.show (x :> obj)

[<Sealed>]
type Factory() =
    let delay f = fun x -> f () x
    let getS : System.Type -> S = memoFix delay getS
    let getD : System.Type -> D = memoFix delay getD
    let sb = System.Text.StringBuilder 128

    member this.GetFormatFor (t: System.Type) : Format<obj> =
        let d = getD t
        let s = getS t
        {
            read = fun input ->
                if input = null then None else
                    let parts = input.Split '/'
                    let e = (parts :> seq<string>).GetEnumerator()
                    let n () = if e.MoveNext() then Some e.Current else None
                    d n
            show = fun value ->
                if s sb value then Some (sb.Flush()) else None
        }

    member this.GetFormat<'T>() : Format<'T> =
        let fmt = this.GetFormatFor typeof<'T>
        {
            show = fmt.show
            read = fmt.read
        }

    static member Create() = Factory()

let GetFormat<'T> ()=
    Factory.Create().GetFormat<'T>()

let GetFormatFor t =
    Factory.Create().GetFormatFor t
