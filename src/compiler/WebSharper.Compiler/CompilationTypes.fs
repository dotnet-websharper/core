// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2016 IntelliFactory
//
// Licensed under the Apache License, Version 2.0 (the "License"); you
// may not use this file except in compliance with the License.  You may
// obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied.  See the License for the specific language governing
// permissions and limitations under the License.
//
// $end{copyright}

namespace WebSharper.Compiler
  
open System.Collections.Generic

open WebSharper.Core
open WebSharper.Core.AST
open WebSharper.Core.Metadata

open WebSharper.Core.DependencyGraph

type CompilingMember =
    | NotCompiled of CompiledMember * notVirtual: bool * opts: Optimizations
    | NotGenerated of TypeDefinition * option<obj> * CompiledMember * notVirtual: bool * opts: Optimizations

module NotResolved =
    [<RequireQualifiedAccess>]
    type NotResolvedMemberKind = 
        | Instance
        | Abstract
        | Static
        | Constructor
        | Override of TypeDefinition
        | Implementation of TypeDefinition
        | Remote of RemotingKind * MethodHandle * option<TypeDefinition * option<obj>>
        | Inline
        | NoFallback

    type NotResolvedMethod =
        {
            mutable Kind : NotResolvedMemberKind
            StrongName : option<string>
            Macros: list<TypeDefinition * option<obj>>
            Generator : option<TypeDefinition * option<obj>>
            Compiled : bool
            Pure : bool
            mutable FuncArgs : option<list<FuncArgOptimization>>
            Args : list<Id>
            mutable Body : Expression
            Requires : list<TypeDefinition * option<obj>>
            Warn : option<string>
        }

    type NotResolvedField =
        {
            StrongName : option<string>
            IsStatic : bool
            IsOptional : bool 
            IsReadonly : bool
            FieldType : Type
        }

    [<RequireQualifiedAccess>]
    type NotResolvedMember =
        | Constructor of Constructor * NotResolvedMethod
        | Field of string * NotResolvedField
        | StaticConstructor of Expression
        | Method of Method * NotResolvedMethod

    [<RequireQualifiedAccess>]
    type NotResolvedClassKind =
        | Static
        | Class
        | WithPrototype
        | Stub

    type NotResolvedClass =
        {
            StrongName : option<string>
            BaseClass : option<TypeDefinition>
            Requires : list<TypeDefinition * option<obj>> 
            Members : list<NotResolvedMember>
            Kind : NotResolvedClassKind
            IsProxy : bool
            Macros : list<TypeDefinition * option<obj>> 
            ForceNoPrototype : bool
            ForceAddress : bool
        }

    type NotResolvedInterface =
        {
            StrongName : option<string>
            Extends : list<TypeDefinition>
            NotResolvedMethods : list<Method * option<string>>
        }

    type N = NotResolvedMemberKind
    type M = NotResolvedMember

    let hasWSPrototype ckind (baseCls: TypeDefinition option) cmembers =
        let nonObjBaseClass() = 
            match baseCls with
            | None -> false
            | Some td when td = Definitions.Obj -> false
            | _ -> true
        match ckind with
        | NotResolvedClassKind.Stub -> false
        | NotResolvedClassKind.Static -> nonObjBaseClass()
        | NotResolvedClassKind.WithPrototype -> true
        | _ ->
            nonObjBaseClass() ||
            cmembers
            |> Seq.exists (
                function
                | M.Constructor (_, nr)
                | M.Method (_, nr) ->
                    match nr.Kind with
                    | N.Instance 
                    | N.Abstract
                    | N.Constructor 
                    | N.Override _
                    | N.Implementation _ -> true
                    | _ -> false
                | _ -> false
            )

type internal MergedDictionary<'TKey, 'TValue when 'TKey: equality>(orig: IDictionary<'TKey, 'TValue>, current: IDictionary<'TKey, 'TValue>) =   
    
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

    member x.TryGetValue(key: 'TKey, value: byref<'TValue>): bool = 
        orig.TryGetValue(key, &value) || current.TryGetValue(key, &value)

    member this.TryFind(key: 'TKey) =
        let mutable value = Unchecked.defaultof<'TValue>
        if this.TryGetValue(key, &value) then Some value else None

    member x.Values: seq<'TValue> = 
        Seq.append orig.Values current.Values

    member x.ContainsValue(value) = 
        Seq.append orig.Values current.Values |> Seq.exists (Unchecked.equals value)

    member this.Original = orig
    member this.Current = current

    interface IDictionary<'TKey, 'TValue> with
        member x.Add(key: 'TKey, value: 'TValue): unit = 
            x.Add(key, value)
        
        member x.Add(item: KeyValuePair<'TKey,'TValue>): unit = 
            current.Add(item)
        
        member x.Clear(): unit = 
            raise (System.NotSupportedException())
        
        member x.Contains(item: KeyValuePair<'TKey,'TValue>): bool = 
            orig.Contains item || current.Contains item
        
        member x.ContainsKey(key: 'TKey): bool = 
            x.ContainsKey key
        
        member x.CopyTo(array: KeyValuePair<'TKey,'TValue> [], arrayIndex: int): unit = 
            raise (System.NotSupportedException())
        
        member x.Count: int = 
            x.Count
        
        member x.GetEnumerator(): IEnumerator<KeyValuePair<'TKey,'TValue>> = 
            (Seq.append orig current).GetEnumerator()
        
        member x.GetEnumerator(): System.Collections.IEnumerator = 
            (Seq.append orig current :> System.Collections.IEnumerable).GetEnumerator()
        
        member x.IsReadOnly: bool = false
        
        member x.Item
            with get (key: 'TKey): 'TValue = 
                x.[key]
            and set (key: 'TKey) (v: 'TValue): unit = 
                x.[key] <- v
        
        member this.Keys: ICollection<'TKey> = 
            { new ICollection<'TKey> with
                  member x.Add(item: 'TKey): unit = 
                      raise (System.NotSupportedException())
                  
                  member x.Clear(): unit = 
                      raise (System.NotSupportedException())
                  
                  member x.Contains(item: 'TKey): bool = 
                      this.ContainsKey item
                  
                  member x.CopyTo(array: 'TKey [], arrayIndex: int): unit = 
                      raise (System.NotSupportedException())
                  
                  member x.Count: int = 
                      this.Count
                  
                  member x.GetEnumerator(): IEnumerator<'TKey> = 
                      this.Keys.GetEnumerator()
                  
                  member x.GetEnumerator(): System.Collections.IEnumerator = 
                      (this.Keys :> System.Collections.IEnumerable).GetEnumerator()
                  
                  member x.IsReadOnly: bool = 
                      true
                  
                  member x.Remove(item: 'TKey): bool = 
                      raise (System.NotSupportedException())
            }
        
        member x.Remove(key: 'TKey): bool = 
            raise (System.NotSupportedException())
        
        member x.Remove(item: KeyValuePair<'TKey,'TValue>): bool = 
            raise (System.NotSupportedException())
        
        member x.TryGetValue(key: 'TKey, value: byref<'TValue>): bool = 
            x.TryGetValue(key, &value)
        
        member this.Values: ICollection<'TValue> = 
            { new ICollection<'TValue> with
                  member x.Add(item: 'TValue): unit = 
                      raise (System.NotSupportedException())
                  
                  member x.Clear(): unit = 
                      raise (System.NotSupportedException())
                  
                  member x.Contains(item: 'TValue): bool = 
                      this.ContainsValue(item)
                  
                  member x.CopyTo(array: 'TValue [], arrayIndex: int): unit = 
                      raise (System.NotSupportedException())
                  
                  member x.Count: int = 
                      this.Count
                  
                  member x.GetEnumerator(): IEnumerator<'TValue> = 
                      this.Values.GetEnumerator()
                  
                  member x.GetEnumerator(): System.Collections.IEnumerator = 
                      (this.Values :> System.Collections.IEnumerable).GetEnumerator()
                  
                  member x.IsReadOnly: bool = 
                      true
                  
                  member x.Remove(item: 'TValue): bool = 
                      raise (System.NotSupportedException())
            }

type internal MappedDictionary<'TKey, 'TOrigValue, 'TValue when 'TKey: equality>(orig: IDictionary<'TKey, 'TOrigValue>, mapping: 'TOrigValue -> 'TValue) =
    
    member x.ContainsKey(key) = orig.ContainsKey key

    member x.Count = orig.Count

    member x.Item 
        with get (key: 'TKey): 'TValue = mapping orig.[key]

    member x.Keys = orig.Keys

    member x.TryGetValue(key: 'TKey, value: byref<'TValue>): bool = 
        let mutable v = Unchecked.defaultof<'TOrigValue>
        if orig.TryGetValue(key, &v) then
            value <- mapping v
            true
        else false
         
    member x.Values: seq<'TValue> = orig.Values |> Seq.map mapping

    member x.ContainsValue(value) = orig.Values |> Seq.exists (fun i -> Unchecked.equals (mapping i) value)

    interface IDictionary<'TKey, 'TValue> with
        member x.Add(key: 'TKey, value: 'TValue): unit = raise (System.NotSupportedException())
        
        member x.Add(item: KeyValuePair<'TKey,'TValue>): unit = raise (System.NotSupportedException())
        
        member x.Clear(): unit = 
            raise (System.NotSupportedException())
        
        member x.Contains(item: KeyValuePair<'TKey,'TValue>): bool = 
            let (KeyValue(ik, iv)) = item
            orig |> Seq.exists (fun (KeyValue(k, v)) -> k = ik && Unchecked.equals (mapping v) iv) 
        
        member x.ContainsKey(key: 'TKey): bool = 
            x.ContainsKey key
        
        member x.CopyTo(array: KeyValuePair<'TKey,'TValue> [], arrayIndex: int): unit = 
            raise (System.NotSupportedException())
        
        member x.Count: int = 
            x.Count
        
        member x.GetEnumerator(): IEnumerator<KeyValuePair<'TKey,'TValue>> = 
            (orig |> Seq.map (fun (KeyValue(k, v)) -> KeyValuePair(k, mapping v))).GetEnumerator() 
        
        member x.GetEnumerator(): System.Collections.IEnumerator = 
            (orig |> Seq.map (fun (KeyValue(k, v)) -> KeyValuePair(k, mapping v)) :> System.Collections.IEnumerable).GetEnumerator() 
        
        member x.IsReadOnly: bool = true
        
        member x.Item
            with get (key: 'TKey): 'TValue = 
                x.[key]
            and set (key: 'TKey) (v: 'TValue): unit = 
                raise (System.NotSupportedException())
        
        member this.Keys: ICollection<'TKey> = 
            { new ICollection<'TKey> with
                  member x.Add(item: 'TKey): unit = 
                      raise (System.NotSupportedException())
                  
                  member x.Clear(): unit = 
                      raise (System.NotSupportedException())
                  
                  member x.Contains(item: 'TKey): bool = 
                      this.ContainsKey item
                  
                  member x.CopyTo(array: 'TKey [], arrayIndex: int): unit = 
                      raise (System.NotSupportedException())
                  
                  member x.Count: int = 
                      this.Count
                  
                  member x.GetEnumerator(): IEnumerator<'TKey> = 
                      this.Keys.GetEnumerator()
                  
                  member x.GetEnumerator(): System.Collections.IEnumerator = 
                      (this.Keys :> System.Collections.IEnumerable).GetEnumerator()
                  
                  member x.IsReadOnly: bool = 
                      true
                  
                  member x.Remove(item: 'TKey): bool = 
                      raise (System.NotSupportedException())
            }
        
        member x.Remove(key: 'TKey): bool = 
            raise (System.NotSupportedException())
        
        member x.Remove(item: KeyValuePair<'TKey,'TValue>): bool = 
            raise (System.NotSupportedException())
        
        member x.TryGetValue(key: 'TKey, value: byref<'TValue>): bool = 
            x.TryGetValue(key, &value)
        
        member this.Values: ICollection<'TValue> = 
            { new ICollection<'TValue> with
                  member x.Add(item: 'TValue): unit = 
                      raise (System.NotSupportedException())
                  
                  member x.Clear(): unit = 
                      raise (System.NotSupportedException())
                  
                  member x.Contains(item: 'TValue): bool = 
                      this.ContainsValue item
                  
                  member x.CopyTo(array: 'TValue [], arrayIndex: int): unit = 
                      raise (System.NotSupportedException())
                  
                  member x.Count: int = 
                      this.Count
                  
                  member x.GetEnumerator(): IEnumerator<'TValue> = 
                      this.Values.GetEnumerator()
                  
                  member x.GetEnumerator(): System.Collections.IEnumerator = 
                      (this.Values :> System.Collections.IEnumerable).GetEnumerator()
                  
                  member x.IsReadOnly: bool = 
                      true
                  
                  member x.Remove(item: 'TValue): bool = 
                      raise (System.NotSupportedException())
            }

type CompilationError =
    | SourceError of string
    | NameConflict of string * string
    | TypeNotFound of TypeDefinition
    | MethodNotFound of TypeDefinition * Method * list<Method>
    | MethodNameNotFound of TypeDefinition * Method * list<string>
    | ConstructorNotFound of TypeDefinition * Constructor * list<Constructor>
    | FieldNotFound of TypeDefinition * string 
//    | MacroError of TypeDefinition * string
  
    override this.ToString() =
        match this with
        | SourceError msg -> msg
        | NameConflict (msg, a) -> sprintf "%s at JavaScript address: '%s'" msg a
        | TypeNotFound typ -> sprintf "Type not found in JavaScript compilation: %s" typ.Value.FullName
        | MethodNotFound (typ, meth, candidates) ->
            sprintf "Method not found in JavaScript compilation: %s, Candidates: %s" 
                (string meth.Value) 
                (candidates |> Seq.map (fun m -> string m.Value) |> String.concat ", ")
        | MethodNameNotFound (typ, meth, candidates) ->
            sprintf "Method name not found in JavaScript compilation: %s, Members: %s" 
                (string meth.Value) 
                (candidates |> String.concat ", ")
        | ConstructorNotFound (typ, ctor, candidates) ->
            sprintf "Constructor not found in JavaScript compilation: %s, Candidates: %s" 
                (string ctor.Value) 
                (candidates |> Seq.map (fun c -> string c.Value) |> String.concat ", ")
        | FieldNotFound (typ, field) -> sprintf "Field not found in JavaScript compilation: %s.%s" typ.Value.FullName field

type LookupMemberResult =
    | Compiled of CompiledMember * Optimizations * Expression
    | Compiling of CompilingMember * Expression
    | CustomTypeMember of CustomTypeInfo
    | LookupMemberError of CompilationError 

type LookupFieldResult =
    | CompiledField of CompiledField * bool * Type
    | PropertyField of option<Method> * option<Method>
    | CustomTypeField of CustomTypeInfo
    | LookupFieldError of CompilationError 

type CompilationWarning =
    | SourceWarning of string
    | PublicProxy of TypeDefinition

    override this.ToString() =
        match this with
        | SourceWarning msg -> msg 
        | PublicProxy typ -> sprintf "Proxy type should not be public: %s" typ.Value.FullName
