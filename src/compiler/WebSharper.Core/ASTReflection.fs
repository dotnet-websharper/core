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

module WebSharper.Core.AST.Reflection

open WebSharper.Core

type private FST = Microsoft.FSharp.Reflection.FSharpType

let private getTypeDefinitionUnchecked fullAsmName (t: System.Type) =
    let rec getName (t: System.Type) =
        if t.IsNested then
            getName t.DeclaringType + "+" + t.Name 
        else t.Namespace + "." + t.Name         
    let name = getName t
    let asmName =
        if fullAsmName then
            match AssemblyConventions.StandardAssemblyFullNameForTypeNamed name with
            | Some n -> n
            | None -> t.Assembly.FullName
        else
            match AssemblyConventions.StandardAssemblyNameForTypeNamed name with
            | Some n -> n
            | None -> t.Assembly.FullName.Split(',').[0]
    Hashed {
        Assembly = asmName
        FullName = name
    } 

let ReadTypeDefinition (t: System.Type) =
    if t.IsArray then
        if t.GetArrayRank() = 1 then
            Definitions.Array
        else 
            Definitions.Array2
    elif FST.IsFunction t then 
        Definitions.FSharpFunc
    elif FST.IsTuple t then 
        Definitions.Tuple t.IsValueType (t.GetGenericArguments().Length)
    else
        getTypeDefinitionUnchecked false t

let private unitTy = typeof<unit>
let private voidTy = typeof<System.Void>

let rec ReadType (t: System.Type) =        
    let gen () =
        ConcreteType {
            Generics = 
                if t.IsGenericType then 
                    t.GetGenericArguments() |> Seq.map ReadType |> List.ofSeq 
                else [] 
            Entity = getTypeDefinitionUnchecked false t
        }
    if t.IsArray then
        ArrayType (ReadType(t.GetElementType()), t.GetArrayRank())
    elif t.IsByRef then
        ByRefType (ReadType(t.GetElementType()))
    elif FST.IsFunction t then
        let a, r = FST.GetFunctionElements t
        FSharpFuncType(ReadType a, ReadType r)        
    elif FST.IsTuple t then
        // if a tuple type is generic on the rest parameter, we don't have a definite length tuple and GetTupleElements fails
        try TupleType(FST.GetTupleElements t |> Seq.map ReadType |> List.ofSeq, t.IsValueType) 
        with _ -> gen()
    elif t.IsGenericParameter then  
        match t.DeclaringMethod with
        | null ->
            TypeParameter t.GenericParameterPosition
        | _ ->
            let dT = t.DeclaringType
            let k =
                if not dT.IsGenericType then 0 else
                    dT.GetGenericArguments().Length
            TypeParameter (k + t.GenericParameterPosition)
    elif t = voidTy || t = unitTy then
        VoidType
    else
        gen()

let private readMethodInfo (m : System.Reflection.MethodInfo) =
    let i = m.Module.ResolveMethod m.MetadataToken :?> System.Reflection.MethodInfo
    {
        MethodName = i.Name
        Parameters = i.GetParameters() |> Seq.map (fun p -> ReadType p.ParameterType) |> List.ofSeq
        ReturnType = ReadType i.ReturnType 
        Generics   = if i.IsGenericMethod then i.GetGenericArguments().Length else 0
    }

let ReadMethod m =
    Method (readMethodInfo m)

let private readConstructorInfo (i : System.Reflection.ConstructorInfo) =
    {
        CtorParameters = i.GetParameters() |> Seq.map (fun p -> ReadType p.ParameterType) |> List.ofSeq
    }

let ReadConstructor c =
    Constructor (readConstructorInfo c)    

let ReadMember (m: System.Reflection.MemberInfo) =
    match m with
    | :? System.Reflection.ConstructorInfo as c ->
        if c.IsStatic then Member.StaticConstructor
        else Member.Constructor (ReadConstructor c)    
        |> Some
    | :? System.Reflection.MethodInfo as m ->
        if m.IsVirtual then
            let b = m.GetBaseDefinition()
            let typ = b.DeclaringType 
            let info = ReadTypeDefinition typ, ReadMethod b 
            if typ.IsInterface then Member.Implementation info
            else Member.Override info
        else Member.Method (not m.IsStatic, ReadMethod m)    
        |> Some
    | _ -> None

let LoadType (t: Type) =
    try System.Type.GetType(t.AssemblyQualifiedName, true)  
    with _ -> failwithf "Failed to load type %s" t.AssemblyQualifiedName

let LoadTypeDefinition (td: TypeDefinition) =
    try System.Type.GetType(td.Value.AssemblyQualifiedName, true)   
    with _ -> failwithf "Failed to load type %s from assembly %s" td.Value.FullName td.Value.Assembly

let [<Literal>] AllMethodsFlags = 
    System.Reflection.BindingFlags.Instance
    ||| System.Reflection.BindingFlags.Static
    ||| System.Reflection.BindingFlags.Public
    ||| System.Reflection.BindingFlags.NonPublic

let [<Literal>] AllPublicMethodsFlags = 
    System.Reflection.BindingFlags.Instance
    ||| System.Reflection.BindingFlags.Static
    ||| System.Reflection.BindingFlags.Public

let LoadMethod td (m: Method) =
    let m = m.Value
    let methodInfos = (LoadTypeDefinition td).GetMethods(AllMethodsFlags)
    try
        methodInfos
        |> Seq.find (fun i -> i.Name = m.MethodName && readMethodInfo i = m)
    with _ ->
        failwithf "Could not load method %O candidates: %A" m (methodInfos |> Seq.choose (fun c -> 
            let mc = readMethodInfo c
            if mc.MethodName = m.MethodName then Some (string mc) else None
        ) |> Array.ofSeq)

let LoadConstructor td (c: Constructor) =
    let c = c.Value
    let ctorInfos = (LoadTypeDefinition td).GetConstructors(AllMethodsFlags)
    try
        ctorInfos
        |> Seq.find (fun i -> readConstructorInfo i = c)
    with _ ->
        failwithf "Could not load constructor for type %s" td.Value.AssemblyQualifiedName

let ReadSignature (m : System.Reflection.MethodInfo) =
    let i = m.Module.ResolveMethod m.MetadataToken :?> System.Reflection.MethodInfo
    i.GetParameters() |> Seq.map (fun p -> 
        ReadType p.ParameterType,
        if p.IsOptional then Some (ReadLiteral p.DefaultValue) else None
    ) |> List.ofSeq
    ,
    ReadType i.ReturnType 
