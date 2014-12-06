// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2014 IntelliFactory
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

namespace IntelliFactory.WebSharper.InterfaceGenerator

#nowarn "1189"

[<AutoOpen>]
module Pervasives =
    open System
    module Code = CodeModel
    module R = IntelliFactory.WebSharper.Core.Reflection

    type private T = Type.Type

    type IExtension =
        abstract Assembly : Code.Assembly

    [<Sealed>]
    [<AttributeUsage(AttributeTargets.Assembly)>]
    type ExtensionAttribute(t: Type) =
        inherit Attribute()

        member attr.GetAssembly() =
            let e = Activator.CreateInstance(t) :?> IExtension
            e.Assembly

    /// Constructs a new assembly.
    let Assembly namespaces : Code.Assembly =
        {
            Namespaces = namespaces
            DependsOn = []
        }

    /// Constructs a new namespace.
    let Namespace name (members: list<Code.NamespaceEntity>) : Code.Namespace =
        let mutable classes = []
        let mutable interfaces = []
        let mutable resources = []
        for m in members do
            match m with
            | :? Code.Class as x -> classes <- x :: classes
            | :? Code.Interface as x -> interfaces <- x :: interfaces
            | :? Code.Resource as x -> resources <- x :: resources
            | _ -> ()
        {
            Name = name
            Classes = classes
            Interfaces = interfaces
            Resources = resources
        }

    /// Constructs a new class.
    let Class name = Code.Class name

    /// Constructs a new interface.
    let Interface name = Code.Interface name

    /// Constructs a new method.
    let Method name (ty: Type.IType) = Code.Method (name, ty.Type)

    /// Constructs a new constructor.
    let Constructor (ps: Type.IParameters) =
        Code.Constructor (Type.Lambda ps.Parameters Type.Unit,
            ps.Parameters = Type.Parameters.Empty)

    /// Constructs a new property getter.
    let Getter name (ty: Type.IType) = 
        Code.Property (name, ty.Type, HasGetter = true)

    /// Constructs a new property setter.
    let Setter name (ty: Type.IType) =
        Code.Property (name, ty.Type, HasSetter = true)

    /// Constructs a new property with a getter and a setter.
    let Property name (ty: Type.IType) =
        Code.Property (name, ty.Type, HasGetter = true, HasSetter = true)

    /// Constructs a new resource from a source path.
    let Resource name path =
        Code.Resource (name, [path])

    /// Constructs a new resource from a base path and a list of subpaths.
    let Resources name basePath paths =
        Code.Resource (name, basePath :: paths)

    /// Makes a member static.
    let Static (x: #Code.Member) =
        x |> Code.Entity.Update (fun x -> x.IsStatic <- true)
        
    /// Makes a member instance.
    let Instance (x: #Code.Member) =
        x |> Code.Entity.Update (fun x -> x.IsStatic <- false)

    /// Sets the base class.
    let Inherits (c: Type.IType) =
        { new Code.IClassProperty with
            member this.SetOn x = 
                let x = x.Clone() :?> Code.Class
                x.BaseClass <- Some c.Type
                x
        }

    /// Adds nested classes and interfaces.
    let Nested (cs: list<#Code.TypeDeclaration>) =
        { new Code.IClassProperty with
            member this.SetOn x = 
                let x = x.Clone() :?> Code.Class
                for decl in cs do
                    match decl :> Code.TypeDeclaration with
                    | :? Code.Class as c ->
                        x.NestedClasses <- c :: x.NestedClasses
                    | :? Code.Interface as i ->
                        x.NestedInterfaces <- i :: x.NestedInterfaces
                    | _ -> ()
                x
        }

    /// Adds an `implements` clause to a class.
    let Implements (interfaces: list<Type.IType>) =
        { new Code.IClassProperty with
            member this.SetOn x =
                let x = x.Clone() :?> Code.Class
                x.ImplementedInterfaces <- [for i in interfaces -> i.Type]
                x
        }

    /// Appends the base interfaces.
    let Extends (interfaces: list<Type.IType>) =
        { new Code.IInterfaceProperty with
            member this.SetOn x =
                let x = x.Clone() :?> Code.Interface
                x.BaseInterfaces <- [for i in interfaces -> i.Type]
                x
        }

    /// Constructs a new method.
    let ( => ) name ty = Method name ty

    /// Constructs a new property with a getter and a setter.
    let ( =@ ) name ty = Property name ty

    /// Constructs a new property getter.
    let ( =? ) name ty = Getter name ty

    /// Constructs a new property setter.
    let ( =! ) name ty = Setter name ty

    /// Constructs a new property with a getter and a setter.
    let ( =% ) name ty = Property name ty

    let private Access m (x: #Code.Entity) =
        let x = x.Clone() :?> 'T
        x.AccessModifier <- m
        x
        
    /// Marks an entity as public.
    let Public x = Access Code.AccessModifier.Public x

    /// Marks an entity as private.
    let Private x = Access Code.AccessModifier.Private x

    /// Marks an entity as protected.
    let Protected x = Access Code.AccessModifier.Protected x

    /// Marks an entity as internal.
    let Internal x = Access Code.AccessModifier.Internal x

    /// Marks an entity with the Obsolete attribute.
    let Obsolete (x: #Code.Entity) =
        x |> Code.Entity.Update (fun x -> x.ObsoleteStatus <- CodeModel.Obsolete None)

    /// Marks an entity with the Obsolete attribute.
    let ObsoleteWithMessage message (x: #Code.Entity) =
        x |> Code.Entity.Update (fun x -> x.ObsoleteStatus <- CodeModel.Obsolete (Some message))

    /// Constructs a class protocol (instance members).
    let Protocol (members: list<Code.Member>) =
        [ for m in members -> Instance m :> Code.IClassMember ]

    /// Adds a comment.
    let WithComment (comment: string) (x: #Code.Entity) =
        x |> Code.Entity.Update (fun x -> x.Comment <- Some comment)

    /// Adds a source name.
    let WithSourceName (name: string) (x: #Code.Entity) =
        x |> Code.Entity.Update (fun x -> x.SourceName <- Some name)

    /// Adds an inline.
    let WithInline (code: string) (x: #Code.MethodBase) =
        x |> Code.Entity.Update (fun x -> x.Inline <- Some code)

    /// Adds an inline.
    let WithGetterInline (code: string) (p: Code.Property) =
        p |> Code.Entity.Update (fun x -> x.GetterInline <- Some code)

    /// Adds an inline.
    let WithSetterInline (code: string) (p: Code.Property) =
        p |> Code.Entity.Update (fun x -> x.SetterInline <- Some code)

    /// Adds indexer argument.
    let Indexed (indexer: T) (p: Code.Property) =
        p |> Code.Entity.Update (fun x -> x.IndexerType <- Some indexer)

    /// Constructs a new `Type`.
    let T<'T> = Type.SystemType (R.Type.FromType typeof<'T>)

    /// Constructs a new `Type` from System.Type object.
    let SystemType t = Type.SystemType (R.Type.FromType t)

    /// Constructs a new `FunctionType`.
    let ( ^-> ) (parameters: Type.IParameters) (returnType: Type.IType) =
        Type.Lambda parameters.Parameters returnType.Type

    /// Defines the type of the `this` parameter.
    let ( -* ) (thisType: Type.IType) (parameters: Type.IParameters) =
        { parameters.Parameters with This = Some thisType.Type }

    /// Defines the type of the variable-argument parameter.
    let ( *+ ) (parameters: Type.IParameters) (paramArrayType: Type.IType) =
        { parameters.Parameters with Variable = Some paramArrayType.Type }

    /// Constructs an optional parameter.
    let ( !? ) (parameter: Type.IParameter) =
        { parameter.Parameter with Optional = true }

    /// Constructs variable-argument `Parameters`.
    let ( !+ ) (ty: Type.IType) : Type.Parameters =
        {
            This = None
            Variable = Some ty.Type
            Arguments = []
        }

    /// Adds a resource dependency.
    let Requires<'T when 'T :> Code.IResourceDependable<'T>> (requires : Code.Resource list) (ty: 'T) =
        ty.AddRequires (requires |> List.map (fun res -> Code.LocalDependency res.Id))

    /// Adds an externally defined resource dependency.
    let RequiresExternal<'T when 'T :> Code.IResourceDependable<'T>> (requires: Type.Type list) (ty: 'T) =
        ty.AddRequires (requires |> List.map (fun res -> Code.ExternalDependency res))

    let private Fresh =
        let x = ref 0
        fun () ->
            incr x
            !x

    /// Generics helper.
    type GenericHelper = | Generic with

        member this.Entity (arity: int) (make: list<T> -> #Code.Entity) =
            let generics = List.init arity (fun x -> Code.TypeParameter ("T" + string (x + 1)))
            let types = generics |> List.map (fun p -> Type.GenericType (p.Name, ref []))
            let x = make types
            for g, t in Seq.zip generics types do
                match t with
                | Type.GenericType(_, cs) ->
                    g.Constraints <- !cs
                | _ -> ()
            match box x with
            | :? Code.Method as m -> m.Generics <- generics
            | _ -> ()
            x

        member this.Type (arity: int) =
            let t = Type.New ()
            fun ts -> t.[List.toArray ts]

        member this.Type1() = 
            let f = this.Type 1
            fun a -> f [a]

        member this.Type2 () =
            let f = this.Type 2
            fun a b -> f [a; b]

        member this.Type3 () =
            let f = this.Type 3
            fun a b c -> f [a; b; c]

        member this.Type4 () =
            let f = this.Type 4
            fun a b c d -> f [a; b; c; d]

        member this.TypeDeclaration (arity: int) (make: list<T> -> #Code.TypeDeclaration) =
            let prefix = String.Format("T{0:x}", Fresh ())
            let generics = [for n in 1 .. arity -> Code.TypeParameter (prefix + "_" + string n)]
            let types = [for g in generics -> Type.GenericType (g.Name, ref [])]
            let id = (make types).Id
            for g, t in Seq.zip generics types do
                match t with
                | Type.GenericType(_, cs) ->
                    g.Constraints <- !cs
                | _ -> ()
            fun parameters ->
                let decl = make types
                decl.Id <- id
                decl.Generics <- generics
                decl.Type <- (Type.DeclaredType id).[List.toArray parameters]
                decl

        static member ( / ) (this: GenericHelper, f) =
            let f = this.TypeDeclaration 1 (fun x -> f x.[0])
            fun x -> f [x]

        static member ( / ) (this: GenericHelper, f) =
            let f = this.TypeDeclaration 2 (fun x -> f x.[0] x.[1])
            fun x y -> f [x; y]

        static member ( / ) (this: GenericHelper, f) =
            let f = this.TypeDeclaration 3 (fun x -> f x.[0] x.[1] x.[2])
            fun x y z -> f [x; y; z]

        static member ( / ) (this: GenericHelper, f) =
            let f = this.TypeDeclaration 4 (fun x -> f x.[0] x.[1] x.[2] x.[3])
            fun a b c d -> f [a; b; c; d]

        static member ( - ) (this: GenericHelper, f) =
            this.Entity 1 (fun x -> f x.[0])

        static member ( - ) (this: GenericHelper, f) =
            this.Entity 2 (fun x -> f x.[0] x.[1])

        static member ( - ) (this: GenericHelper, f) =
            this.Entity 3 (fun x -> f x.[0] x.[1] x.[2])

        static member ( - ) (this: GenericHelper, f) =
            this.Entity 4 (fun x -> f x.[0] x.[1] x.[2] x.[3])

    let WithConstraint (constraints: list<T>) (ty: T) =
        match ty with
        | Type.GenericType (_, cs) -> cs := constraints
        | _ -> ()
        ty
