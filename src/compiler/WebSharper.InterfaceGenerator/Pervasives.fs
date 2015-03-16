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

namespace WebSharper.InterfaceGenerator

#nowarn "25"

[<AutoOpen>]
module Pervasives =
    open System
    module Code = CodeModel
    module R = WebSharper.Core.Reflection

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
        Code.Constructor (Type.Lambda ps.Parameters Type.Unit)

    /// Constructs a new constructor with an object expression as inline.
    let ObjectConstructor (ps: Type.IParameters) =
        Code.Constructor (Type.Lambda ps.Parameters Type.Unit, true)

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

    /// Makes a list of members static.
    let Static xs =
        Code.Static xs

    /// Makes a list of members instance.
    let Instance xs =
        Code.Instance xs

    /// Sets the base class.
    let Inherits (c: Type.IType) =
        { new Code.IClassProperty with
            member this.SetOn x = 
                let x = x.Clone() :?> Code.Class
                x.BaseClass <- Some c.Type
                x
        }

    /// Adds nested classes and interfaces.
    let Nested (cs: list<Code.TypeDeclaration>) =
        { new Code.IClassProperty with
            member this.SetOn x = 
                let x = x.Clone() :?> Code.Class
                for decl in cs do
                    match decl with
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
    [<Obsolete "Use the equivalent =@ operator">]
    let ( =% ) name ty = Property name ty

    // Note: the operator below may seem useless, since `op_Dynamic`
    // already provides the `T?x` syntax.
    // However, static members operators are always overridden
    // by `let`-bound operators, regardless of the order in which
    // they are brought to scope. This means that if `IF.WS.JavaScript`
    // is opened in a WIG project, then any use of `?` will invoke
    // JavaScript field access, instead of the intended parameter naming.
    // With the `let` definition below, opening `IF.WS.InterfaceGenerator`
    // after `IF.WS.JavaScript` allows the use of the correct operator.

    /// `T?x` constructs a `Parameter` named "x" of type `T`.
    let inline ( ? ) (ty: ^T) name =
        (^T : (static member op_Dynamic : ^T * string -> ^U) (ty, name))
        
    /// Marks an entity with the Obsolete attribute.
    let Obsolete (x: #Code.Entity) =
        x |> Code.Entity.Update (fun x -> x.ObsoleteStatus <- CodeModel.Obsolete None)

    /// Marks an entity with the Obsolete attribute.
    let ObsoleteWithMessage message (x: #Code.Entity) =
        x |> Code.Entity.Update (fun x -> x.ObsoleteStatus <- CodeModel.Obsolete (Some message))

    /// Constructs a class protocol (instance members).
    [<Obsolete "Use |+> Instance [...]">]
    let Protocol (members: list<Code.Member>) =
        [ for m in members -> m |> Code.Entity.Update (fun x -> x.IsStatic <- false) :> Code.IClassMember ]

    /// Adds a comment.
    let WithComment (comment: string) (x: #Code.Entity) =
        x |> Code.Entity.Update (fun x -> x.Comment <- Some comment)

    /// Adds a source name.
    let WithSourceName (name: string) (x: #Code.Entity) =
        x |> Code.Entity.Update (fun x -> x.SourceName <- Some name)

    /// Adds an inline.
    let WithInline (code: string) (x: #Code.MethodBase) =
        x |> Code.Entity.Update (fun x -> x.Inline <- Some (Code.BasicInline code))

    /// Adds an inline for a property getter.
    let WithGetterInline (code: string) (p: Code.Property) =
        p |> Code.Entity.Update (fun x -> x.GetterInline <- Some (Code.BasicInline code))

    /// Adds an inline for a property setter.
    let WithSetterInline (code: string) (p: Code.Property) =
        p |> Code.Entity.Update (fun x -> x.SetterInline <- Some (Code.BasicInline code))

    /// Creates an inline using interop transformations.
    /// Use the function provided by createInline to wrap a parameter name.
    let WithInteropInline (createInline: (string -> string) -> string) (x: #Code.MethodBase) =
        x |> Code.Entity.Update (fun x -> x.Inline <- Some (Code.TransformedInline createInline))

    /// Creates an inline using interop transformations for a property getter.
    /// Use the function provided by createInline to call it with "index" if the property is indexed.
    let WithInteropGetterInline (createInline: (string -> string) -> string) (p: Code.Property) =
        p |> Code.Entity.Update (fun x -> x.GetterInline <- Some (Code.TransformedInline createInline))

    /// Creates an inline using interop transformations for a property setter.
    /// Use the function provided by createInline to call it with "value" and with "index" if the property is indexed.
    let WithInteropSetterInline (createInline: (string -> string) -> string) (p: Code.Property) =
        p |> Code.Entity.Update (fun x -> x.SetterInline <- Some (Code.TransformedInline createInline))

    /// Adds a default in and out inline transform to a type.
    /// In transform is applied to method arguments and property setters.
    /// Out transform is applied to method return values and property getters.
    /// When a member defines a custom inline these transforms are ignored.
    let WithInterop (transforms: Type.InlineTransforms) (t: Type.IType) = 
        match t.Type with
        | Type.InteropType (t, tr) ->
            Type.InteropType (t, transforms * tr)
        | Type.NoInteropType t
        | t ->
            Type.InteropType (t, transforms)

    let WithNoInterop (t: Type.IType) =
        match t.Type with
        | Type.NoInteropType _ as t -> t
        | Type.InteropType (t, _)
        | t -> 
            Type.NoInteropType t

    /// Adds indexer argument.
    let Indexed (indexer: Type.Type) (p: Code.Property) =
        p |> Code.Entity.Update (fun x -> x.IndexerType <- Some indexer)

    /// Constructs a new `Type`.
    let T<'T> = Type.SystemType (R.Type.FromType typeof<'T>)

    /// Will be evaluated to the type the member is added to.
    let TSelf = Type.DefiningType

    /// Constructs a new `Type` from System.Type object.
    let SystemType t = Type.SystemType (R.Type.FromType t)

    /// Adds a macro to method or constructor. Macro type must be defined in another assembly.
    let WithMacro (macroType: System.Type) (x: #Code.MethodBase) =
        x |> Code.Entity.Update (fun x -> x.Macro <- Some (SystemType macroType))

    /// Makes a resource always linked if any type in assembly is used. 
    let AssemblyWide (r: Code.Resource) = r.AssemblyWide()

    /// Constructs a new `FunctionType`.
    let ( ^-> ) (parameters: Type.IParameters) (returnType: Type.IType) =
        Type.Lambda parameters.Parameters returnType.Type

    /// Defines the type of the `this` parameter.
    let ( -* ) (thisType: Type.IType) (parameters: Type.IParameters) =
        { parameters.Parameters with This = Some thisType.Type }

    /// Defines the type of the variable-argument parameter.
    let ( *+ ) (parameters: Type.IParameters) (paramArrayType: Type.IType) =
        { parameters.Parameters with Variable = Some paramArrayType.Type }

    /// Constructs a new `ArrayType`.
    let ( !| ) (itemType: Type.IType) =
        Type.ArrayOf itemType

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

    // only used for Obsolete members
    type private T = Type.Type

    /// Generics helper.
    type GenericHelper = 
        private
        | Generic 
        | GenericNamed of string list
        member private this.MakeParameters (arity: int) =
            match this with
            | GenericNamed ns ->
                if List.length ns <> arity then
                    failwith "Generic + [...] - : number of type parameters and specified names do not match."
                ns |> List.map (fun n -> Code.TypeParameter n)
            | _ ->
                List.init arity (fun x -> Code.TypeParameter "T")

        member this.Entity (arity: int) (make: list<_> -> #Code.Entity) =
            let generics = this.MakeParameters arity
            let x = make generics
            match x :> Code.Entity with
            | :? Code.Method as m -> m.Generics <- m.Generics @ generics
            | :? Code.TypeDeclaration as d -> d.Generics <- d.Generics @ generics
            | _ -> ()
            x

        member this.MemberList (arity: int) (make: list<_> -> list<#Code.Member>) =
            let generics = this.MakeParameters arity
            let xs = make generics
            xs |> List.iter (fun x ->
                match x :> Code.Member with
                | :? Code.Method as m -> 
                    m.Generics <- m.Generics @ generics
                | _ -> ()
            )
            xs

        member this.ClassMembers (arity: int) (make: list<_> -> Code.ClassMembers) =
            let generics = this.MakeParameters arity
            let xs = make generics
            match xs with
            | Code.Instance ms
            | Code.Static ms ->
                ms |> List.iter (fun x ->
                    match x with
                    | :? Code.Method as m -> 
                        m.Generics <- m.Generics @ generics
                    | _ -> ()
                )
            xs

        static member ( - ) (this: GenericHelper, f) =
            this.Entity 1 (fun [ a ] -> f a)

        static member ( - ) (this: GenericHelper, f) =
            this.Entity 2 (fun [ a; b ] -> f a b)

        static member ( - ) (this: GenericHelper, f) =
            this.Entity 3 (fun [ a; b; c ] -> f a b c)

        static member ( - ) (this: GenericHelper, f) =
            this.Entity 4 (fun [ a; b; c; d ] -> f a b c d)

        static member ( % ) (this: GenericHelper, f) =
            this.MemberList 1 (fun [ a ] -> f a)

        static member ( % ) (this: GenericHelper, f) =
            this.MemberList 2 (fun [ a; b ] -> f a b)

        static member ( % ) (this: GenericHelper, f) =
            this.MemberList 3 (fun [ a; b; c ] -> f a b c)

        static member ( % ) (this: GenericHelper, f) =
            this.MemberList 4 (fun [ a; b; c; d ] -> f a b c d)

        static member ( * ) (this: GenericHelper, f) =
            this.ClassMembers 1 (fun [ a ] -> f a)

        static member ( * ) (this: GenericHelper, f) =
            this.ClassMembers 2 (fun [ a; b ] -> f a b)

        static member ( * ) (this: GenericHelper, f) =
            this.ClassMembers 3 (fun [ a; b; c ] -> f a b c)

        static member ( * ) (this: GenericHelper, f) =
            this.ClassMembers 4 (fun [ a; b; c; d ] -> f a b c d)

        static member ( + ) (this: GenericHelper, names) =
            GenericNamed names

    /// Generics helper.
    let Generic = Generic

    type GenericNHelper =
        private
        | GenericN of int * GenericHelper

        static member ( - ) (this: GenericNHelper, f) =
            match this with
            | GenericN (arity, h) -> 
                h.Entity arity f

        static member ( % ) (this: GenericNHelper, f) =
            match this with
            | GenericN (arity, h) -> 
                h.MemberList arity f

        static member ( * ) (this: GenericNHelper, f) =
            match this with
            | GenericN (arity, h) -> 
                h.ClassMembers arity f

        static member ( + ) (this: GenericNHelper, names) =
            match this with
            | GenericN (arity, _) -> 
                if List.length names <> arity then
                    failwith "GenericN + : wrong number of type parameter names specified."
                GenericN (arity, GenericNamed names)

    let GenericN arity = GenericN (arity, Generic)   
