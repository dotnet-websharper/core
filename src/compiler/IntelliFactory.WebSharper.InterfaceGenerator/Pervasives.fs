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

#nowarn "25"

[<AutoOpen>]
module Pervasives =
    open System
    module Code = CodeModel
    module R = IntelliFactory.WebSharper.Core.Reflection

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

    let private Access<'T when 'T :> Code.Entity> m (x: 'T) =
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

    open System.Collections.Generic
    
    let private addGenerics (generics: Code.TypeParameter list) (x: Code.Entity) =
        let add (prevGen: Code.TypeParameter list) setGen =
            if List.isEmpty prevGen then 
                setGen generics
            else
                let namesTaken = HashSet (prevGen |> Seq.map (fun p -> p.Name))
                let fmt (x: string) (n: int) =
                    if n = 0 then x else
                        System.String.Format("{0}{1:x}", x, n)
                let rec pick name k =
                    let res = fmt name k
                    if namesTaken.Contains res then
                        pick name (k + 1)
                    else res
                for p in generics do
                    let n = pick p.Name 0
                    namesTaken.Add n |> ignore
                    p.Name <- n
                setGen <| prevGen @ generics
        
        match x with
        | :? Code.Method as m -> add m.Generics (fun g -> m.Generics <- g)
        | :? Code.TypeDeclaration as d -> add d.Generics (fun g -> d.Generics <- g)
        | _ -> ()

    /// Generics helper.
    type GenericHelper = 
        private
        | Generic 
        | GenericNamed of string list
        member private this.MakeParameters (arity: int) isTypeDecl =
            match this with
            | GenericNamed ns ->
                ns |> List.mapi (fun i n -> Code.TypeParameter (i, n))
            | _ ->
                let genName = if isTypeDecl then "T" else "U" 
                if arity = 1 then [ Code.TypeParameter (0, genName) ]
                else List.init arity (fun x -> Code.TypeParameter (x, genName + string (x + 1)))

        member this.TypeDeclaration (arity: int) (make: list<_> -> #Code.TypeDeclaration) =
            let generics = this.MakeParameters arity true
            let x = make generics
            x |> addGenerics generics
            x

        member this.Method (arity: int) (make: list<_> -> Code.Method) =
            let generics = this.MakeParameters arity false
            let x = make generics
            x |> addGenerics generics
            x

        static member ( - ) (this: GenericHelper, f) =
            this.TypeDeclaration 1 (fun [ a ] -> f a)

        static member ( - ) (this: GenericHelper, f) =
            this.TypeDeclaration 2 (fun [ a; b ] -> f a b)

        static member ( - ) (this: GenericHelper, f) =
            this.TypeDeclaration 3 (fun [ a; b; c ] -> f a b c)

        static member ( - ) (this: GenericHelper, f) =
            this.TypeDeclaration 4 (fun [ a; b; c; d ] -> f a b c d)

        static member ( - ) (this: GenericHelper, (arity, make)) =
            this.TypeDeclaration arity make

        static member ( - ) (this: GenericHelper, f) =
            this.Method 1 (fun [ a ] -> f a)

        static member ( - ) (this: GenericHelper, f) =
            this.Method 2 (fun [ a; b ] -> f a b)

        static member ( - ) (this: GenericHelper, f) =
            this.Method 3 (fun [ a; b; c ] -> f a b c)

        static member ( - ) (this: GenericHelper, f) =
            this.Method 4 (fun [ a; b; c; d ] -> f a b c d)

        static member ( - ) (this: GenericHelper, (arity, make)) =
            this.Method arity make

        static member ( + ) (this: GenericHelper, names) =
            GenericNamed names

    /// Generics helper.
    let Generic = Generic

