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

#nowarn "1189"

namespace WebSharper.InterfaceGenerator

/// Defines meta-objects and for describing the interfaces of complete
/// JavaScript libraries or frameworks.
module CodeModel =

    type private T = Type.Type

    type ObsoleteStatus =
        | NotObsolete
        | Obsolete of string option

    and [<AbstractClass>] Entity =
        val mutable Name : string
        val mutable SourceName : option<string>
        val mutable Type : T
        val mutable Comment : option<string>
        val mutable ObsoleteStatus : ObsoleteStatus

        internal new (name, t) =
            {
                Name = name
                SourceName = None
                Type = t
                Comment = None
                ObsoleteStatus = NotObsolete
            }

        /// On type definitions, applies the update.
        /// On other entities, clones the specified entity object, invokes the
        /// function to perform changes on it as side-effects and then
        /// returns the updated entity object.
        static member Update<'TEntity when 'TEntity :> Entity>
            f (x: 'TEntity) : 'TEntity =
                if x :> Entity :? TypeDeclaration then
                    f x
                    x  
                else
                    let clone = x.MemberwiseClone() :?> 'TEntity
                    f clone
                    clone

    and TypeParameter =
        val Id : Type.Id
        val mutable Name : string
        val mutable Constraints : list<T>

        internal new (name) =
            {
                Id = Type.Id()
                Name = name
                Constraints = []
            }

        /// `T?x` constructs a `Parameter` named "x" of type `T`.
        static member op_Dynamic(this: TypeParameter, name: string) =
            this.Type?(name)

        /// Constructs a union type.
        static member ( + ) (this: TypeParameter, x: Type.IType) =
            this.Type + x

        /// Constructs a tuple type.
        static member ( * ) (this: TypeParameter, x: Type.IType) =
            this.Type * x

        /// Adds the class to parameters.
        static member ( * ) (this: TypeParameter, x: Type.Parameter) =
            this.Type * x

        /// Constructs an option type.
        static member ( !? ) (this: TypeParameter) =
            !? this.Type

        member this.Type = Type.GenericType this.Id

        interface Type.IType with
            member this.Type = this.Type

        interface Type.IParameter with
            member this.Parameter =
                let x = (this :> Type.IType).Type
                (x :> Type.IParameter).Parameter

        interface Type.IParameters with
            member this.Parameters =
                let x = (this :> Type.IParameter).Parameter
                (x :> Type.IParameters).Parameters

    and [<AbstractClass>] TypeDeclaration =
        inherit NamespaceEntity
        val mutable Generics : list<TypeParameter>
        val mutable Methods : list<Method>
        val mutable Properties : list<Property>

        internal new (name) =
            {
                inherit NamespaceEntity(name)
                Generics = []
                Methods = []
                Properties = []
            }

        /// Assigns the associated type.
        [<System.Obsolete>]
        static member ( |=> )<'T when 'T :> TypeDeclaration>
            (this: 'T, t: Type.IType) =
                this.Type <- t.Type
                match t.Type with
                | Type.DeclaredType id 
                | Type.SpecializedType(Type.DeclaredType id, _) ->
                    id.Name <- this.Name
                    this.Id <- id
                | _ -> ()
                this

        member this.Item
            with get ([<System.ParamArray>] x : Type.IType []) =
                this.Type.[x]

        /// `T?x` constructs a `Parameter` named "x" of type `T`.
        static member op_Dynamic(this: TypeDeclaration, name: string) =
            this.Type?(name)

        /// Constructs a union type.
        static member ( + ) (this: TypeDeclaration, x: Type.IType) =
            this.Type + x

        /// Constructs a tuple type.
        static member ( * ) (this: TypeDeclaration, x: Type.IType) =
            this.Type * x

        /// Adds the class to parameters.
        static member ( * ) (this: TypeDeclaration, x: Type.Parameter) =
            this.Type * x
   
        /// Constructs an option type.
        static member ( !? ) (this: TypeDeclaration) =
            !? this.Type

        interface Type.IType with
            member this.Type = this.Type

        interface Type.IParameter with
            member this.Parameter =
                let x = (this :> Type.IType).Type
                (x :> Type.IParameter).Parameter

        interface Type.IParameters with
            member this.Parameters =
                let x = (this :> Type.IParameter).Parameter
                (x :> Type.IParameters).Parameters

    and Interface =
        inherit TypeDeclaration
        val mutable BaseInterfaces : list<T>

        internal new (name) =
            {
                inherit TypeDeclaration(name)
                BaseInterfaces = []
            }

        /// Applies updates.
        static member ( |+> ) (this: Interface, xs: list<#IInterfaceMember>) =
            (this, xs)
            ||> List.fold (fun x y -> y.AddTo x)

        /// Applies an update.
        static member ( |=> ) (this: Interface, x: IInterfaceProperty) =
            x.SetOn this

        interface IResourceDependable<Interface> with
            member this.AddRequires res =
                this |> Entity.Update (fun x ->
                    x.DependsOn <- res @ x.DependsOn)
            member this.GetRequires() = this.DependsOn

    and Class =
        inherit TypeDeclaration
        val mutable BaseClass : option<T>
        val mutable ImplementedInterfaces : list<T>
        val mutable Constructors : list<Constructor>
        val mutable NestedClasses : list<Class>
        val mutable NestedInterfaces : list<Interface>

        internal new (name) =
            {
                inherit TypeDeclaration(name)
                BaseClass = None
                ImplementedInterfaces = []
                Constructors = []
                NestedClasses = []
                NestedInterfaces = []
            }

        /// Applies updates.
        [<System.Obsolete "Use |+> Static [...] or |+> Instance [...]">]
        static member ( |+> ) (this: Class, xs: list<IClassMember>) =
            (this, xs)
            ||> List.fold (fun x y -> y.AddTo x)

        /// Applies updates.
        static member ( |+> ) (this: Class, xs: ClassMembers) =
            match xs with
            | Static xs ->
                (this, xs) ||> List.fold (fun x y -> y.SetIsStatic(true).AddTo x)
            | Instance xs ->
                (this, xs) ||> List.fold (fun x y -> y.SetIsStatic(false).AddTo x)

        /// Sets a property.
        static member ( |=> ) (this: Class, x: IClassProperty) =
            x.SetOn this

        interface IResourceDependable<Class> with
            member this.AddRequires res =
                this |> Entity.Update (fun x ->
                    x.DependsOn <- res @ x.DependsOn)
            member this.GetRequires() = this.DependsOn

    and [<AbstractClass>] Member =
        inherit Entity

        val mutable IsStatic : bool

        abstract member AddTo : Class -> Class

        interface IClassMember with
            member this.AddTo x = this.AddTo x
            member this.SetIsStatic s = this |> Entity.Update (fun x -> x.IsStatic <- s) :> _

        internal new (name, t) = 
            {
                inherit Entity(name, t)
                IsStatic = true
            }

    and Inline =
        | BasicInline of string
        | TransformedInline of ((string -> string) -> string)

    and [<AbstractClass>] MethodBase =
        inherit Member
        val mutable Inline : option<Inline>
        val mutable IsPure : bool
        val mutable Macro : option<T>
        val mutable Warning : option<string>

        internal new (name, t) =
            {
                inherit Member(name, t)
                Inline = None
                IsPure = false
                Macro = None
                Warning = None
            }

    and Constructor =
        inherit MethodBase

        val mutable IsObject : bool

        internal new (t, ?obj) =
            { 
                inherit MethodBase("", t)
                IsObject = defaultArg obj false
            }

        override this.AddTo x =
            x
            |> Entity.Update (fun x ->
                x.Constructors <- this :: x.Constructors)

        interface IClassMember with
            member this.AddTo x = this.AddTo x
            member this.SetIsStatic s = this :> _

    and Method =
        inherit MethodBase
        val mutable Generics : list<TypeParameter>
        val mutable IsOverride : bool

        internal new (name, t) =
            {
                inherit MethodBase(name, t)
                Generics = []
                IsOverride = false
            }

        member private this.Add<'T when 'T :> TypeDeclaration> (x: 'T) =
            x |> Entity.Update (fun x -> x.Methods <- this :: x.Methods)

        override this.AddTo x = this.Add x

        interface IInterfaceMember with
            member this.AddTo x =
                let m =
                    this
                    |> Entity.Update (fun x -> x.IsStatic <- false)
                m.Add x

    and Property =
        inherit Member
        val mutable HasGetter : bool
        val mutable HasSetter : bool
        val mutable IndexerType : option<T>
        val mutable GetterInline : option<Inline>
        val mutable SetterInline : option<Inline>
        val mutable IsOverride : bool

        internal new (name, t) = 
            {
                inherit Member(name, t)
                GetterInline = None
                SetterInline = None
                HasGetter = false
                HasSetter = false
                IndexerType = None
                IsOverride = false
            }

        member private this.Add<'T when 'T :> TypeDeclaration> (x: 'T) =
            x |> Entity.Update (fun x -> x.Properties <- this :: x.Properties)

        override this.AddTo x = this.Add x

        interface IInterfaceMember with
            member this.AddTo x =
                let m =
                    this
                    |> Entity.Update (fun x -> x.IsStatic <- false)
                m.Add x

    and IClassMember =
        abstract member AddTo : Class -> Class
        abstract member SetIsStatic: bool -> IClassMember 

    and IInterfaceMember =
        abstract member AddTo : Interface -> Interface

    and ClassMembers =
        | Static of list<IClassMember>
        | Instance of list<IClassMember>

    and IClassProperty =
        abstract member SetOn : Class -> Class

    and IInterfaceProperty =
        abstract member SetOn : Interface -> Interface

    and IResourceDependable =
        abstract member GetRequires : unit -> list<Dependency>

    and IResourceDependable<'T> =
        inherit IResourceDependable
        abstract member AddRequires : list<Dependency> -> 'T

    and [<AbstractClass>] NamespaceEntity =
        inherit Entity
        val mutable DependsOn : list<Dependency>
        val mutable Id : Type.Id

        internal new (name) =
            let id = Type.Id name
            {
                inherit Entity(name, Type.DeclaredType id)
                DependsOn = []
                Id = id
            }

    and Dependency =
        | ExternalDependency of Type.Type
        | LocalDependency of Type.Id

    and Resource =
        inherit NamespaceEntity
        val mutable Paths : string list
        val mutable IsAssemblyWide : bool

        internal new (name, paths) =
            {
                inherit NamespaceEntity(name)
                Paths = paths
                IsAssemblyWide = false
            }

        member r.AssemblyWide() =
            r
            |> Entity.Update(fun r -> r.IsAssemblyWide <- true)

        interface IResourceDependable<Resource> with
            member this.AddRequires res =
                this |> Entity.Update (fun x ->
                    x.DependsOn <- res @ x.DependsOn)
            member this.GetRequires() = this.DependsOn

    type Namespace =
        {
            Name : string
            Interfaces : list<Interface>
            Classes : list<Class>
            Resources : list<Resource>
        }

    type Assembly =
        {
            Namespaces : list<Namespace>
            DependsOn : list<Dependency>
        }

        interface IResourceDependable<Assembly> with
            member this.AddRequires res =
                { this with DependsOn = res @ this.DependsOn }
            member this.GetRequires() = this.DependsOn
