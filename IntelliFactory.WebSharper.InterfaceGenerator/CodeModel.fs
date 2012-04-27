// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2012 IntelliFactory
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

#nowarn "1189"

namespace IntelliFactory.WebSharper.InterfaceGenerator

/// Defines meta-objects and for describing the interfaces of complete
/// JavaScript libraries or frameworks.
module CodeModel =

    type private T = Type.Type

    /// Represents the access restriction modifier.
    type AccessModifier =
        | Public    = 0uy
        | Private   = 2uy
        | Protected = 3uy
        | Internal  = 4uy

    and [<AbstractClass>] Entity =
        val mutable Name           : string
        val mutable SourceName     : option<string>
        val mutable Type           : T
        val mutable AccessModifier : AccessModifier
        val mutable Comment        : option<string>

        internal new (name, t) =
            {
                Name           = name
                SourceName     = None
                Type           = t
                AccessModifier = AccessModifier.Public
                Comment        = None
            }

        member internal this.Clone() = (this.MemberwiseClone()) :?> Entity

        /// Clones the specified entity object, invokes the
        /// function to perform changes on it as side-effects and then
        /// returns the updated entity object.
        static member Update<'TEntity when 'TEntity :> Entity>
            f (x: 'TEntity) : 'TEntity =
                let clone = x.Clone() :?> 'TEntity
                f clone
                clone

    and [<AbstractClass>] TypeDeclaration  =
        inherit Entity
        val mutable Id         : Type.Id
        val mutable Generics   : list<string>
        val mutable Methods    : list<Method>
        val mutable Properties : list<Property>

        internal new (name) =
            let id = Type.Id ()
            {
                inherit Entity(name, Type.DeclaredType id)
                Id = id
                Generics = []
                Methods = []
                Properties = []
            }

        /// Assigns the associated type.
        static member ( |=> )<'T when 'T :> TypeDeclaration>
            (this: 'T, t: Type.IType) =
                let x = this.Clone() :?> 'T
                x.Type <- t.Type
                match t.Type with
                | Type.DeclaredType id -> x.Id <- id
                | Type.SpecializedType(Type.DeclaredType id, _) -> x.Id <- id
                | _ -> ()
                x

        /// Constructs a union type.
        static member ( + ) (this: TypeDeclaration, x: Type.IType) =
            this.Type + x

        /// Constructs a tuple type.
        static member ( * ) (this: TypeDeclaration, x: Type.IType) =
            this.Type * x

        /// Adds the class to parameters.
        static member ( * ) (this: TypeDeclaration, x: Type.Parameter) =
            this.Type * x

        /// Supports the `op_Dynamic` operator.
        static member op_Dynamic (this: TypeDeclaration, name: string) =
            Type.Type.op_Dynamic(this.Type, name)

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
        static member ( |+> ) (this: Interface, xs: list<IInterfaceMember>) =
            (this, xs)
            ||> List.fold (fun x y -> y.AddTo x)

        /// Applies an update.
        static member ( |=> ) (this: Interface, x: IInterfaceProperty) =
            x.SetOn this

    and Class =
        inherit TypeDeclaration
        val mutable BaseClass             : option<T>
        val mutable ImplementedInterfaces : list<T>
        val mutable Constructors          : list<Constructor>
        val mutable NestedClasses         : list<Class>
        val mutable NestedInterfaces      : list<Interface>

        internal new (name) =
            {
                inherit TypeDeclaration(name)
                BaseClass               = None
                ImplementedInterfaces   = []
                Constructors            = []
                NestedClasses           = []
                NestedInterfaces        = []
            }

        /// Applies updates.
        static member ( |+> ) (this: Class, xs: list<IClassMember>) =
            (this, xs)
            ||> List.fold (fun x y -> y.AddTo x)

        /// Sets a property.
        static member ( |=> ) (this: Class, x: IClassProperty) =
            x.SetOn this

    and [<AbstractClass>] Member =
        inherit Entity

        val mutable IsStatic : bool

        abstract member AddTo : Class -> Class

        interface IClassMember with
            member this.AddTo x = this.AddTo x

        internal new (name, t) = 
            {
                inherit Entity(name, t)
                IsStatic = true
            }

    and [<AbstractClass>] MethodBase =
        inherit Member
        val mutable Inline : option<string>

        internal new (name, t) =
            {
                inherit Member(name, t)
                Inline = None
            }

    and Constructor =
        inherit MethodBase

        internal new (t, zero) = { inherit MethodBase("", t) }

        override this.AddTo x =
            x
            |> Entity.Update (fun x ->
                x.Constructors <- this :: x.Constructors)

        interface IClassMember with
            member this.AddTo x = this.AddTo x

    and Method =
        inherit MethodBase
        val mutable Generics : list<string>

        internal new (name, t) =
            {
                inherit MethodBase(name, t)
                Generics = []
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
        val mutable HasGetter    : bool
        val mutable HasSetter    : bool
        val mutable GetterInline : option<string>
        val mutable SetterInline : option<string>

        internal new (name, t) = 
            {
                inherit Member(name, t)
                GetterInline = None
                SetterInline = None
                HasGetter    = false
                HasSetter    = false
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

    and IInterfaceMember =
        abstract member AddTo : Interface -> Interface

    and IClassProperty =
        abstract member SetOn : Class -> Class

    and IInterfaceProperty =
        abstract member SetOn : Interface -> Interface

    type Namespace =
        {
            Name       : string
            Interfaces : list<Interface>
            Classes    : list<Class>
        }

    type Assembly =
        {
            Namespaces : list<Namespace>
        }
