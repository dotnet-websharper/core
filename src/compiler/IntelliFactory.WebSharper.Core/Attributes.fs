// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2013 IntelliFactory
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

/// Defines custom attributes used by WebSharper projects.
module IntelliFactory.WebSharper.Core.Attributes

type private A = System.Attribute
type private T = System.AttributeTargets
type private U = System.AttributeUsageAttribute

/// Marks union cases or properties that should be compiled to constants.
[<Sealed; U(T.Property)>]
type ConstantAttribute private () =
    inherit A()

    /// Constructs a boolean constant annotation.
    new (value: bool) = ConstantAttribute()

    /// Constructs an integer constant annotation.
    new (value: int) = ConstantAttribute()

    /// Constructs a floating constant annotation.
    new (value: float) = ConstantAttribute()

    /// Constructs a string or a null constant annotation.
    new (value: string) = ConstantAttribute()

/// Marks methods and constructors for inline compilation to JavaScript.
/// Inline members work by expanding JavaScript code templates
/// with placeholders of the form such as $0, $x, $this or $value.
[<Sealed; U(T.Constructor|||T.Method|||T.Property)>]
type InlineAttribute() =
    inherit A()

    /// Constructs a new inlining annotation from a code template.
    new (template: string) = InlineAttribute()

/// Similar to InlineAttribute, but does not inline the method invocation.
[<Sealed; U(T.Constructor|||T.Method|||T.Property)>]
type DirectAttribute(template: string) =
    inherit A()

/// Marks methods, properties and constructors for compilation to JavaScript.
type JavaScriptAttribute =
    ReflectedDefinitionAttribute

/// Annotates methods with custom compilation rules. The supplied type
/// should implement Macros.IMacroDefinition and a default constructor.
[<Sealed; U(T.Constructor|||T.Method|||T.Property)>]
type MacroAttribute(def: System.Type) =
    inherit A()

/// Provides a runtime name for members when it differs from the F# name.
/// The constructor accepts either an explicit array of parts,
/// or a single string, in which case it is assumed to be dot-separated.
[<Sealed; U(T.Class|||T.Constructor|||T.Method|||T.Property)>]
type NameAttribute private () =
    inherit A()

    /// Constructs a qualified name from a dot-separated string.
    new (name: string) = NameAttribute()

    /// Constructs a qualified name from an explicit array of parts.
    new ([<System.ParamArray>] names: string []) = NameAttribute()

/// Declares a type to be a proxy for another type, identified directly or
/// by using an assembly-qualified name.
[<Sealed; U(T.Class)>]
type ProxyAttribute private () =
    inherit A()

    /// Constructs a new proxy link using an assembly-qualified name.
    new (assemblyQualifiedName: string) = ProxyAttribute()

    /// Constructs a new proxy link using a type directly.
    new (proxiedType: System.Type) = ProxyAttribute()

/// Marks methods for remote invocation.
[<Sealed; U(T.Method)>]
type RemoteAttribute() =
    inherit A()

/// Annotates members with dependencies. The type passed to the constructor
/// must implement Resources.IResourceDefinition and a default constructor.
[<Sealed; U(T.Assembly|||T.Class|||T.Constructor|||T.Method,
            AllowMultiple=true)>]
type RequireAttribute(def: System.Type) =
    inherit A()

/// Marks members that should be compiled by-name.
[<Sealed; U(T.Class|||T.Constructor|||T.Method|||T.Property)>]
type StubAttribute() =
    inherit A()
