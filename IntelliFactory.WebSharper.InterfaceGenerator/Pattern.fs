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

namespace IntelliFactory.WebSharper.InterfaceGenerator

/// Implements common code generation patterns in binding code.
module Pattern =
    type private T = Type.Type

    /// Constructs a new class with no constructors and a given
    /// list of static inline members.
    let EnumInlines name (values: seq<string * string>) =
        let t = Type.New ()
        Class name
        |=> t
        |+> [ for (n, c) in values ->
                Getter n t
                |> WithGetterInline c :> _
            ]

    /// Constructs a new class with no constructors and a given
    /// list of static inline members. The values of the members
    /// are strings with their names.
    let EnumStrings name (words: seq<string>) =
        EnumInlines name [for w in words -> (w, Util.Quote w)]

    /// Represents the properties of a configuration object type
    /// to be used by the Config function.
    type ConfigProperties =
        {
            Required : seq<string * T>
            Optional : seq<string * T>
        }

    /// Generates a configuration object type.
    let Config (name: string) (properties: ConfigProperties) =
        let t = Type.New()
        let ctor : Type.Parameters =
            { This = None
              Variable = None
              Arguments =
                [ for (n, t) in properties.Required ->
                    (t :> Type.IParameter).Parameter |=> n
                ]
            }
        let code =
            let ss =
                properties.Required
                |> Seq.mapi (fun i (n, _) ->
                    System.String.Format("{0}:${1}", Util.Quote n, i))
            System.String.Format("{{{0}}}", String.concat "," ss)
        Class name
        |+> Protocol
            [ for (n, t) in properties.Required do
                yield Getter n t :> _
              for (n, t) in properties.Optional do
                yield Property n t :> _
            ]
        |+> [ Constructor ctor |> WithInline code ]

