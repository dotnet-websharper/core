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

open System

/// Implements common code generation patterns in binding code.
module Pattern =
    type private T = Type.Type

    /// Constructs a new class with no constructors and a given
    /// list of static inline members.
    let EnumInlines name (values: seq<string * string>) =
        let t = Type.New ()
        Class name
        |=> t
        |+> Static 
            [ for (n, c) in values ->
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
        static member Empty =
            {
                Required = []
                Optional = []
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
                    String.Format("{0}:${1}", Util.Quote n, i))
            String.Format("{{{0}}}", String.concat "," ss)
        Class name
        |+> Instance
            [ for (n, t) in properties.Required do
                yield Getter n t :> _
              for (n, t) in properties.Optional do
                yield Property n t :> _
            ]
        |+> Static [ Constructor ctor |> WithInline code ]

    type ConfigObsProperties =
        {
            Required : seq<string * T>
            Optional : seq<string * T>
            Obsolete : seq<string * T>
        }
        static member Empty =
            {
                Required = []
                Optional = []
                Obsolete = []
            } 

    /// Generates a configuration object type with Obsolete support.
    let ConfigObs (name: string) (properties: ConfigObsProperties) =
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
                    String.Format("{0}:${1}", Util.Quote n, i))
            String.Format("{{{0}}}", String.concat "," ss)
        Class name
        |+> Instance
            [ for (n, t) in properties.Required do
                yield Getter n t :> _
              for (n, t) in properties.Optional do
                yield Property n t :> _
              for (n, t) in properties.Obsolete do
                yield Property n t |> Obsolete :> _
            ]
        |+> Static [ Constructor ctor |> WithInline code ]
