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

namespace IntelliFactory.WebSharper.JavaScript.Ecma

open IntelliFactory.WebSharper.InterfaceGenerator
open IntelliFactory.WebSharper.JavaScript

#nowarn "25"

/// Defines the bindings to ECMA-262 5th edition JavaScript functions
/// and objects.
module Definition =
    module P = Pattern

    let EcmaFunctionT = Type.New()

    let EcmaObject =
        Generic - fun (a: CodeModel.TypeParameter) ->
        Class "Object"
        |+> Instance
            [
                "constructor" =? EcmaFunctionT
                "toString" => T<unit->string>
                "toLocaleString" => T<unit->string>
                "valueOf" => T<unit->obj>
                "hasOwnProperty" => T<string> ^-> T<bool>
                "isPrototypeOf" => T<obj->bool>
                "propertyIsEnumerable" => T<string> ^-> T<bool>
                "" =@ a |> Indexed T<string>            
                "self" =? T<obj> |> WithGetterInline "$this"      
            ]
        |+> Static [
                ObjectConstructor T<unit> 
                Constructor (!|(T<string> * a))?nameValuePairs |> WithInline "$wsruntime.NewObject($nameValuePairs)"
                "prototype" =? T<obj>
                "create" => T<obj>?proto * !?T<obj>?properties ^-> T<obj>                
                "getPrototypeOf" => T<obj> ^-> T<obj>
                "getOwnPropertyDescriptor" => T<obj->obj>
                "defineProperty" => T<obj*string*obj->obj>
                "defineProperties" => T<obj*obj->obj>
                "seal" => T<obj->obj> 
                "freeze" => T<obj->obj>
                "preventExtensions" => T<obj->obj>
                "isSealed" => T<obj->bool>
                "isFrozen" => T<obj->bool>
                "isExtensible" => T<obj->bool>
                "keys" => T<obj->string[]>
            ]
       
    /// The Array object is used to store multiple values in a single variable.
    let EcmaArray =
        Generic - fun (a: CodeModel.TypeParameter) ->
        let ReduceMethod name =
            Instance [
                name => (a * a * T<int> * !|a ^-> a)?callback ^-> a
                Generic - fun b -> name => (b * a * T<int> * !|a ^-> b)?callback * b?initialValue ^-> b
            ] 
        let CallbackMethod name cRes mRes =
            Instance [
                name => (a * T<int> * !|a ^-> cRes)?callback ^-> mRes
                Generic - fun t -> name => (t -* a * T<int> * !|a ^-> cRes)?callback * t?thisArg ^-> mRes
            ]     
        Class "Array"
        |=> Inherits EcmaObject.[a]
        |+> Instance [
                "concat" => !+ a ^-> !|a
                "join" => T<string> ^-> T<string>
                "pop" => T<unit> ^-> a
                "push" => !+ a ^-> T<int>
                "reverse" => T<unit> ^-> !|a
                "shift" => T<unit> ^-> a
                "slice" => !?T<int>?startPos * !?T<int>?endPos ^-> !|a
                "sort" => (a * a ^-> T<int>) + T<unit> ^-> !|a
                "splice" => T<int>?start * T<int>?delete *+ a ^-> !|a
                "unshift" => !+ a ^-> T<int>
                "indexOf" => a * !?T<int>?fromIndex ^-> T<int>
                "lastIndexOf" => a * !?T<int>?fromIndex ^-> T<int>
                "length" =@ T<int>
                "" =@ a |> Indexed T<int>
                "self" =? !|a |> WithGetterInline "$this"
             ]
        |+> ReduceMethod "reduce"
        |+> ReduceMethod "reduceRight"
        |+> CallbackMethod "every" T<bool> T<bool>
        |+> CallbackMethod "some" T<bool> T<bool>
        |+> CallbackMethod "forEach" T<bool> T<bool>
        |+> Generic * fun b -> CallbackMethod "map" b !|b
        |+> CallbackMethod "filter" T<bool> !|a
        |+> Static [
                Constructor (T<int>)
                Constructor (!+ a)
                "isArray" => T<obj->bool>
            ]

    let EcmaObjectT = EcmaObject.[T<obj>]

    let EcmaFunction =
        Class "Function" |> WithSourceName "Function"
        |=> Inherits EcmaObjectT
        |=> EcmaFunctionT
        |+> Instance 
            [
                "length" =? T<int>
                "apply" => T<obj> * !?T<obj[]>?args ^-> T<obj>
                "call" => T<obj> *+ T<obj> ^-> T<obj>
                "bind" => T<obj> *+ T<obj> ^-> TSelf
            ]
        |+> Static [
                Constructor (T<string> *+ T<string>)
                Generic - fun a b -> "as" => (a ^-> b)?f ^-> TSelf |> WithInline "$f"
                Generic - fun a b -> "as" => T<FuncWithArgs<_,_>>.[a, b]?f ^-> TSelf |> WithInline "$f" 
                Generic - fun a b -> "as" => T<FuncWithThis<_,_>>.[a, b]?f ^-> TSelf |> WithInline "$f"
                Generic - fun a b -> "as" => T<FuncWithRest<_,_>>.[a, b]?f ^-> TSelf |> WithInline "$f"
                Generic - fun a b c -> "as" => T<FuncWithRest<_,_,_>>.[a, b, c]?f ^-> TSelf |> WithInline "$f"
                Generic - fun a b c d -> "as" => T<FuncWithRest<_,_,_,_>>.[a, b, c, d]?f ^-> TSelf |> WithInline "$f"
                GenericN 5 - function [a; b; c; d; e] -> "as" => T<FuncWithRest<_,_,_,_,_>>.[a, b, c, d, e]?f ^-> TSelf |> WithInline "$f"
                GenericN 6 - function [a; b; c; d; e; f]-> "as" => T<FuncWithRest<_,_,_,_,_,_>>.[a, b, c, d, e, f]?f ^-> TSelf |> WithInline "$f"
                GenericN 7 - function [a; b; c; d; e; f; g] -> "as" => T<FuncWithRest<_,_,_,_,_,_,_>>.[a, b, c, d, e, f, g]?f ^-> TSelf |> WithInline "$f"
                GenericN 8 - function [a; b; c; d; e; f; g; h] -> "as" => T<FuncWithRest<_,_,_,_,_,_,_,_>>.[a, b, c, d, e, f, g, h]?f ^-> TSelf |> WithInline "$f"
                Generic - fun a b c -> "as" => T<FuncWithArgsRest<_,_,_>>.[a, b, c]?f ^-> TSelf |> WithInline "$f"
            ]

    /// A resgular expression is an object that describes a pattern of characters.
    let EcmaRegExp =
        Class "RegExp"
        |=> Inherits EcmaObjectT
        |+> Instance
            [
                "exec" => T<string> ^-> !|T<string>
                "test" => T<string> ^-> T<bool>
                "source" =? T<string>
                "global" =? T<bool>
                "ignoreCase" =? T<bool>
                "multiLine" =? T<bool>
                "lastIndex" =@ T<int>
            ]
        |+> Static [
                Constructor(T<string> * !?T<string>?flags)
            ]

    /// The String object is used to manipulate a stored piece of text.
    let EcmaString =
        Class "String"
        |=> Inherits EcmaObjectT
        |+> Instance 
            [
                "charAt" => T<int->string>
                "charCodeAt" => T<int->int>
                "concat" => !+ T<string> ^-> T<string>
                "indexOf" => T<string> * !?T<int>?pos ^-> T<int>
                "lastIndexOf" => T<string> * !?T<int>?pos ^-> T<int>
                "localeCompare" => T<obj> ^-> T<int>
                "match" => (EcmaRegExp + T<string>) ^-> T<string []>
                "replace" => (EcmaRegExp + T<string>) * T<string> ^-> T<string>
                "search" => !?EcmaRegExp ^-> T<int>
                "slice" => T<int>?startPos * !?T<int>?endPos ^-> T<string>
                "split" =>
                    (T<string> + EcmaRegExp) * !?T<int>?limit ^-> T<string[]>
                "substring" => T<int>?startPos * !?T<int>?endPos ^-> T<string>
                "toLowerCase" => T<unit->string>
                "toLocaleLowerCase" => T<unit->string>
                "toUpperCase" => T<unit->string>
                "toLocaleUpperCase" => T<unit->string>
                "trim" => T<unit->string>
                "length" =? T<int>
                "self" =? T<string> |> WithGetterInline "$this"
            ]
        |+> Static [   
                Constructor (T<unit> + T<obj>)
                "fromCharCode" => !+ T<int> ^-> T<string>
            ]

    /// The Boolean object is used to convert a non-Boolean value to a Boolean value (true or false).
    let EcmaBoolean =
        Class "Boolean"
        |=> Inherits EcmaObjectT
        |+> Instance [
                "self" =? T<bool> |> WithGetterInline "$this"      
            ]
        |+> Static [               
                Constructor (T<obj>) 
            ]

    /// The Number object is an object wrapper for primitive numeric values.
    let EcmaNumber =
        Class "Number"
        |=> Inherits EcmaObjectT
        |+> Instance
            [
                "toString" => T<int>?tobase ^-> T<string>
                "toFixed" => !?T<int>?fractionDigits ^-> T<string>
                "toExponential" => !?T<int>?fractionDigits ^-> T<string>
                "toPrecision" => T<double->string>
                "self" =? T<double> |> WithGetterInline "$this"      
            ]
        |+> Static [
                Constructor (T<unit> + T<obj>)
                "MAX_VALUE" =? T<double>
                "MIN_VALUE" =? T<double>
                "NaN" =? T<double>
                "NEGATIVE_INFINITY" =? T<double>
                "POSITIVE_INFINITY" =? T<double>
            ]

    /// The Math object allows you to perform mathematical tasks.
    let EcmaMath =
        let D = T<double>
        let F = D ^-> D
        Class "Math"
        |+> Static [
                "E" =? D
                "LN10" =? D
                "LN2" =? D
                "LOG2E" =? D
                "LOG10E"  =? D
                "PI" =? D
                "SQRT1_2" =? D
                "SQRT2" =? D
                "abs" => F
                "acos" => F
                "asin" => F
                "atan" => F
                "atan2" => D * D ^-> D
                "ceil" => D ^-> T<int>
                "cos" => F
                "exp" => F
                "floor" => D ^-> T<int>
                "log" => F
                "max" => !+ T<obj> ^-> D
                "min" => !+ T<obj> ^-> D
                "pow" => D * D ^-> D
                "random" => T<unit> ^-> D
                "round" => D ^-> T<int>
                "sin" => F
                "sqrt" => F
                "tan" => F
            ]

    /// The Date object is used to work with dates and times.
    let EcmaDate =
        let FullYear = T<int>?year * !?T<int>?month * !?T<int>?date ^-> T<unit>
        let Month    = T<int>?month * !?T<int>?date ^-> T<unit>
        let Date     = T<int>?date ^-> T<unit>
        let Hours =
            T<int>?hr * !?T<int>?min * !?T<int>?sec * !?T<int>?ms ^-> T<unit>
        let Minutes  = T<int>?min * !?T<int>?sec * !?T<int>?ms ^-> T<unit>
        let Seconds  = T<int>?sec * !?T<int>?ms ^-> T<unit>
        let Msec     = T<int>?ms ^-> T<unit>

        let DateArgs =
            T<int>?year * T<int>?month * !?T<int>?date *
            !?T<int>?hours * !?T<int>?minutes * !?T<int>?seconds *
            !?T<int>?ms
        
        let DateType = Class "Date" 
        DateType
        |=> Inherits EcmaObjectT
        |+> Instance [
                "toDateString" => T<unit->string>
                "toTimeString" => T<unit->string>
                "toLocaleString" => T<unit->string>
                "toLocaleDateString" => T<unit->string>
                "toLocaleTimeString" => T<unit->string>
                "toUTCString" => T<unit->string>
                "toISOString" => T<unit->string>
                "valueOf" => T<unit->obj>
                "getTime" => T<unit->int>
                "setTime" => T<int->unit>
                "getTimezoneOffset"  => T<unit->double>
                "toJSON" => !?T<bool>?key ^-> T<unit>

                "getFullYear" => T<unit->int>
                "getMonth" => T<unit->int>
                "getDate" => T<unit->int>
                "getDay" => T<unit->int>
                "getHours" => T<unit->int>
                "getMinutes" => T<unit->int>
                "getSeconds" => T<unit->int>
                "getMilliseconds" => T<unit->int>

                "getUTCFullYear" => T<unit->int>
                "getUTCMonth" => T<unit->int>
                "getUTCDate" => T<unit->int>
                "getUTCDay" => T<unit->int>
                "getUTCHours" => T<unit->int>
                "getUTCMinutes" => T<unit->int>
                "getUTCSeconds" => T<unit->int>
                "getUTCMilliseconds" => T<unit->int>

                "setFullYear" => FullYear
                "setMonth" => Month
                "setDate" => Date
                "setHours" => Hours
                "setMinutes" => Minutes
                "setSeconds" => Seconds
                "setMilliseconds" => T<int->unit>

                "setUTCFullYear" => FullYear
                "setUTCMonth" => Month
                "setUTCDate" => Date
                "setUTCHours" => Hours
                "setUTCMinutes" => Minutes
                "setUTCSeconds" => Seconds
                "setUTCMilliseconds" => T<int->unit>

                "self" =? T<System.DateTime> |> WithGetterInline "$0.getTime()"
            ]
        |+> Static [
                Constructor T<unit>
                Constructor (T<int> * T<int>)
                Constructor (T<int> * T<int> * T<int>)
                Constructor (T<int> * T<int> * T<int> * T<int>)
                Constructor (T<int> * T<int> * T<int> * T<int> * T<int>)
                Constructor (T<int> * T<int> * T<int> * T<int>  * T<int> * T<int>)
                Constructor (T<int> * T<int> * T<int> * T<int>  * T<int> * T<int> * T<int>)
                Constructor (T<string> + T<int>)
                "UTC" => DateArgs ^-> T<int>
                "now" => T<unit> ^-> T<int>
                "parse" => T<string> ^-> T<int>
            ]

    let EcmaError =
        Class "Error"
        |=> Inherits EcmaObjectT
        |+> Instance
            [
                "name" =? T<string>
                "message" =@ T<string>
                "self" =? T<exn> |> WithGetterInline "$this"
            ]

        |+> Static [ Constructor (T<string>)]

    let EcmaJSON =
        Class "JSON"
        |=> Inherits EcmaObjectT
        |+> Static
            [
                "parse" => T<string> * !?T<obj->obj->bool>?reviver ^-> T<obj>
                "stringify" => T<obj>?value * !?(T<obj->obj> + (Type.ArrayOf T<obj>))?replacer * !?(T<string> + T<int>)?space ^-> T<string>
            ]

    let Namespaces =
        [
            Namespace "IntelliFactory.WebSharper.JavaScript" [
                EcmaObject
                EcmaFunction
                EcmaArray
                EcmaString
                EcmaBoolean
                EcmaNumber
                EcmaMath
                EcmaDate
                EcmaRegExp
                EcmaError
                EcmaJSON
            ]
        ]
