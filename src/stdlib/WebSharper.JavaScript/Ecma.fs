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

namespace WebSharper.JavaScript.Ecma

open WebSharper.InterfaceGenerator
open WebSharper.JavaScript

#nowarn "25"

/// Defines the bindings to ECMA-262 5th edition JavaScript functions
/// and objects.
module Definition =
    module P = Pattern

    let EcmaObject =
        Class "Object"
        |+> Instance [
                "constructor" =? T<Function>
                "toLocaleString" => T<unit->string>
                "valueOf" => T<unit->obj>
                "hasOwnProperty" => T<string> ^-> T<bool>
                "isPrototypeOf" => T<obj->bool>
                "propertyIsEnumerable" => T<string> ^-> T<bool>
                "self" =? T<obj> |> WithGetterInline "$this"      
            ]
        |+> Static [
                ObjectConstructor T<unit>
                "prototype" =? TSelf
                "assign" => T<obj>?target *+ T<obj> ^-> T<obj>
                "create" => T<obj>?proto * !?T<obj>?properties ^-> T<obj>                
                "getPrototypeOf" => T<obj> ^-> T<obj>
                "getOwnPropertyDescriptor" => T<obj->obj>
                "getOwnPropertyNames" => T<obj->obj>
                "defineProperty" => T<obj*string*obj->obj>
                "defineProperties" => T<obj*obj->obj>
                "seal" => T<obj->obj> 
                "freeze" => T<obj->obj>
                "preventExtensions" => T<obj->obj>
                "isSealed" => T<obj->bool>
                "isFrozen" => T<obj->bool>
                "isExtensible" => T<obj->bool>
                "keys" => T<obj->string[]>
                "is" => T<obj> * T<obj> ^-> T<bool>
                "setPrototypeOf" => T<obj> * T<obj> ^-> T<obj>
            ]
       
    let EcmaObjectG =
        Generic - fun (a: CodeModel.TypeParameter) ->
        Class "Object"
        |=> Inherits EcmaObject
        |+> Instance
            [
                "" =@ a |> Indexed T<string>            
            ]
        |+> Static [
                Constructor (!|(T<string> * a))?nameValuePairs |> WithInline "$wsruntime.NewObject($nameValuePairs)"
            ]

    /// A resgular expression is an object that describes a pattern of characters.
    let EcmaRegExp =
        Class "RegExp"
        |=> Inherits EcmaObject
        |+> Instance
            [
                "exec" => T<string> ^-> !|T<string>
                "test" => T<string> ^-> T<bool>
                "source" =? T<string>
                "global" =? T<bool>
                "flags" =? T<string>
                "sticky" =? T<bool>
                "unicode" =? T<bool>
                "ignoreCase" =? T<bool>
                "multiLine" =? T<bool>
                "lastIndex" =@ T<int>
                "match" => T<string>?str ^-> T<string []>
                    |> WithInline "$str.match($this)"
                "replace" => T<string>?str * T<string>?newSubStr ^-> T<string>
                    |> WithInline "$str.replace($this, $newSubStr)"
                "replace" => T<string>?str * T<Function>?replaceFn ^-> T<string>
                    |> WithInline "$str.replace($this, $replaceFn)"
                "search" => T<string>?str ^-> T<int>
                    |> WithInline "$str.search($this)"
                "split" => T<string>?str ^-> T<string []>
                    |> WithInline "$str.split($this)"
            ]
        |+> Static [
                Constructor(T<string> * !?T<string>?flags)
                "prototype" =? TSelf
            ]

    /// The String object is used to manipulate a stored piece of text.
    let EcmaString =
        Class "String"
        |=> Inherits EcmaObject
        |+> Instance 
            [
                "charAt" => T<int->string>
                "charCodeAt" => T<int->int>
                "codePointAt" => T<int->int>
                "concat" => !+ T<string> ^-> T<string>
                "endsWith" => T<string> * !?T<int>?length ^-> T<bool>
                "includes" => T<string>?str * !?T<int>?beginPos ^-> T<bool>
                "indexOf" => T<string> * !?T<int>?pos ^-> T<int>
                "lastIndexOf" => T<string> * !?T<int>?pos ^-> T<int>
                "localeCompare" => T<obj> ^-> T<int>
                "match" => (EcmaRegExp + T<string>) ^-> T<string []>
                "normalize" => T<string->string>
                "repeat" => T<int> ^-> T<string>
                "replace" => (EcmaRegExp + T<string>) * (T<string> + (!+ T<obj> ^-> T<string>)) ^-> T<string>
                "search" => !?EcmaRegExp ^-> T<int>
                "slice" => T<int>?startPos * !?T<int>?endPos ^-> T<string>
                "split" =>
                    (T<string> + EcmaRegExp) * !?T<int>?limit ^-> T<string[]>
                "startsWith" => T<string> * !?T<int>?beginPos ^-> T<bool>
                "substr" => T<int>?startPos * !?T<int>?length ^-> T<string>
                "substring" => T<int>?startPos * !?T<int>?endPos ^-> T<string>
                "toLowerCase" => T<unit->string>
                "toLocaleLowerCase" => T<unit->string>
                "toUpperCase" => T<unit->string>
                "toLocaleUpperCase" => T<unit->string>
                "trim" => T<unit->string>
                "valueOf" => T<unit->string>
                "length" =? T<int>
                "self" =? T<string> |> WithGetterInline "$this"
            ]
        |+> Static [   
                Constructor (T<unit> + T<obj>)
                "fromCharCode" => !+ T<int> ^-> T<string>
                "prototype" =? TSelf
            ]

    /// The Boolean object is used to convert a non-Boolean value to a Boolean value (true or false).
    let EcmaBoolean =
        Class "Boolean"
        |=> Inherits EcmaObject
        |+> Instance [
                "self" =? T<bool> |> WithGetterInline "$this"      
            ]
        |+> Static [               
                Constructor (T<obj>) 
                "prototype" =? TSelf
            ]

    /// The Number object is an object wrapper for primitive numeric values.
    let EcmaNumber =
        Class "Number"
        |=> Inherits EcmaObject
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
                "EPSILON" =? T<double>
                "MAX_SAFE_INTEGER" =? T<double>
                "MIN_SAFE_INTEGER" =? T<double>
                "isNaN" => T<obj> ^-> T<bool>
                "isFinite" => T<obj> ^-> T<bool>
                "isInteger" => T<obj> ^-> T<bool>
                "isSafeInteger" => T<obj> ^-> T<bool>
                "parseFloat" => T<string> ^-> T<double>
                "parseInt" => T<string> * !?T<int>?radix ^-> T<int>
                "prototype" =? TSelf
            ]

    let EcmaArray =
        Class "Array"
        |+> Static [
                "isArray" => T<obj->bool>
                "prototype" =? TSelf
            ]

    let EcmaArrayG =
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
        |=> Inherits EcmaObject
        |+> Instance [
                "concat" => !+ a ^-> !|a
                "join" => !?T<string> ^-> T<string>
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
                "asinh" => F
                "atan" => F
                "atanh" => F
                "atan2" => D * D ^-> D
                "cbrt" => F
                "ceil" => D ^-> T<int>
                "clz32" => F
                "cos" => F
                "cosh" => F
                "exp" => F
                "expm1" => F
                "floor" => D ^-> T<int>
                "fround" => F
                "hypot" => !+ D ^-> D
                "imul" => D * D ^-> D
                "log" => F
                "log1p" => F
                "log10" => F
                "log2" => F
                "max" => !+ T<obj> ^-> D
                "min" => !+ T<obj> ^-> D
                "pow" => D * D ^-> D
                "random" => T<unit> ^-> D
                "round" => D ^-> T<int>
                "sign" => F
                "sin" => F
                "sinh" => F
                "sqrt" => F
                "tan" => F
                "tanh" => F
                "trunc" => F
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
                "prototype" =? TSelf
            ]

    let EcmaError =
        Class "Error"
        |=> Inherits EcmaObject
        |+> Instance
            [
                "name" =? T<string>
                "message" =@ T<string>
                "self" =? T<exn> |> WithGetterInline "$this"
            ]
        |+> Static [
                Constructor (!?T<string>?message)
                "prototype" =? TSelf
            ]

    let EcmaJSON =
        Class "JSON"
        |+> Static
            [
                "parse" => T<string> * !?T<obj->obj->bool>?reviver ^-> T<obj>
                "stringify" => T<obj>?value * (T<string>*T<obj>^->T<obj>)?replacer ^-> T<string>
                |> WithInteropInline (fun p -> sprintf "JSON.stringify(%s,%s)" (p"value") (p"replacer"))
                "stringify" => T<obj>?value * (T<string>*T<obj>^->T<obj>)?replacer * (T<string> + T<int>)?space ^-> T<string>
                |> WithInteropInline (fun p -> sprintf "JSON.stringify(%s,%s,%s)" (p"value") (p"replacer") (p "space"))
                "stringify" => T<obj>?value * !?(Type.ArrayOf T<obj>)?replacer * !?(T<string> + T<int>)?space ^-> T<string>
            ]

    let EcmaPromise =
        Generic - fun (a: CodeModel.TypeParameter) ->
        let resolveFn b = (a ^-> b) + (a ^-> TSelf.[b])
        let rejectFn b = (T<obj> ^-> b) + (T<obj> ^-> TSelf.[b])
        Class "Promise"
        |+> Static [
            Constructor ((a ^-> T<unit>) * (T<obj> ^-> T<unit>) ^-> T<unit>)?executor
            "all" => Type.ArrayOf TSelf.[a] ^-> TSelf.[Type.ArrayOf a]
            "race" => Type.ArrayOf TSelf.[a] ^-> TSelf.[a]
            "reject" => T<obj>?reason ^-> TSelf.[a]
            "resolve" => a?value ^-> TSelf.[a]
            "resolve" => TSelf.[a] ^-> TSelf.[a]
        ]
        |+> Instance [
            Generic - fun b -> "catch" => (rejectFn b) ^-> TSelf.[b]
            "finally" => (T<unit> ^-> T<unit>) ^-> TSelf.[a]
            Generic - fun b -> "then" => (resolveFn b)?onFulfilled * !?(rejectFn b)?onRejected ^-> TSelf.[b]
        ]

    let Namespaces =
        [
            Namespace "WebSharper.JavaScript" [
                EcmaObject
                EcmaObjectG
                EcmaNumber
                EcmaString
                EcmaArray
                EcmaArrayG
                EcmaBoolean
                EcmaError
                EcmaMath
                EcmaDate
                EcmaRegExp
                EcmaJSON
                EcmaPromise
            ]
        ]
