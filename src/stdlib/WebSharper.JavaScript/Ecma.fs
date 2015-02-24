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

namespace WebSharper.JavaScript.Ecma

open WebSharper.InterfaceGenerator
open WebSharper.JavaScript

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
                "" =@ a |> Indexed T<string>            
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
       

    /// A resgular expression is an object that describes a pattern of characters.
    let EcmaRegExp =
        Class "RegExp"
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

    /// The Number object is an object wrapper for primitive numeric values.
    let EcmaNumber =
        Class "Number"
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

    let EcmaString =
        Class "String"
        |+> Static [
            "fromCharCode" => !+ T<int> ^-> T<string>
        ]

    let EcmaArray =
        Class "Array"
        |+> Static [
            "isArray" => T<obj->bool>
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

    let EcmaJSON =
        Class "JSON"
        |+> Static
            [
                "parse" => T<string> * !?T<obj->obj->bool>?reviver ^-> T<obj>
                "stringify" => T<obj>?value * !?(T<obj->obj> + (Type.ArrayOf T<obj>))?replacer * !?(T<string> + T<int>)?space ^-> T<string>
            ]

    let Namespaces =
        [
            Namespace "WebSharper.JavaScript" [
                EcmaObject
                EcmaNumber
                EcmaString
                EcmaArray
                EcmaMath
                EcmaDate
                EcmaRegExp
                EcmaJSON
            ]
        ]
