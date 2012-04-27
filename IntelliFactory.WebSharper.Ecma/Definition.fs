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

namespace IntelliFactory.WebSharper.EcmaExtension

/// Defines the bindings to ECMA-262 5th edition JavaScript functions
/// and objects.
module Ecma =
    open IntelliFactory.WebSharper.InterfaceGenerator
    module P = Pattern

    let EcmaRegExpT = Type.New()
    let EcmaNumberT = Type.New()
    let EcmaBooleanT = Type.New()
    let EcmaFunctionT = Type.New()
    let EcmaObjectT = Type.New()
    let EcmaArrayT = Type.New()

    /// The JavaScript global properties and functions can be used with all the built-in JavaScript objects.
    let EcmaGlobal =
        let N = T<int> + T<float>
        Class "Global"
        |+> [
                "NaN" =? T<double>
                "Infinity" =? T<double>
                "undefined" =? T<obj>
                "eval" => T<string->obj>
                "parseInt" => T<string> * !?T<int>?radix ^-> T<int>
                "parseFloat" => T<string->double>
                "isNaN" => N ^-> T<bool>
                "isFinite" => N ^-> T<bool>
                "decodeURI" => T<string->string>
                "decodeURIComponent" => T<string->string>
                "encodeURI" => T<string->string>
                "encodeURIComponent" => T<string->string>
            ]

    let EcmaObject =
        Class "Object"
        |=> EcmaObjectT
        |+> Protocol
            [
                "constructor" =? EcmaFunctionT
                "toString" => T<unit->string>
                "toLocaleString" => T<unit->string>
                "valueOf" => T<unit->obj>
                "hasOwnProperty" => T<string->bool>
                "isPrototypeOf" => T<obj->bool>
                "propertyIsEnumerable" => T<string->bool>
            ]
        |+> [
                Constructor (T<unit> + T<obj>) 
                "prototype" =? EcmaObjectT
                "create" => T<obj>?proto * !?T<obj>?properties ^-> T<obj>                
                "getPrototypeOf" => T<obj> ^-> EcmaObjectT
                "getOwnPropertyDescriptor" => T<obj->obj>
                "defineProperty" => T<obj*string*obj->obj>
                "defineProperties" => T<obj*obj->obj>
                "seal" => T<obj->obj> // not implemented in firefox/chrome
                "freeze" => T<obj->obj> // not implemented in firefox/chrome
                "preventExtensions" => T<obj->obj> // not implemented in firefox/chrome
                "isSealed" => T<obj->bool> //not implemented in firefox/chrome
                "isFrozen" => T<obj->bool> // not implemented in firefox/chrome
                "isExtensible" => T<obj->bool> // not implemented in firefox/chrome
                "keys" => T<obj->string[]>
            ]

       
    let EcmaFunction =
        Class "Function"
        |=> Inherits EcmaObject
        |=> EcmaFunctionT
        |+> Protocol 
            [
                "length" =? T<int>
                "apply" => T<obj> * !?T<obj[]>?args ^-> T<obj>
                "call" => T<obj> *+ T<obj> ^-> T<obj>
                "bind" => T<obj> *+ T<obj> ^-> EcmaFunctionT
            ]
        |+> [
                Constructor (T<string> *+ T<string>)
            ]

    /// The Array object is used to store multiple values in a single variable.
    let EcmaArray =
        let WithCallback r x =
            let t = T<obj> * T<int> * EcmaArrayT
            ((t ^-> r) ^-> x) + ((T<obj> -* t ^-> r) * T<obj> ^-> x)
        let Reduce =
            let cb =
                T<obj>?previousValue * T<obj>?currentValue *
                T<int>?index * EcmaArrayT ^-> T<obj>
            cb * !?T<obj>?initialValue ^-> T<obj>
        Class "Array"
        |=> Inherits EcmaObject
        |=> EcmaArrayT
        |+> Protocol [
                "concat" => !+ T<obj> ^-> EcmaArrayT
                "join" => T<string->string>
                "pop" => T<unit->obj>
                "push" => !+ T<obj> ^-> T<int>
                "reverse" => T<unit> ^-> EcmaArrayT
                "shift" => T<unit->obj>
                "slice" => T<int>?startPos * !?T<int>?endPos ^-> EcmaArrayT
                "sort" => T<obj * obj -> int> + T<unit> ^-> EcmaArrayT
                "splice" => T<int>?start * T<int>?delete *+ T<obj> ^-> EcmaArrayT
                "unshift" => !+ T<obj> ^-> T<int>
                "indexOf" => T<obj> * !?T<int>?fromIndex ^-> T<int>
                "lastIndexOf" => T<obj> * !?T<int>?fromIndex ^-> T<int>
                "every" => WithCallback T<bool> T<bool>
                "some" => WithCallback T<bool> T<bool>
                "forEach" => WithCallback T<unit> T<unit>
                "map" => WithCallback T<obj> EcmaArrayT
                "filter" => WithCallback T<bool> EcmaArrayT
                "reduce" => Reduce
                "reduceRight" => Reduce
                "length" =@ T<int>
            ]
        |+> [
                Constructor (T<int>)
                Constructor (!+ T<obj>)
                "isArray" => T<obj->bool>
            ]

    let EcmaStringT = Type.New()

    /// The String object is used to manipulate a stored piece of text.
    let EcmaString =
        Class "String"
        |=> Inherits EcmaObject
        |+> Protocol 
            [
                "charAt" => T<int->string>
                "charCodeAt" => T<int->int>
                "concat" => !+ (T<string> + EcmaStringT) ^-> T<string>
                "indexOf" => T<string> * !?T<int>?pos ^-> T<int>
                "lastIndexOf" => T<string> * !?T<int>?pos ^-> T<int>
                "localeCompare" => T<obj> ^-> T<int>
                "match" => EcmaRegExpT + T<string> ^-> T<string []>
                "replace" => EcmaRegExpT * T<string> ^-> T<string>
                "search" => EcmaRegExpT ^-> T<int>
                "slice" => T<int>?startPos * !?T<int>?endPos ^-> T<string>
                "split" =>
                    (T<string> + EcmaRegExpT) * !?T<int>?limit ^-> T<string[]>
                "substring" => T<int>?startPos * !?T<int>?endPos ^-> T<string>
                "toLowerCase" => T<unit->string>
                "toLocaleLowerCase" => T<unit->string>
                "toUpperCase" => T<unit->string>
                "toLocaleUpperCase" => T<unit->string>
                "trim" => T<unit->string>
                "length" =? T<int>
            ]
        |+> [   
                Constructor (T<unit> + T<obj>)
                "fromCharCode" => !+ T<int> ^-> T<string>
            ]

    /// The Boolean object is used to convert a non-Boolean value to a Boolean value (true or false).
    let EcmaBoolean =
        Class "Boolean"
        |=> Inherits EcmaObject
        |+> [               
                Constructor (T<obj>) 
            ]

    /// The Number object is an object wrapper for primitive numeric values.
    let EcmaNumber =
        Class "Number"
        |=> Inherits EcmaObject
        |+> Protocol
            [
                "toString" => T<int>?tobase ^-> T<string>
                "toFixed" => !?T<int>?fractionDigits ^-> T<string>
                "toExponential" => !?T<int>?fractionDigits ^-> T<string>
                "toPrecision" => T<double->string>
            ]
        |+> [
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
        let F = T<double->double>
        Class "Math"
        |+> [
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
                "atan2" => T<double*double->double>
                "ceil" => T<double->int>
                "cos" => F
                "exp" => F
                "floor" => T<double->int>
                "log" => F
                "max" => !+ T<obj> ^-> T<double>
                "min" => !+ T<obj> ^-> T<double>
                "pow" => T<double> * T<double> ^-> T<double>
                "random" => T<unit->double>
                "round" => T<double->int>
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

        let Protocol =
            Protocol [
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
            ]

        let DateArgs =
            T<int>?year * T<int>?month * !?T<int>?date *
            !?T<int>?hours * !?T<int>?minutes * !?T<int>?seconds *
            !?T<int>?ms
        
        let DateType = Class "Date" 
        DateType
        |=> Inherits EcmaObject
        |+> Protocol
        |+> [
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

    /// A resgular expression is an object that describes a pattern of characters.
    let EcmaRegExp =
        Class "RegExp"
        |=> Inherits EcmaObject
        |+> Protocol
            [
                "exec" => T<string->string[]>
                "test" => T<string->bool>
                "source" =? T<string>
                "global" =? T<bool>
                "ignoreCase" =? T<bool>
                "multiLine" =? T<bool>
                "lastIndex" =@ T<int>
            ]
        |+> [
                Constructor(T<string> * !?T<string>?flags)
            ]

    let EcmaError =
        Class "Error"
        |=> Inherits EcmaObject
        |+> Protocol
            [
                "name" =? T<string>
                "message" =@ T<string>
            ]

        |+> [ Constructor (T<string>)]

    let EcmaJSON =
        Class "JSON"
        |=> Inherits EcmaObject
        |+> Protocol
            [
                "parse" => T<string> * !?T<obj->obj->bool>?reviver ^-> T<obj>
                "stringify" => T<obj>?value * !?(T<obj->obj> + (Type.ArrayOf T<obj>))?replacer * !?(T<string> + T<int>)?space ^-> T<string>
            ]

    let Assembly =
        Assembly [
            Namespace "IntelliFactory.WebSharper.EcmaScript" [
                EcmaGlobal
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
