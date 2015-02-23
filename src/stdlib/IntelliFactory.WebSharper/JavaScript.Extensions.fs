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

namespace IntelliFactory.WebSharper.JavaScript

[<AutoOpen>]
module Extensions =
    open IntelliFactory.WebSharper

    type System.Object with
        /// JavaScript-only.
        [<Inline "$this.constructor">]
        member this.Constructor = X<Function>         

        /// JavaScript-only.
        [<Inline "$this.toLocaleString()">]
        member this.ToLocaleString() = X<string>

        /// JavaScript-only.
        [<Inline "$this.valueOf()">]
        member this.ValueOf() = X<obj>

        /// JavaScript-only.
        [<Inline "$this.hasOwnProperty($prop)">]
        member this.HasOwnProperty(prop: string) = X<bool>

        /// JavaScript-only.
        [<Inline "$this.propertyIsEnumerable($prop)">]
        member this.PropertyIsEnumerable(prop: string) = X<bool>

    type ``[]``<'T> with
        /// JavaScript-only.
        [<Inline "$this.concat($value)">]
        member this.Concat(value: 'T) = X<'T[]>

        /// JavaScript-only.
        [<Inline "$this.concat($array)">]
        member this.Concat(array: 'T[]) = X<'T[]>

        /// JavaScript-only.
        [<Inline "Array.prototype.concat.apply($this, [$1].concat($2))">]
        member this.Concat(value1: 'T, [<System.ParamArray>] rest: 'T[]) = X<'T[]>

        /// JavaScript-only.
        [<Inline "Array.prototype.concat.apply($this, [$1].concat($2))">]
        member this.Concat(array1: 'T[], [<System.ParamArray>] rest: 'T[][]) = X<'T[]>

        /// JavaScript-only.
        [<Inline "$this.join()">]
        member this.Join() = X<string>

        /// JavaScript-only.
        [<Inline "$this.join($separator)">]
        member this.Join(separator: string) = X<string>

        /// JavaScript-only.
        [<Inline "$this.pop()">]
        member this.Pop() = X<'T>

        /// JavaScript-only.
        [<Inline "$this.push($element)">]
        member this.Push(element: 'T) = X<int>

//        /// JavaScript-only.
//        [<Inline "Array.prototype.push.apply($0, [$1].concat($2))">]
//        member this.Push(element1: 'T, [<System.ParamArray>] rest: 'T[]) = X<int>

        /// JavaScript-only.
        [<Inline "$this.reverse()">]
        member this.Reverse() = X<'T[]>

        /// JavaScript-only.
        [<Inline "$this.shift()">]
        member this.Shift() = X<'T>

        /// JavaScript-only.
        [<Inline "$this.slice()">]
        member this.Slice() = X<'T[]>

        /// JavaScript-only.
        [<Inline "$this.slice($startPos)">]
        member this.Slice(startPos: int) = X<'T[]>

        /// JavaScript-only.
        [<Inline "$this.slice($startPos, $endPos)">]
        member this.Slice(startPos: int, endPos: int) = X<'T[]>

        /// JavaScript-only.
        [<Inline "$this.sort()">]
        member this.Sort() = X<'T[]>

        /// JavaScript-only.
        [<Inline "$this.sort($wsruntime.CreateFuncWithArgs($compareFunction))">]
        member this.Sort(compareFunction: 'T * 'T -> int) = X<'T[]>

        /// JavaScript-only.
        [<Inline "$this.splice($startPos, $deleteCount)">]
        member this.Splice(startPos: int, deleteCount: int) = X<'T[]>

//        /// JavaScript-only.
//        [<Inline "Array.prototype.splice.apply($this, [$startPos, $deleteCount].concat($items))">]
//        member this.Splice(startPos: int, deleteCount: int, [<System.ParamArray>] items: 'T[]) = X<'T[]>

        /// JavaScript-only.
        [<Inline "$this.unshift($element)">]
        member this.Unshift(element: 'T) = X<int>

        /// JavaScript-only.
        [<Inline "Array.prototype.unshift.apply($0, [$1].concat($2))">]
        member this.Unshift(element1: 'T, [<System.ParamArray>] rest: 'T[]) = X<int>

        /// JavaScript-only.
        [<Inline "Array.isArray($obj)">]
        static member IsArray(obj: obj) = X<bool>

    type System.String with
        /// JavaScript-only.
        [<Inline "$this.charAt($index)">]
        member this.CharAt(index: int) = X<string>

        /// JavaScript-only.
        [<Inline "$this.charCodeAt($index)">]
        member this.CharCodeAt(index: int) = X<int>

        /// JavaScript-only.
        [<Inline "$this.concat($string)">]
        member this.Concat(string: string) = X<string>

        /// JavaScript-only.
        [<Inline "String.prototype.concat.apply($this, [$1].concat($2))">]
        member this.Concat(string1: string, [<System.ParamArray>] rest: 'T[]) = X<string>

        /// JavaScript-only.
        [<Inline "$this.localeCompare($compareString)">]
        member this.LocaleCompare(compareString: string) = X<int> 
        
        /// JavaScript-only.
        [<Inline "$this.localeCompare($compareString, $locale)">]
        member this.LocaleCompare(compareString: string, locale: string) = X<int> 

        /// JavaScript-only.
        [<Inline "$this.localeCompare($compareString, locales)">]
        member this.LocaleCompare(compareString: string, locales: string[]) = X<int> 

        // TODO: LocaleCompare with options

        // TODO : RegExp-related functions

        /// JavaScript-only.
        [<Inline "$this.toLocaleLowerCase()">]
        member this.ToLocaleLowerCase() = X<string> 

        /// JavaScript-only.
        [<Inline "$this.toLocaleUpperCase()">]
        member this.ToLocaleUpperCase() = X<string> 

    type System.Exception with
        /// JavaScript-only.
        [<Inline "$this.name">]
        member this.Name = X<string>

    type Date with
        [<Inline "$0.getTime()">]
        [<System.Obsolete "Deprecated. Use Self property instead.">]
        member this.ToDotNet() = X<System.DateTime>
    type System.DateTime with
        [<Inline "new Date($0)">]
        [<System.Obsolete "Deprecated. Use ToJS property instead.">]
        member this.ToEcma() = X<Date>
        [<Inline "new Date($0)">]
        member this.JS = X<Date>

    type Function with
        [<Inline "$0">]
        [<System.Obsolete "Deprecated. Use Self property instead.">]
        member this.ToDotNet<'T, 'R>() = X<'T -> 'R>

    type Number with
        [<Inline "$0">]
        [<System.Obsolete "Deprecated. Use Self property instead.">]
        member this.ToDotNet<'T when 'T: struct>() = X<'T>
    type System.Byte    with
        [<Inline "$0">]
        [<System.Obsolete "Deprecated. Use JS property instead.">]
        member this.ToEcma() = X<Number>
        [<Inline "$0">]
        member this.JS = X<Number>
    type System.SByte   with
        [<Inline "$0">]
        [<System.Obsolete "Deprecated. Use JS property instead.">]
        member this.ToEcma() = X<Number>
        [<Inline "$0">]
        member this.JS = X<Number>
    type System.Int16   with
        [<Inline "$0">]
        [<System.Obsolete "Deprecated. Use JS property instead.">]
        member this.ToEcma() = X<Number>
        [<Inline "$0">]
        member this.JS = X<Number>
    type System.Int32   with
        [<Inline "$0">]
        [<System.Obsolete "Deprecated. Use JS property instead.">]
        member this.ToEcma() = X<Number>      
        [<Inline "$0">]                       
        member this.JS = X<Number>            
    type System.Int64   with                  
        [<Inline "$0">]                       
        [<System.Obsolete "Deprecated. Use JS property instead.">]
        member this.ToEcma() = X<Number>      
        [<Inline "$0">]                       
        member this.JS = X<Number>            
    type System.UInt16  with                  
        [<Inline "$0">]                       
        [<System.Obsolete "Deprecated. Use JS property instead.">]
        member this.ToEcma() = X<Number>      
        [<Inline "$0">]                       
        member this.JS = X<Number>            
    type System.UInt32  with                  
        [<Inline "$0">]                       
        [<System.Obsolete "Deprecated. Use JS property instead.">]
        member this.ToEcma() = X<Number>
        [<Inline "$0">]
        member this.JS = X<Number>
    type System.UInt64  with
        [<Inline "$0">]
        [<System.Obsolete "Deprecated. Use JS property instead.">]
        member this.ToEcma() = X<Number>
        [<Inline "$0">]
        member this.JS = X<Number>
    type System.Single  with
        [<Inline "$0">]
        [<System.Obsolete "Deprecated. Use JS property instead.">]
        member this.ToEcma() = X<Number>
        [<Inline "$0">]
        member this.JS = X<Number>
    type System.Double  with
        [<Inline "$0">]
        [<System.Obsolete "Deprecated. Use JS property instead.">]
        member this.ToEcma() = X<Number>
        [<Inline "$0">]
        member this.JS = X<Number>
    type System.Decimal with
        [<Inline "$0">]
        [<System.Obsolete "Deprecated. Use JS property instead.">]
        member this.ToEcma() = X<Number>
        [<Inline "$0">]
        member this.JS = X<Number>
