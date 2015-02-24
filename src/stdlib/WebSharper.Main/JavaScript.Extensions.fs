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

namespace WebSharper.JavaScript

open System.Runtime.CompilerServices

[<AutoOpen; Extension>]
module Extensions =
    open WebSharper

    [<Extension>]
    type System.Object with
        /// JavaScript-only.
        [<Extension; Inline "$this.constructor">]
        member this.Constructor = X<Function>         

        /// JavaScript-only.
        [<Extension; Inline "$this.toLocaleString()">]
        member this.ToLocaleString() = X<string>

        /// JavaScript-only.
        [<Extension; Inline "$this.valueOf()">]
        member this.ValueOf() = X<obj>

        /// JavaScript-only.
        [<Extension; Inline "$this.hasOwnProperty($prop)">]
        member this.HasOwnProperty(prop: string) = X<bool>

        /// JavaScript-only.
        [<Extension; Inline "$this.propertyIsEnumerable($prop)">]
        member this.PropertyIsEnumerable(prop: string) = X<bool>

    [<Extension>]
    type ``[]``<'T> with
        /// JavaScript-only.
        [<Extension; Inline "Array.prototype.concat.apply($this, $values)">]
        member this.Concat([<System.ParamArray>] values: 'T[]) = X<'T[]>

        /// JavaScript-only.
        [<Extension; Inline "Array.prototype.concat.apply($this, $arrays)">]
        member this.Concat([<System.ParamArray>] arrays: 'T[][]) = X<'T[]>

        /// JavaScript-only.
        [<Extension; Inline "$this.join()">]
        member this.Join() = X<string>

        /// JavaScript-only.
        [<Extension; Inline "$this.join($separator)">]
        member this.Join(separator: string) = X<string>

        /// JavaScript-only.
        [<Extension; Inline "$this.pop()">]
        member this.Pop() = X<'T>

        /// JavaScript-only.
        [<Extension; Inline "Array.prototype.push.apply($this, $elements)">]
        member this.Push([<System.ParamArray>] elements: 'T[]) = X<int>

        /// JavaScript-only.
        [<Extension; Inline "$this.reverse()">]
        member this.Reverse() = X<'T[]>

        /// JavaScript-only.
        [<Extension; Inline "$this.shift()">]
        member this.Shift() = X<'T>

        /// JavaScript-only.
        [<Extension; Inline "$this.slice()">]
        member this.Slice() = X<'T[]>

        /// JavaScript-only.
        [<Extension; Inline "$this.slice($startPos)">]
        member this.Slice(startPos: int) = X<'T[]>

        /// JavaScript-only.
        [<Extension; Inline "$this.slice($startPos, $endPos)">]
        member this.Slice(startPos: int, endPos: int) = X<'T[]>

        /// JavaScript-only.
        [<Extension; Inline "$this.sort()">]
        member this.Sort() = X<'T[]>

        /// JavaScript-only.
        [<Extension; Inline "$this.sort($wsruntime.CreateFuncWithArgs($compareFunction))">]
        member this.Sort(compareFunction: 'T * 'T -> int) = X<'T[]>

        /// JavaScript-only.
        [<Extension; Inline "Array.prototype.splice.apply($this, [$startPos, $deleteCount].concat($items))">]
        member this.Splice(startPos: int, deleteCount: int, [<System.ParamArray>] items: 'T[]) = X<'T[]>

        /// JavaScript-only.
        [<Extension; Inline "Array.prototype.unshift.apply($this, $1elements)">]
        member this.Unshift([<System.ParamArray>] elements: 'T[]) = X<int>

        /// JavaScript-only.
        [<Extension; Inline "Array.isArray($obj)">]
        static member IsArray(obj: obj) = X<bool>

    [<Extension>]
    type System.String with
        /// JavaScript-only.
        [<Extension; Inline "$this.charAt($index)">]
        member this.CharAt(index: int) = X<string>

        /// JavaScript-only.
        [<Extension; Inline "$this.charCodeAt($index)">]
        member this.CharCodeAt(index: int) = X<int>

        /// JavaScript-only.
        [<Extension; Inline "String.prototype.concat.apply($this, $strings)">]
        member this.Concat([<System.ParamArray>] strings: 'T[]) = X<string>

        /// JavaScript-only.
        [<Extension; Inline "$this.localeCompare($compareString)">]
        member this.LocaleCompare(compareString: string) = X<int> 
        
        /// JavaScript-only.
        [<Extension; Inline "$this.localeCompare($compareString, $locale)">]
        member this.LocaleCompare(compareString: string, locale: string) = X<int> 

        /// JavaScript-only.
        [<Extension; Inline "$this.localeCompare($compareString, locales)">]
        member this.LocaleCompare(compareString: string, locales: string[]) = X<int> 

        // TODO: LocaleCompare with options

        /// JavaScript-only.
        [<Extension; Inline "$this.match($regexp)">]
        member this.Match(regexp: RegExp) = X<string[]>

        /// JavaScript-only.
        [<Extension; Inline "$this.match($regexp)">]
        member this.Match(regexp: string) = X<string[]>

        /// JavaScript-only.
        [<Extension; Inline "$this.replace($regexp, $newSubStr)">]
        member this.Replace(regexp: RegExp, newSubStr: string) = X<string>

        /// JavaScript-only.
        [<Extension; Inline "$this.replace($regexp, $func)">]
        member this.Replace(regexp: RegExp, func: Function) = X<string>

        /// JavaScript-only.
        [<Extension; Inline "$this.replace($regexp, $func)">]
        member this.Replace(regexp: string, func: Function) = X<string>

        /// JavaScript-only.
        [<Extension; Inline "$this.search($regexp)">]
        member this.Search(regexp: RegExp) = X<int>

        /// JavaScript-only.
        [<Extension; Inline "$this.search($regexp)">]
        member this.Search(regexp: string) = X<int>

        /// JavaScript-only.
        [<Extension; Inline "$this.slice($startPos)">]
        member this.Slice(startPos: int) = X<string>

        /// JavaScript-only.
        [<Extension; Inline "$this.slice($startPos, $endPos)">]
        member this.Slice(startPos: int, endPos: int) = X<string>

        /// JavaScript-only.
        [<Extension; Inline "$this.split()">]
        member this.Split() = X<string[]>

        /// JavaScript-only.
        [<Extension; Inline "$this.split($separator)">]
        member this.Split(separator: RegExp) =X<string[]>

        /// JavaScript-only.
        [<Extension; Inline "$this.split($separator)">]
        member this.Split(separator: string) =X<string[]>

        /// JavaScript-only.
        [<Extension; Inline "$this.split($separator, $limit)">]
        member this.Split(separator: RegExp, limit: int) =X<string[]>

        /// JavaScript-only.
        [<Extension; Inline "$this.split($separator, $limit)">]
        member this.Split(separator: string, limit: int) =X<string[]>

        /// JavaScript-only.
        [<Extension; Inline "$this.toLocaleLowerCase()">]
        member this.ToLocaleLowerCase() = X<string> 

        /// JavaScript-only.
        [<Extension; Inline "$this.toLocaleUpperCase()">]
        member this.ToLocaleUpperCase() = X<string> 

    [<Extension>]
    type System.Exception with
        /// JavaScript-only.
        [<Extension; Inline "$this.name">]
        member this.Name = X<string>

    [<Extension>]
    type Date with
        [<Extension; Inline "$0.getTime()">]
        [<System.Obsolete "Deprecated. Use Self property instead.">]
        member this.ToDotNet() = X<System.DateTime>

    [<Extension>]
    type System.DateTime with
        [<Extension; Inline "new Date($0)">]
        [<System.Obsolete "Deprecated. Use ToJS property instead.">]
        member this.ToEcma() = X<Date>
        [<Extension; Inline "new Date($0)">]
        member this.JS = X<Date>

    [<Extension>]
    type Function with
        [<Extension; Inline "$0">]
        [<System.Obsolete "Deprecated. Use Self property instead.">]
        member this.ToDotNet<'T, 'R>() = X<'T -> 'R>

    [<Extension>]
    type Number with
        [<Extension; Inline "$0">]
        [<System.Obsolete "Deprecated. Use Self property instead.">]
        member this.ToDotNet<'T when 'T: struct>() = X<'T>

    [<Extension>]
    type System.Byte    with
        [<Extension; Inline "$0">]
        [<System.Obsolete "Deprecated. Use JS property instead.">]
        member this.ToEcma() = X<Number>
        [<Extension; Inline "$0">]
        member this.JS = X<Number>

    [<Extension>]
    type System.SByte   with
        [<Extension; Inline "$0">]
        [<System.Obsolete "Deprecated. Use JS property instead.">]
        member this.ToEcma() = X<Number>
        [<Extension; Inline "$0">]
        member this.JS = X<Number>

    [<Extension>]
    type System.Int16   with
        [<Extension; Inline "$0">]
        [<System.Obsolete "Deprecated. Use JS property instead.">]
        member this.ToEcma() = X<Number>
        [<Extension; Inline "$0">]
        member this.JS = X<Number>

    [<Extension>]
    type System.Int32   with
        [<Extension; Inline "$0">]
        [<System.Obsolete "Deprecated. Use JS property instead.">]
        member this.ToEcma() = X<Number>      
        [<Extension; Inline "$0">]                       
        member this.JS = X<Number>            

    [<Extension>]
    type System.Int64   with                  
        [<Extension; Inline "$0">]                       
        [<System.Obsolete "Deprecated. Use JS property instead.">]
        member this.ToEcma() = X<Number>      
        [<Extension; Inline "$0">]                       
        member this.JS = X<Number>            

    [<Extension>]
    type System.UInt16  with                  
        [<Extension; Inline "$0">]                       
        [<System.Obsolete "Deprecated. Use JS property instead.">]
        member this.ToEcma() = X<Number>      
        [<Extension; Inline "$0">]                       
        member this.JS = X<Number>            

    [<Extension>]
    type System.UInt32  with                  
        [<Extension; Inline "$0">]                       
        [<System.Obsolete "Deprecated. Use JS property instead.">]
        member this.ToEcma() = X<Number>
        [<Extension; Inline "$0">]
        member this.JS = X<Number>

    [<Extension>]
    type System.UInt64  with
        [<Extension; Inline "$0">]
        [<System.Obsolete "Deprecated. Use JS property instead.">]
        member this.ToEcma() = X<Number>
        [<Extension; Inline "$0">]
        member this.JS = X<Number>

    [<Extension>]
    type System.Single  with
        [<Extension; Inline "$0">]
        [<System.Obsolete "Deprecated. Use JS property instead.">]
        member this.ToEcma() = X<Number>
        [<Extension; Inline "$0">]
        member this.JS = X<Number>

    [<Extension>]
    type System.Double  with
        [<Extension; Inline "$0">]
        [<System.Obsolete "Deprecated. Use JS property instead.">]
        member this.ToEcma() = X<Number>
        [<Extension; Inline "$0">]
        member this.JS = X<Number>

    [<Extension>]
    type System.Decimal with
        [<Extension; Inline "$0">]
        [<System.Obsolete "Deprecated. Use JS property instead.">]
        member this.ToEcma() = X<Number>
        [<Extension; Inline "$0">]
        member this.JS = X<Number>

    [<assembly: Extension>]
    do ()