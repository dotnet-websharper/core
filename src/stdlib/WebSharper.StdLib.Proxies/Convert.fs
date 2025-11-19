namespace WebSharper

open System
open WebSharper
open WebSharper.JavaScript

module Helpers =
    open WebSharper.JavaScript

    [<Inline "!!$v">]
    let convertToBool (v: 'T) = X<bool>
    

[<Proxy(typeof<System.Convert>)>]
type internal SystemConvertProxy =
    static member ToBoolean(b: bool) : bool = b
    static member ToBoolean(b: byte) : bool =  Helpers.convertToBool b
    static member ToBoolean(c: char) = raise <| System.InvalidCastException()
    static member ToBoolean(dt: DateTime) = raise <| System.InvalidCastException()
    static member ToBoolean(d: double) = Helpers.convertToBool d
    static member ToBoolean(i: int16) = Helpers.convertToBool i
    static member ToBoolean(i: int32) = Helpers.convertToBool i
    static member ToBoolean(i: int64) = Helpers.convertToBool i
    static member ToBoolean(s: sbyte) = Helpers.convertToBool s
    static member ToBoolean(s: single) = Helpers.convertToBool s
    static member ToBoolean(s: string) = bool.Parse(s)
    static member ToBoolean(u: uint16) = Helpers.convertToBool u
    static member ToBoolean(u: uint32) = Helpers.convertToBool u
    static member ToBoolean(u: uint64) = Helpers.convertToBool u


    static member ToByte(b: bool) : byte = if b then 1uy else 0uy
    static member ToByte(b: byte) : byte =  b
    static member ToByte(c: char) = int c |> byte
    static member ToByte(dt: DateTime) = raise <| System.InvalidCastException()
    static member ToByte(d: double) = int d |> byte
    static member ToByte(i: int16) = i |> byte
    static member ToByte(i: int32) = i |> byte
    static member ToByte(i: int64) = i |> byte
    static member ToByte(s: sbyte) = s |> byte
    static member ToByte(s: single) = int s |> byte
    static member ToByte(s: string) = byte s
    static member ToByte(u: uint16) = u |> byte
    static member ToByte(u: uint32) = u |> byte
    static member ToByte(u: uint64) = u |> byte

    static member ToSByte(b: bool) : sbyte = if b then 1y else 0y
    static member ToSByte(b: byte) : sbyte =  sbyte b
    static member ToSByte(c: char) = int c |> sbyte
    static member ToSByte(dt: DateTime) = raise <| System.InvalidCastException()
    static member ToSByte(d: double) = int d |> sbyte
    static member ToSByte(i: int16) = i |> sbyte
    static member ToSByte(i: int32) = i |> sbyte
    static member ToSByte(i: int64) = i |> sbyte
    static member ToSByte(s: sbyte) = s |> sbyte
    static member ToSByte(s: single) = int s |> sbyte
    static member ToSByte(s: string) = byte s
    static member ToSByte(u: uint16) = u |> sbyte
    static member ToSByte(u: uint32) = u |> sbyte
    static member ToSByte(u: uint64) = u |> sbyte


    static member ToChar(b: bool) : char = raise <| System.InvalidCastException()
    static member ToChar(b: byte) : char = char b
    static member ToChar(c: char) = c
    static member ToChar(dt: DateTime) = raise <| System.InvalidCastException()
    static member ToChar(d: double) = raise <| System.InvalidCastException()
    static member ToChar(i: int16) = char i
    static member ToChar(i: int32) = char i
    static member ToChar(i: int64) = char i
    static member ToChar(s: sbyte) = char s
    static member ToChar(s: single) = raise <| System.InvalidCastException()
    static member ToChar(s: string) = char s.[0]
    static member ToChar(u: uint16) = char u
    static member ToChar(u: uint32) = char u
    static member ToChar(u: uint64) = char u


    static member ToInt32(b: bool) : int = if b then 1 else 0
    static member ToInt32(b: byte) : int =  b |> int
    static member ToInt32(c: char) = int c
    static member ToInt32(dt: DateTime) = raise <| System.InvalidCastException()
    static member ToInt32(d: double) = d |> int
    static member ToInt32(i: int16) = i |> int
    static member ToInt32(i: int32) = i |> int
    static member ToInt32(i: int64) = i |> int
    static member ToInt32(s: sbyte) = s |> int
    static member ToInt32(s: single) = s |> int
    static member ToInt32(s: string) = Int32.Parse s
    static member ToInt32(u: uint16) = u |> int
    static member ToInt32(u: uint32) = u |> int
    static member ToInt32(u: uint64) = u |> int

    static member ToInt64(b: bool) : int64 = if b then 1l else 0l
    static member ToInt64(b: byte) =  b |> int64
    static member ToInt64(c: char) = int c |> int64
    static member ToInt64(dt: DateTime) = raise <| System.InvalidCastException()
    static member ToInt64(d: double) = int d |> int64
    static member ToInt64(i: int16) = i |> int64
    static member ToInt64(i: int32) = i |> int64
    static member ToInt64(i: int64) = i |> int64
    static member ToInt64(s: sbyte) = s |> int64
    static member ToInt64(s: single) = int s |> int64
    static member ToInt64(s: string) = Int64.Parse s
    static member ToInt64(u: uint16) = u |> int64
    static member ToInt64(u: uint32) = u |> int64
    static member ToInt64(u: uint64) = u |> int64

    static member ToInt16(b: bool) : int16 = if b then 1s else 0s
    static member ToInt16(b: byte) =  b |> int16
    static member ToInt16(c: char) = int c |> int16
    static member ToInt16(dt: DateTime) = raise <| System.InvalidCastException()
    static member ToInt16(d: double) = int d |> int16
    static member ToInt16(i: int16) = i |> int16
    static member ToInt16(i: int32) = i |> int16
    static member ToInt16(i: int64) = i |> int16
    static member ToInt16(s: sbyte) = s |> int16
    static member ToInt16(s: single) = int s |> int16
    static member ToInt16(s: string) = Int16.Parse s
    static member ToInt16(u: uint16) = u |> int16
    static member ToInt16(u: uint32) = u |> int16
    static member ToInt16(u: uint64) = u |> int16


    static member ToUInt32(b: bool) : uint = if b then 1u else 0u
    static member ToUInt32(b: byte) : uint =  b |> uint32
    static member ToUInt32(c: char) = int c |> uint32
    static member ToUInt32(dt: DateTime) = raise <| System.InvalidCastException()
    static member ToUInt32(d: double) = int d |> uint32
    static member ToUInt32(i: int16) = i |> uint32
    static member ToUInt32(i: int32) = i |> uint32
    static member ToUInt32(i: int64) = i |> uint32
    static member ToUInt32(s: sbyte) = s |> uint32
    static member ToUInt32(s: single) = int s |> uint32
    static member ToUInt32(s: string) = UInt32.Parse s
    static member ToUInt32(u: uint16) = u |> uint32
    static member ToUInt32(u: uint32) = u |> uint32
    static member ToUInt32(u: uint64) = u |> uint32

    static member ToUInt64(b: bool) : uint64 = if b then 1UL else 0UL
    static member ToUInt64(b: byte) =  b |> uint64
    static member ToUInt64(c: char) = int c |> uint64
    static member ToUInt64(dt: DateTime) = raise <| System.InvalidCastException()
    static member ToUInt64(d: double) = int d |> uint64
    static member ToUInt64(i: int16) = i |> uint64
    static member ToUInt64(i: int32) = i |> uint64
    static member ToUInt64(i: int64) = i |> uint64
    static member ToUInt64(s: sbyte) = s |> uint64
    static member ToUInt64(s: single) = int s |> uint64
    static member ToUInt64(s: string) = UInt64.Parse s
    static member ToUInt64(u: uint16) = u |> uint64
    static member ToUInt64(u: uint32) = u |> uint64
    static member ToUInt64(u: uint64) = u |> uint64

    static member ToUInt16(b: bool) : uint16 = if b then 1us else 0us
    static member ToUInt16(b: byte) =  b |> uint16
    static member ToUInt16(c: char) = int c |> uint16
    static member ToUInt16(dt: DateTime) = raise <| System.InvalidCastException()
    static member ToUInt16(d: double) = int d |> uint16
    static member ToUInt16(i: int16) = i |> uint16
    static member ToUInt16(i: int32) = i |> uint16
    static member ToUInt16(i: int64) = i |> uint16
    static member ToUInt16(s: sbyte) = s |> uint16
    static member ToUInt16(s: single) = int s |> uint16
    static member ToUInt16(s: string) = UInt16.Parse s
    static member ToUInt16(u: uint16) = u |> uint16
    static member ToUInt16(u: uint32) = u |> uint16
    static member ToUInt16(u: uint64) = u |> uint16

    static member ToDateTime(b: bool) = raise <| System.InvalidCastException()
    static member ToDateTime(b: byte) =  raise <| System.InvalidCastException()
    static member ToDateTime(c: char) = raise <| System.InvalidCastException()
    static member ToDateTime(dt: DateTime) = dt
    static member ToDateTime(d: double) = raise <| System.InvalidCastException()
    static member ToDateTime(i: int16) = raise <| System.InvalidCastException()
    static member ToDateTime(i: int32) = raise <| System.InvalidCastException()
    static member ToDateTime(i: int64) = raise <| System.InvalidCastException()
    static member ToDateTime(s: sbyte) = raise <| System.InvalidCastException()
    static member ToDateTime(s: single) = raise <| System.InvalidCastException()
    static member ToDateTime(s: string) = DateTime.Parse s
    static member ToDateTime(u: uint16) = raise <| System.InvalidCastException()
    static member ToDateTime(u: uint32) = raise <| System.InvalidCastException()
    static member ToDateTime(u: uint64) = raise <| System.InvalidCastException()

    static member ToSingle(b: bool) : single = if b then 1f else 0f
    static member ToSingle(b: byte) =  b |> float32
    static member ToSingle(c: char) = raise <| System.InvalidCastException()
    static member ToSingle(dt: DateTime) = raise <| System.InvalidCastException()
    static member ToSingle(d: double) = int d |> float32
    static member ToSingle(i: int16) = i |> float32
    static member ToSingle(i: int32) = i |> float32
    static member ToSingle(i: int64) = i |> float32
    static member ToSingle(s: sbyte) = s |> float32
    static member ToSingle(s: single) = int s |> float32
    static member ToSingle(s: string) = Single.Parse s
    static member ToSingle(u: uint16) = u |> float32
    static member ToSingle(u: uint32) = u |> float32
    static member ToSingle(u: uint64) = u |> float32

    static member ToDouble(b: bool) : double = if b then 1. else 0.
    static member ToDouble(b: byte) =  b |> double
    static member ToDouble(c: char) = raise <| System.InvalidCastException()
    static member ToDouble(dt: DateTime) = raise <| System.InvalidCastException()
    static member ToDouble(d: double) = int d |> double
    static member ToDouble(i: int16) = i |> double
    static member ToDouble(i: int32) = i |> double
    static member ToDouble(i: int64) = i |> double
    static member ToDouble(s: sbyte) = s |> double
    static member ToDouble(s: single) = int s |> double
    static member ToDouble(s: string) = Single.Parse s
    static member ToDouble(u: uint16) = u |> double
    static member ToDouble(u: uint32) = u |> double
    static member ToDouble(u: uint64) = u |> double