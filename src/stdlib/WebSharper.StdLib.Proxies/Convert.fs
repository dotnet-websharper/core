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
    [<Inline>] static member ToBoolean(b: bool) : bool = b
    [<Inline>] static member ToBoolean(b: byte) : bool =  Helpers.convertToBool b
    [<Inline>] static member ToBoolean(c: char) : bool = raise <| System.InvalidCastException()
    [<Inline>] static member ToBoolean(dt: DateTime) : bool = raise <| System.InvalidCastException()
    [<Inline>] static member ToBoolean(d: double) = Helpers.convertToBool d
    [<Inline>] static member ToBoolean(i: int16) = Helpers.convertToBool i
    [<Inline>] static member ToBoolean(i: int32) = Helpers.convertToBool i
    [<Inline>] static member ToBoolean(i: int64) = Helpers.convertToBool i
    [<Inline>] static member ToBoolean(s: sbyte) = Helpers.convertToBool s
    [<Inline>] static member ToBoolean(s: single) = Helpers.convertToBool s
    [<Inline>] static member ToBoolean(s: string) = bool.Parse(s)
    [<Inline>] static member ToBoolean(u: uint16) = Helpers.convertToBool u
    [<Inline>] static member ToBoolean(u: uint32) = Helpers.convertToBool u
    [<Inline>] static member ToBoolean(u: uint64) = Helpers.convertToBool u

    [<Inline>] static member ToByte(b: bool) : byte = if b then 1uy else 0uy
    [<Inline>] static member ToByte(b: byte) : byte =  b
    [<Inline>] static member ToByte(c: char) = int c |> byte
    [<Inline>] static member ToByte(dt: DateTime) : byte = raise <| System.InvalidCastException()
    [<Inline>] static member ToByte(d: double) = int d |> byte
    [<Inline>] static member ToByte(i: int16) = i |> byte
    [<Inline>] static member ToByte(i: int32) = i |> byte
    [<Inline>] static member ToByte(i: int64) = i |> byte
    [<Inline>] static member ToByte(s: sbyte) = s |> byte
    [<Inline>] static member ToByte(s: single) = int s |> byte
    [<Inline>] static member ToByte(s: string) = byte s
    [<Inline>] static member ToByte(u: uint16) = u |> byte
    [<Inline>] static member ToByte(u: uint32) = u |> byte
    [<Inline>] static member ToByte(u: uint64) = u |> byte

    [<Inline>] static member ToSByte(b: bool) : sbyte = if b then 1y else 0y
    [<Inline>] static member ToSByte(b: byte) : sbyte =  sbyte b
    [<Inline>] static member ToSByte(c: char) = int c |> sbyte
    [<Inline>] static member ToSByte(dt: DateTime) : sbyte = raise <| System.InvalidCastException()
    [<Inline>] static member ToSByte(d: double) = int d |> sbyte
    [<Inline>] static member ToSByte(i: int16) = i |> sbyte
    [<Inline>] static member ToSByte(i: int32) = i |> sbyte
    [<Inline>] static member ToSByte(i: int64) = i |> sbyte
    [<Inline>] static member ToSByte(s: sbyte) = s |> sbyte
    [<Inline>] static member ToSByte(s: single) = int s |> sbyte
    [<Inline>] static member ToSByte(s: string) = SByte.Parse s
    [<Inline>] static member ToSByte(u: uint16) = u |> sbyte
    [<Inline>] static member ToSByte(u: uint32) = u |> sbyte
    [<Inline>] static member ToSByte(u: uint64) = u |> sbyte

    [<Inline>] static member ToChar(b: bool) : char = raise <| System.InvalidCastException()
    [<Inline>] static member ToChar(b: byte) : char = char b
    [<Inline>] static member ToChar(c: char) = c
    [<Inline>] static member ToChar(dt: DateTime) : char = raise <| System.InvalidCastException()
    [<Inline>] static member ToChar(d: double) : char = raise <| System.InvalidCastException()
    [<Inline>] static member ToChar(i: int16) = char i
    [<Inline>] static member ToChar(i: int32) = char i
    [<Inline>] static member ToChar(i: int64) = char i
    [<Inline>] static member ToChar(s: sbyte) = char s
    [<Inline>] static member ToChar(s: single) : char = raise <| System.InvalidCastException()
    [<Inline>] static member ToChar(s: string) = char s.[0]
    [<Inline>] static member ToChar(u: uint16) = char u
    [<Inline>] static member ToChar(u: uint32) = char u
    [<Inline>] static member ToChar(u: uint64) = char u

    [<Inline>] static member ToInt32(b: bool) : int = if b then 1 else 0
    [<Inline>] static member ToInt32(b: byte) : int =  b |> int
    [<Inline>] static member ToInt32(c: char) = int c
    [<Inline>] static member ToInt32(dt: DateTime) : int = raise <| System.InvalidCastException()
    [<Inline>] static member ToInt32(d: double) = d |> int
    [<Inline>] static member ToInt32(i: int16) = i |> int
    [<Inline>] static member ToInt32(i: int32) = i |> int
    [<Inline>] static member ToInt32(i: int64) = i |> int
    [<Inline>] static member ToInt32(s: sbyte) = s |> int
    [<Inline>] static member ToInt32(s: single) = s |> int
    [<Inline>] static member ToInt32(s: string) = Int32.Parse s
    [<Inline>] static member ToInt32(u: uint16) = u |> int
    [<Inline>] static member ToInt32(u: uint32) = u |> int
    [<Inline>] static member ToInt32(u: uint64) = u |> int

    [<Inline>] static member ToInt64(b: bool) : int64 = if b then 1l else 0l
    [<Inline>] static member ToInt64(b: byte) =  b |> int64
    [<Inline>] static member ToInt64(c: char) = int c |> int64
    [<Inline>] static member ToInt64(dt: DateTime) : int64 = raise <| System.InvalidCastException()
    [<Inline>] static member ToInt64(d: double) = int d |> int64
    [<Inline>] static member ToInt64(i: int16) = i |> int64
    [<Inline>] static member ToInt64(i: int32) = i |> int64
    [<Inline>] static member ToInt64(i: int64) = i |> int64
    [<Inline>] static member ToInt64(s: sbyte) = s |> int64
    [<Inline>] static member ToInt64(s: single) = int s |> int64
    [<Inline>] static member ToInt64(s: string) = Int64.Parse s
    [<Inline>] static member ToInt64(u: uint16) = u |> int64
    [<Inline>] static member ToInt64(u: uint32) = u |> int64
    [<Inline>] static member ToInt64(u: uint64) = u |> int64

    [<Inline>] static member ToInt16(b: bool) : int16 = if b then 1s else 0s
    [<Inline>] static member ToInt16(b: byte) =  b |> int16
    [<Inline>] static member ToInt16(c: char) = int c |> int16
    [<Inline>] static member ToInt16(dt: DateTime) : int16 = raise <| System.InvalidCastException()
    [<Inline>] static member ToInt16(d: double) = int d |> int16
    [<Inline>] static member ToInt16(i: int16) = i |> int16
    [<Inline>] static member ToInt16(i: int32) = i |> int16
    [<Inline>] static member ToInt16(i: int64) = i |> int16
    [<Inline>] static member ToInt16(s: sbyte) = s |> int16
    [<Inline>] static member ToInt16(s: single) = int s |> int16
    [<Inline>] static member ToInt16(s: string) = Int16.Parse s
    [<Inline>] static member ToInt16(u: uint16) = u |> int16
    [<Inline>] static member ToInt16(u: uint32) = u |> int16
    [<Inline>] static member ToInt16(u: uint64) = u |> int16

    [<Inline>] static member ToUInt32(b: bool) : uint = if b then 1u else 0u
    [<Inline>] static member ToUInt32(b: byte) : uint =  b |> uint32
    [<Inline>] static member ToUInt32(c: char) = int c |> uint32
    [<Inline>] static member ToUInt32(dt: DateTime) : uint32 = raise <| System.InvalidCastException()
    [<Inline>] static member ToUInt32(d: double) = int d |> uint32
    [<Inline>] static member ToUInt32(i: int16) = i |> uint32
    [<Inline>] static member ToUInt32(i: int32) = i |> uint32
    [<Inline>] static member ToUInt32(i: int64) = i |> uint32
    [<Inline>] static member ToUInt32(s: sbyte) = s |> uint32
    [<Inline>] static member ToUInt32(s: single) = int s |> uint32
    [<Inline>] static member ToUInt32(s: string) = UInt32.Parse s
    [<Inline>] static member ToUInt32(u: uint16) = u |> uint32
    [<Inline>] static member ToUInt32(u: uint32) = u |> uint32
    [<Inline>] static member ToUInt32(u: uint64) = u |> uint32

    [<Inline>] static member ToUInt64(b: bool) : uint64 = if b then 1UL else 0UL
    [<Inline>] static member ToUInt64(b: byte) =  b |> uint64
    [<Inline>] static member ToUInt64(c: char) = int c |> uint64
    [<Inline>] static member ToUInt64(dt: DateTime) : uint64 = raise <| System.InvalidCastException()
    [<Inline>] static member ToUInt64(d: double) = int d |> uint64
    [<Inline>] static member ToUInt64(i: int16) = i |> uint64
    [<Inline>] static member ToUInt64(i: int32) = i |> uint64
    [<Inline>] static member ToUInt64(i: int64) = i |> uint64
    [<Inline>] static member ToUInt64(s: sbyte) = s |> uint64
    [<Inline>] static member ToUInt64(s: single) = int s |> uint64
    [<Inline>] static member ToUInt64(s: string) = UInt64.Parse s
    [<Inline>] static member ToUInt64(u: uint16) = u |> uint64
    [<Inline>] static member ToUInt64(u: uint32) = u |> uint64
    [<Inline>] static member ToUInt64(u: uint64) = u |> uint64

    [<Inline>] static member ToUInt16(b: bool) : uint16 = if b then 1us else 0us
    [<Inline>] static member ToUInt16(b: byte) =  b |> uint16
    [<Inline>] static member ToUInt16(c: char) = int c |> uint16
    [<Inline>] static member ToUInt16(dt: DateTime) : uint16 = raise <| System.InvalidCastException()
    [<Inline>] static member ToUInt16(d: double) = int d |> uint16
    [<Inline>] static member ToUInt16(i: int16) = i |> uint16
    [<Inline>] static member ToUInt16(i: int32) = i |> uint16
    [<Inline>] static member ToUInt16(i: int64) = i |> uint16
    [<Inline>] static member ToUInt16(s: sbyte) = s |> uint16
    [<Inline>] static member ToUInt16(s: single) = int s |> uint16
    [<Inline>] static member ToUInt16(s: string) = UInt16.Parse s
    [<Inline>] static member ToUInt16(u: uint16) = u |> uint16
    [<Inline>] static member ToUInt16(u: uint32) = u |> uint16
    [<Inline>] static member ToUInt16(u: uint64) = u |> uint16

    [<Inline>] static member ToDateTime(b: bool) : DateTime = raise <| System.InvalidCastException()
    [<Inline>] static member ToDateTime(b: byte) : DateTime =  raise <| System.InvalidCastException()
    [<Inline>] static member ToDateTime(c: char) : DateTime = raise <| System.InvalidCastException()
    [<Inline>] static member ToDateTime(dt: DateTime) = dt
    [<Inline>] static member ToDateTime(d: double) : DateTime = raise <| System.InvalidCastException()
    [<Inline>] static member ToDateTime(i: int16) : DateTime = raise <| System.InvalidCastException()
    [<Inline>] static member ToDateTime(i: int32) : DateTime = raise <| System.InvalidCastException()
    [<Inline>] static member ToDateTime(i: int64) : DateTime = raise <| System.InvalidCastException()
    [<Inline>] static member ToDateTime(s: sbyte) : DateTime = raise <| System.InvalidCastException()
    [<Inline>] static member ToDateTime(s: single) : DateTime = raise <| System.InvalidCastException()
    [<Inline>] static member ToDateTime(s: string) = DateTime.Parse s
    [<Inline>] static member ToDateTime(u: uint16) : DateTime = raise <| System.InvalidCastException()
    [<Inline>] static member ToDateTime(u: uint32) : DateTime = raise <| System.InvalidCastException()
    [<Inline>] static member ToDateTime(u: uint64) : DateTime = raise <| System.InvalidCastException()

    [<Inline>] static member ToSingle(b: bool) : single = if b then 1f else 0f
    [<Inline>] static member ToSingle(b: byte) =  b |> float32
    [<Inline>] static member ToSingle(c: char) : single = raise <| System.InvalidCastException()
    [<Inline>] static member ToSingle(dt: DateTime) : single = raise <| System.InvalidCastException()
    [<Inline>] static member ToSingle(d: double) = int d |> float32
    [<Inline>] static member ToSingle(i: int16) = i |> float32
    [<Inline>] static member ToSingle(i: int32) = i |> float32
    [<Inline>] static member ToSingle(i: int64) = i |> float32
    [<Inline>] static member ToSingle(s: sbyte) = s |> float32
    [<Inline>] static member ToSingle(s: single) = int s |> float32
    [<Inline>] static member ToSingle(s: string) = Single.Parse s
    [<Inline>] static member ToSingle(u: uint16) = u |> float32
    [<Inline>] static member ToSingle(u: uint32) = u |> float32
    [<Inline>] static member ToSingle(u: uint64) = u |> float32

    [<Inline>] static member ToDouble(b: bool) : double = if b then 1. else 0.
    [<Inline>] static member ToDouble(b: byte) =  b |> double
    [<Inline>] static member ToDouble(c: char) : double = raise <| System.InvalidCastException()
    [<Inline>] static member ToDouble(dt: DateTime) : double = raise <| System.InvalidCastException()
    [<Inline>] static member ToDouble(d: double) = int d |> double
    [<Inline>] static member ToDouble(i: int16) = i |> double
    [<Inline>] static member ToDouble(i: int32) = i |> double
    [<Inline>] static member ToDouble(i: int64) = i |> double
    [<Inline>] static member ToDouble(s: sbyte) = s |> double
    [<Inline>] static member ToDouble(s: single) = int s |> double
    [<Inline>] static member ToDouble(s: string) = Double.Parse s
    [<Inline>] static member ToDouble(u: uint16) = u |> double
    [<Inline>] static member ToDouble(u: uint32) = u |> double
    [<Inline>] static member ToDouble(u: uint64) = u |> double
