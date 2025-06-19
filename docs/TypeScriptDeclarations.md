# WebSharper type declarations

WebSharper 9.1+ generates .d.ts type declaration files next to its .js output except for `html` project type, and `web` if `prebundle` is true.

## Create npm packages with TypeScript definitions included

This is how you can upgrade a WebSharper library project to produce code output for npm use.

* To `wsconfig.json`, add:
  ```json
  "javascriptExport": true,
  "dce": true,
  "outputDir": "build"
  ```
* Note: `.d.ts` file generation is now on by default, no need to specify it.
* Create an `assets` folder and run `npm init` there to set up a `package.json` file. Leave entry point on the default setting (`index.js`). We do this so that the `build` folder can be wiped on a clean safely.
* Add to your project file:
  ```xml
  <Target Name="CleanBuildDir" BeforeTargets="CoreCompile">
    <RemoveDir Directories="build" />
  </Target>
  
  <Target Name="CopyPackageJsonAndPack" AfterTargets="WebSharperCompile">
    <Copy SourceFiles="assets/package.json" DestinationFolder="build" />
    <Exec Command="npm pack" WorkingDirectory="build" />
  </Target>
  ```
* Build your project which now writes dead code eliminated output to `build` folder, then packages it into a `tgz` package with `npm`. You can test locally with `npm install PathToTgz`, and run `npm publish PathToTgz` when ready to push to the public registry.

## Type translations

- Numeric values except `int64` and `uint64` are translated to type `number`. Also `DateTime` and `TimeSpan` are translated to type `number`.
- The large numeric types as well as `bigint` is translated to type `bigint`.
- `string`, `char`, and `Guid`s are translated to type `string`.
- Both delegates and F# functions are translated to JavaScript functions.
- For `decimal` support, install the `WebSharper.MathJS` NuGet package as they are handled by `mathjs`. (No .d.ts support yet.)
- `System.Exception` is translated to type `Error`.
- Arrays and `System.Collections.Generic.List` as well as `Queue` and `Stack` are translated to a JavaScript type `T[]`.

### Structured types

- F# tuples and .NET `ValueTuple`s both get translated to TypeScript tuple (for example `[string, number]`).
- F# unions are translated to types with a `$` property for the union case tag, and `$0`, `$1` properties for the union fields. If the union type has no members, it will be translated to a plain object, typed as a union of an interface for each case. Functions are also created for constructing the union cases. If the union type has a member, then there will be a class also for the type and a combined type for the intersection of the cases and this class.
  F# code:
  ```fsharp
  [<JavaScript>]
  type PlainUnion = 
      | A of int 
      | B of string
  
  [<JavaScript>]
  type UnionWithMember = 
      | C of int 
      | D of string
      
      member this.Text = 
          match this with 
          | C x -> string x
          | D x -> x  
  ```
  TypeScript declaration output:
  ```typescript
  // File: WebSharper.Tests.PlainUnion.d.ts
  export function B(Item:string):PlainUnion
  export function A(Item:number):PlainUnion
  export interface A {
    $:0;
    $0:number;
  }
  export interface B {
    $:1;
    $0:string;
  }
  export type PlainUnion = (A | B)
  
  // File: WebSharper.Tests.UnionWithMember.d.ts
  export interface C {
    $:0;
    $0:number;
  }
  export interface D {
    $:1;
    $0:string;
  }
  export type UnionWithMember_T = (UnionWithMember & (C | D))
  export default class UnionWithMember {
    static D(Item:string):UnionWithMember_T
    static C(Item:number):UnionWithMember_T
    get Text():string
  }
  ```
- F# records are similarly translated to plain objects or classes. A record constructor function or static member `New` is emitted.
  F# code:
  ```fsharp
  [<JavaScript>]
  type PlainRecord = 
      {
          A: int
          B: int
      }
  
  [<JavaScript>]
  type RecordWithMember = 
      {
          C: int
          D: int
      }
    
      member this.Total = 
          this.C + this.D
  ```
  TypeScript declaration output:
  ```typescript
  // File: WebSharper.Tests.PlainRecord.d.ts
  export function New(A, B)
  export default interface PlainRecord {
    A:number;
    B:number;
  }
  
  // File: WebSharper.Tests.RecordWithMember.d.ts
  export default class RecordWithMember {
    C:number;
    D:number;
    get Total():number
    static New(C:number, D:number):RecordWithMember
  }
  ```

### Objects

Classes are translated to JavaScript classes. If there are multiple constructors in .NET, the JavaScript constructor will take a first string argument to disambiguate. All the signatures for constructors are exposed separately.

F# code:
```fsharp
[<JavaScript>]
type BaseClass(x) =
    let mutable x = x
    new() = BaseClass(0)
    member this.X = x
    [<Name "Incr_i">]
    member this.Incr(i) = x <- x + i
    abstract Incr: unit -> unit
    default this.Incr() = this.Incr(1)

[<JavaScript>]
type SubClass(x) =
    inherit BaseClass(x)
    override this.Incr() = base.Incr(2)
```
TypeScript declaration output:
```typescript
// File: WebSharper.Tests.BaseClass.d.ts
import Object from "../WebSharper.StdLib/System.Object"
export default class BaseClass extends Object {
  x:number;
  Incr_i(i:number):void
  get X():number
  Incr():void
  constructor(i:"New")
  constructor(i:"New_1", x:number)
}

// File: WebSharper.Tests.SubClass.d.ts
import BaseClass from "./WebSharper.Tests.BaseClass"
export default class SubClass extends BaseClass {
  Incr():void
  constructor(x:number)
}
```

### Interfaces

Interfaces are translated to TypeScript interfaces.

F# code:
```fsharp
type MyInterface =
    abstract Incr: int -> int
    [<Name "IncrTwo">]
    abstract IncrTwo: int -> int

[<JavaScript>]
module Inst =
    let getIntfObj() =
        { new MyInterface with 
            member this.Incr x = x + 1 
            member this.IncrTwo x = x + 2 
        }
```
TypeScript declaration output:
```typescript
// File: WebSharper.Tests.MyInterface.d.ts
export function isMyInterface(x):x is MyInterface
export default interface MyInterface {
  IncrTwo(a:number):number
  WebSharper_Tests_MyInterface$Incr(a:number):number
}

// File: WebSharper.Tests.Inst.d.ts
import MyInterface from "./WebSharper.Tests.MyInterface"
export function getIntfObj():MyInterface
```