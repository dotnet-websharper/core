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

/// Defines a reduced language that is suitable
/// for simple optimization, and efficient compilation to JavaScript.
///
/// The goal of Core is to define a JavaScript-like
/// language satisfying the following criteria:
///
/// (1) Has tractable semantics to simplify optimization.
///
/// (2) Is rich enough to express JavaScript efficiently.
///
/// (3) Can be read from a JavaScript subset and elaborated to
///     JavaScript with minimal loss of structure.
///
/// Expanding all JavaScript programs into Core
/// has not been a design goal, therefore certain constructs, such as
/// mutable variables and break statements have been removed for
/// simplicity.
module WebSharper.Core.JavaScript.Core

/// Represents unary operators. These are JavaScript unary operators
/// excluding increment, decrement and delete.
type UnaryOperator =
    | ``~`` = 0
    | ``-`` = 1
    | ``!`` = 2
    | ``+`` = 3
    | ``typeof`` = 4
    | ``void`` = 5

/// Represents binary operators. These are JavaScript binary operators
/// excluding assignment, sequence and member operators.
type BinaryOperator =
    | ``!==`` = 0
    | ``!=`` = 1
    | ``%`` = 2
    | ``&&`` = 3
    | ``&`` = 4
    | ``*`` = 5
    | ``+`` = 6
    | ``-`` = 7
    | ``/`` = 8
    | ``<<`` = 9
    | ``<=`` = 10
    | ``<`` = 11
    | ``===`` = 12
    | ``==`` = 13
    | ``>=`` = 14
    | ``>>>`` = 15
    | ``>>`` = 16
    | ``>`` = 17
    | ``^`` = 18
    | ``in`` = 19
    | ``instanceof`` = 20
    | ``|`` = 21
    | ``||`` = 22

/// Represents identifiers.  Reference equality is used in
/// representing bindings.  Names can be optionally provided, their
/// role is advisory.
[<Sealed>]
type Id =

    /// Constructs a new identifier.
    new : unit -> Id

    /// Constructs a new identifier with a given readable name hint.
    new : string -> Id

    /// Constructs a new identifier with an optional given readable name hint.
    new : string option -> Id

    /// Constructs a new identifier with a given readable name hint, can be set to mutable.
    new : string * bool -> Id

    /// Clones an identifier.
    new : Id -> Id

    /// Mutability flag.
    member IsMutable : bool

    /// Returns the readable name hint of the identifier, if provided.
    member Name : option<string> with get, set

    /// Satisfies the comparison constraint.
    interface System.IComparable

/// Represents self-evaluating literals.
type Literal =
    | Double of double
    | False
    | Integer of int64
    | Null
    | String of string
    | True
    | Undefined

    /// Lifts to an expression.
    static member ( !~ ) : Literal -> Expression

/// Represents expressions of the language.
and Expression =
    private
    | Application         of E * list<E>
    | Arguments
    | Binary              of E * BinaryOperator * E
    | Call                of E * E * list<E>
    | Constant            of Literal
    | FieldDelete         of E * E
    | FieldGet            of E * E
    | FieldSet            of E * E * E
    | ForEachField        of Id * E * E
    | ForIntegerRangeLoop of Id * E * E * E
    | Global              of list<string>
    | IfThenElse          of E * E * E
    | Lambda              of option<Id> * list<Id> * E
    | Let                 of Id * E * E
    | LetRecursive        of list<Id * E> * E
    | New                 of E * list<E>
    | NewArray            of list<E>
    | NewObject           of list<string * E>
    | NewRegex            of string
    | Runtime
    | Sequential          of E * E
    | Throw               of E
    | TryFinally          of E * E
    | TryWith             of E * Id * E
    | Unary               of UnaryOperator * E
    | Var                 of Id
    | VarSet              of Id * E
    | WhileLoop           of E * E
    | SourcePos           of E * Syntax.SourcePos

    static member ( + ) : E * E -> E
    static member ( - ) : E * E -> E
    static member ( * ) : E * E -> E
    static member ( / ) : E * E -> E
    static member ( % ) : E * E -> E
    static member ( &== ) : E * E -> E
    static member ( &!= ) : E * E -> E
    static member ( &=== ) : E * E -> E
    static member ( &!== ) : E * E -> E
    static member ( &< ) : E * E -> E
    static member ( &> ) : E * E -> E
    static member ( &<= ) : E * E -> E
    static member ( &>= ) : E * E -> E
    static member ( &>> ) : E * E -> E
    static member ( &<< ) : E * E -> E
    static member ( ? ) : E * string -> E
    static member ( !! ) : E -> E
    static member ( ~+ ) : E -> E
    static member ( ~- ) : E -> E

    member Void : E
    member TypeOf : E
    member In : E -> E
    member InstanceOf : E -> E

    member Item : E -> E        with get
    member Item : list<E> -> E  with get

and private E = Expression

val (|Application        |_|) : E -> (E * list<E>              ) option                         
val (|Arguments          |_|) : E -> (unit                     ) option        
val (|Binary             |_|) : E -> (E * BinaryOperator * E   ) option                              
val (|Call               |_|) : E -> (E * E * list<E>          ) option                       
val (|Constant           |_|) : E -> (Literal                  ) option               
val (|FieldDelete        |_|) : E -> (E * E                    ) option             
val (|FieldGet           |_|) : E -> (E * E                    ) option             
val (|FieldSet           |_|) : E -> (E * E * E                ) option                 
val (|ForEachField       |_|) : E -> (Id * E * E               ) option                  
val (|ForIntegerRangeLoop|_|) : E -> (Id * E * E * E           ) option                      
val (|Global             |_|) : E -> (list<string>             ) option                    
val (|IfThenElse         |_|) : E -> (E * E * E                ) option                 
val (|Lambda             |_|) : E -> (option<Id> * list<Id> * E) option                                 
val (|Let                |_|) : E -> (Id * E * E               ) option                  
val (|LetRecursive       |_|) : E -> (list<Id * E> * E         ) option                        
val (|New                |_|) : E -> (E * list<E>              ) option                   
val (|NewArray           |_|) : E -> (list<E>                  ) option               
val (|NewObject          |_|) : E -> (list<string * E>         ) option                        
val (|NewRegex           |_|) : E -> (string                   ) option              
val (|Runtime            |_|) : E -> (unit                     ) option        
val (|Sequential         |_|) : E -> (E * E                    ) option             
val (|Throw              |_|) : E -> (E                        ) option         
val (|TryFinally         |_|) : E -> (E * E                    ) option             
val (|TryWith            |_|) : E -> (E * Id * E               ) option                  
val (|Unary              |_|) : E -> (UnaryOperator * E        ) option                         
val (|Var                |_|) : E -> (Id                       ) option          
val (|VarSet             |_|) : E -> (Id * E                   ) option              
val (|WhileLoop          |_|) : E -> (E * E                    ) option             

val Application         : E * list<E>               -> E
val Arguments           :                              E
val Binary              : E * BinaryOperator * E    -> E
val Call                : E * E * list<E>           -> E
val Constant            : Literal                   -> E
val FieldDelete         : E * E                     -> E
val FieldGet            : E * E                     -> E
val FieldSet            : E * E * E                 -> E
val ForEachField        : Id * E * E                -> E
val ForIntegerRangeLoop : Id * E * E * E            -> E
val Global              : list<string>              -> E
val IfThenElse          : E * E * E                 -> E
val Lambda              : option<Id> * list<Id> * E -> E
val Let                 : Id * E * E                -> E
val LetRecursive        : list<Id * E> * E          -> E
val New                 : E * list<E>               -> E
val NewArray            : list<E>                   -> E
val NewObject           : list<string * E>          -> E
val NewRegex            : string                    -> E
val Runtime             :                              E
val Sequential          : E * E                     -> E
val Throw               : E                         -> E
val TryFinally          : E * E                     -> E
val TryWith             : E * Id * E                -> E
val Unary               : UnaryOperator * E         -> E
val Var                 : Id                        -> E
val VarSet              : Id * E                    -> E
val WhileLoop           : E * E                     -> E

/// Add a source mapping position to an expression.
val WithPos : Syntax.SourcePos -> E -> E

/// Transfer the source mapping position of the first argument to the
/// second expression if it exists.
val WithPosOf : E -> E -> E

/// Maps over the immediate sub-expressions. Expression forms
/// that bind variables are transformed by inserting Lambda,
/// so that Lambda is the only variable-binding form.
val Transform : (E -> E) -> E -> E

/// Folds over the immediate sub-expressions. Expression forms
/// that bind variables are transformed by inserting Lambda,
/// so that Lambda is the only variable-binding form.
val Fold<'T> : ('T -> E -> 'T) -> 'T -> E -> 'T

/// Checks that no identifier is bound twice in an expression.
val IsAlphaNormalized : E -> bool

/// Refreshes bound identifiers, so that no identifier is bound twice.
val AlphaNormalize : E -> E

/// Finds all free variables in an expression.
val GetFreeIds : E -> Set<Id>

/// Finds all mutable variables in an expression.
val GetMutableIds : E -> Set<Id>

/// Tests if an expression is ground (contains no free
/// identifiers).
val IsGround : E -> bool

/// Replaces free variables in an expression.
val Substitute : (Id -> option<E>) -> E -> E

/// Performs an optimization pass. Current optimizations include
/// local tail-call elimination that transforms LetRecursive nodes
/// to loops.  The complexity is bounded by O(n) in
/// the number of sub-expressions.
val Optimize : E -> E

/// Elaborates an expression to a JavaScript program.
val ToProgram : Preferences -> E -> Syntax.Program

/// Recognizes expressions from a subset of syntactic forms.
val Recognize : Syntax.Expression -> option<E>

val internal GlobalName : Preferences -> string
val internal RuntimeName : Preferences -> string
