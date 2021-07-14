// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2018 IntelliFactory
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
namespace WebSharper.MathJS

open WebSharper
open WebSharper.InterfaceGenerator

module Definition =
    open System.Numerics

    let Vector = !| T<float>

    let Matrix = !| Vector
    
    let BaseNumber =
        Class "MathNumber"
        |+> Instance [
            Constructor Vector?a
            |> WithInline "$a"

            Constructor Matrix?a
            |> WithInline "$a"

            Constructor T<float>?a
            |> WithInline "$a"

            Constructor T<float32>?a
            |> WithInline "$a"

            Constructor T<bigint>?a
            |> WithInline "$a"

            Constructor T<Complex>?a
            |> WithInline "$a"

            Constructor T<string>?a
            |> WithInline "$a"

            Constructor T<bool>?a
            |> WithInline "$a"

            Constructor T<int8>?a
            |> WithInline "$a"

            Constructor T<int16>?a
            |> WithInline "$a"

            Constructor T<int32>?a
            |> WithInline "$a"

            Constructor T<int64>?a
            |> WithInline "$a"

            Constructor T<uint8>?a
            |> WithInline "$a"

            Constructor T<uint16>?a
            |> WithInline "$a"

            Constructor T<uint32>?a
            |> WithInline "$a"

            Constructor T<uint64>?a
            |> WithInline "$a"
        ]

    let UnitJSON =
        Class "UnitJSON"
        |+> Pattern.RequiredFields [
            "value", T<float> + T<int>
            "unit", T<string>            
        ]
        |+> Pattern.OptionalFields [
            "mathjs", T<string>
            "fixPrefix", T<bool>
        ]

    let Unit =
        Class "MathUnit"
        |=> Inherits BaseNumber
        |+> Instance [
            Constructor (T<string> + (BaseNumber * T<string>))

            "valueOf" =? T<string>

            "clone" =? TSelf

            "_isDerived" =? T<bool>

            "hasBase" =? T<bool>

            "equalBase" => T<bool>

            "equals" => TSelf ^-> T<bool>

            "multiply" => TSelf ^-> TSelf

            "divide" => TSelf ^-> TSelf

            "pow" => T<float> ^-> TSelf

            "abs" => T<float> ^-> TSelf

            "to" => TSelf ^-> TSelf

            "toNumber" => TSelf ^-> T<float>

            "toNumeric" => (TSelf ^-> T<float>) + (T<string> ^-> T<float>)

            "toString" => T<unit> ^-> T<string>

            "toJSON" => T<unit> ^-> T<obj>

            "formatUnits" => T<unit> ^-> T<string>

            "format" => T<obj> ^-> T<string>

            "fromJSON" => UnitJSON ^-> TSelf

            "splitUnits" => !| T<string> ^-> T<string>

            "toSI" => T<unit> ^-> TSelf
        ]

    let AllValues = 
        [|
            BaseNumber.Type
            T<float>
            T<int>
            Unit.Type
        |]
        |> List.ofArray

    let Matrices = 
        [|
            BaseNumber.Type
        |]
        |> List.ofArray

    let Numbers = 
        [|
            BaseNumber.Type
            T<float>
            T<int>
            Unit.Type
        |]
        |> List.ofArray
        
    let WithTypes values f =
        List.map f values
        |> List.reduce (fun l r -> l + r)

    let Scope
        = T<obj>

    let Expression
        = T<string>

    let Config =
        Pattern.Config "config" {
            Required = []
            Optional =
                [
                    "epsilon", T<float>
                    "matrix", T<string>
                    "number", T<string>
                    "precision", T<float>
                    "predictable", T<bool>
                    "randomSeed", T<string>
                ]
        }

    let Options =
        Pattern.Config "options" {
            Required = []
            Optional =
                [
                    "override", T<bool>
                    "silent", T<bool>
                    "wrap", T<bool>
                ]
        }

    let DerivativeOption =
        Pattern.Config "DerivativeOption" {
            Required = []
            Optional = 
                [
                    "simplify", T<bool>
                ]
        }

    let Chain =
        Class "math.type.Chain"

    let Parser =
        Class "Parser"
        |+> Instance [
            "clear" => T<unit> ^-> T<unit>
            "evaluate" => T<string> ^-> BaseNumber
            "get" => T<string> ^-> BaseNumber + T<JavaScript.Function>
            "getAll" => T<unit> ^-> T<obj>
            "remove" => T<string> ^-> T<unit>
            "set" => T<string> * BaseNumber ^-> T<unit>
        ]

    let AccessorNode            = Class "math.expression.node.accessornode"
    let ArrayNode               = Class "math.expression.node.arraynode"
    let AssignmentNode          = Class "math.expression.node.assignmentnode"
    let BlockNode               = Class "math.expression.node.blocknode"
    let ConditionalNode         = Class "math.expression.node.conditionalnode"
    let ConstantNode            = Class "math.expression.node.constantnode"
    let FunctionAssignmentNode  = Class "math.expression.node.functionassignmentnode"
    let FunctionNode            = Class "math.expression.node.functionnode"
    let IndexNode               = Class "math.expression.node.indexnode"
    let ObjectNode              = Class "math.expression.node.objectnode"
    let OperatorNode            = Class "math.expression.node.operatornode"
    let ParenthesisNode         = Class "math.expression.node.parenthesisnode"
    let RangeNode               = Class "math.expression.node.rangenode"
    let RelationalNode          = Class "math.expression.node.relationalnode"
    let SymbolNode              = Class "math.expression.node.symbolnode"

    let ParenthesisOptions =
        Pattern.EnumStrings "ParenthesisOptions" [
            "keep"
            "auto"
            "all"
        ]

    let ImplicitOptions =
        Pattern.EnumStrings "ImplicitOptions" [
            "hide"
            "show"
        ]

    let OutputOptions =
        Pattern.Config "HtmlOptions" {
            Required = []
            Optional = [
                "parenthesis", ParenthesisOptions.Type
                "handler", T<JavaScript.Function>
                "implicit", ImplicitOptions.Type
            ]
        }

    let Node =
        Class "Node"
        |+> Instance [
            "clone" => T<unit> ^-> TSelf

            "cloneDeep" => T<unit> ^-> TSelf

            "compile" => T<unit> ^-> T<obj>

            "evaluate" => !? Scope ^-> BaseNumber

            "equals" => TSelf ^-> T<bool>

            "filter" => T<JavaScript.Function> ^-> !| TSelf

            "forEach" => T<JavaScript.Function> ^-> !| TSelf

            "map" => T<JavaScript.Function> ^-> !| TSelf

            "toHtml" => (T<unit> + OutputOptions.Type) ^-> T<string>

            "toString" => (T<unit> + OutputOptions.Type) ^-> T<string>

            "toTex" => (T<unit> + OutputOptions.Type) ^-> T<string>

            "transform" => T<JavaScript.Function> ^-> TSelf

            "traverse" => T<JavaScript.Function> ^-> T<unit>

            "comment" =? T<string>

            "isNode" =? T<bool>

            "type" =? T<string>
        ]

    let NullValue = Pattern.EnumInlines "NullValue" ["null", "null"]

    let NaNValue = Pattern.EnumInlines "NaNValue" ["naN", "NaN"]

    AccessorNode
        |=> Inherits Node
        |+> Instance [
            Constructor (Node * IndexNode)

            "object" =? Node
            "index" =? IndexNode
            "name" =? T<string>
        ]
        |> ignore

    ArrayNode
        |=> Inherits Node
        |+> Instance [
            Constructor !| Node

            "items" =? !| Node
        ]
        |> ignore

    AssignmentNode
        |=> Inherits Node
        |+> Instance [
            Constructor ((SymbolNode + AccessorNode) * IndexNode * Node)
            Constructor (SymbolNode * Node)

            "object" =? (SymbolNode + AccessorNode)

            "value" =? Node

            "index" =? IndexNode + NullValue

            "name" =? T<string>
        ]
        |> ignore

    BlockNode
        |=> Inherits Node
        |+> Instance [
            Constructor (!| (Node + (Node * T<bool>)))

            "blocks" =? !| (Node * T<bool>)
        ]
        |> ignore

    ConditionalNode
        |=> Inherits Node
        |+> Instance [
            Constructor (Node * Node * Node)

            "condition" =? Node

            "trueExpr" =? Node

            "falseExpr" =? Node
        ]
        |> ignore

    ConstantNode
        |=> Inherits Node
        |+> Instance [
            Constructor T<obj>

            "value" =? T<obj>
        ]
        |> ignore

    FunctionAssignmentNode
        |=> Inherits Node
        |+> Instance [
            Constructor (T<string> * !| T<string> * Node)

            "name" =? T<string>
            "params" =? !| T<string>
            "expr" =? Node
        ]
        |> ignore

    FunctionNode
        |=> Inherits Node
        |+> Instance [
            Constructor ((Node + T<string>) * !| Node)

            "fn" =? Node + T<string>
            "args" =? !| Node
        ]
        |> ignore

    IndexNode
        |=> Inherits Node
        |+> Instance [
            Constructor (!| Node * !? T<bool>)

            "dimension" =? !| Node
            "dotNotation" =? T<bool>
        ]
        |> ignore

    ObjectNode
        |=> Inherits Node
        |+> Instance [
            Constructor (T<string> * Node)

            "properties" =? T<string> * Node
        ]
        |> ignore

    OperatorNode
        |=> Inherits Node
        |+> Instance [
            Constructor (T<string> * T<string> * !| Node)

            "isUnary" => T<unit> ^-> T<bool>
            "isBinary" => T<unit> ^-> T<bool>

            "op" =? T<string>
            "fn" =? T<string>
            "args" =? !| Node
            "implicit" =? T<bool>
        ]
        |> ignore

    ParenthesisNode
        |=> Inherits Node
        |+> Instance [
            Constructor Node

            "content" =? Node
        ]
        |> ignore

    RangeNode
        |=> Inherits Node
        |+> Instance [
            Constructor (Node * Node * !? Node)

            "start" =? Node
            "end" =? Node
            "step" =? Node
        ]
        |> ignore

    RelationalNode
        |=> Inherits Node
        |+> Instance [
            Constructor ((!| T<string>) * (!| Node))

            "conditionals" =? !| T<string>
            "params" =? !| Node
        ]
        |> ignore

    SymbolNode
        |=> Inherits Node
        |+> Instance [
            Constructor T<string>

            "name" =? T<string>
        ]
        |> ignore

    let Index =
        Class "index"
        |+> Instance [
            Constructor (T<string> + T<int> + BaseNumber)
        ]

    

    let mathOps: CodeModel.IClassMember list =
        [
            "config" => Config.Type ^-> T<unit>
            |> WithComment "Configure the math engine."

            "chain" => WithTypes AllValues (fun t -> !? t ^-> Chain.Type)
            |> WithComment "Chain functions."

            "import" => (!| T<string * JavaScript.Function> + !| T<JavaScript.Function>) * !? Options.Type
            |> WithComment "Import custom function and values."

            "typed" => !? T<string> * T<obj> ^-> T<JavaScript.Function>
            |> WithComment "Creates a typed function."

            "factory" => T<string> * !| T<string> * T<JavaScript.Function> * !? T<obj> ^-> T<JavaScript.Function>
            |> WithComment "Creates a factory function"

            //construction
            "bignumber" => WithTypes AllValues (fun t -> (t ^-> T<bigint>) + (T<string> ^-> T<bigint>))

            "boolean" => WithTypes AllValues (fun t -> (t ^-> T<bool>) + T<string> ^-> T<bool>)

            "complex" => (T<unit> + T<float> + T<Complex> + T<string> + Vector ^-> T<Complex>) + (T<float> * T<string> ^-> T<Complex>)

            "createUnit" => T<string> * (T<string> * Unit.Type) * T<obj> ^-> Unit.Type

            "fraction" => (T<float> * T<float> ^-> T<float>) + (T<string> ^-> T<float>)

            "fraction" => WithTypes AllValues (fun t -> t ^-> t)

            "index" => Vector ^-> Index

            "matrix" => !? Matrix * !? T<string> * !? T<string> ^-> Matrix

            "number" => WithTypes AllValues (fun t -> t * !? T<string> ^-> t)
        
            "sparse" => WithTypes AllValues (fun t -> !? t * !? T<string> ^-> Matrix)

            "splitUnit" => Unit * !| T<string> ^-> Vector

            "string" => WithTypes AllValues (fun t -> t ^-> T<string>)

            "unit" => WithTypes AllValues (fun t -> t * T<string> ^-> Unit.Type) + (T<string> ^-> Unit.Type)

            //expression
            "compile" => (T<string> + !| T<string>) ^-> T<obj>

            "evaluate" => WithTypes AllValues (fun t -> (T<string> ^-> t) + (T<string> * Scope ^-> t) + (!| T<string> ^-> t) + (!| T<string> * Scope ^-> t))

            "help" => (T<JavaScript.Function> + T<string> + T<obj>) ^-> T<obj>

            "parse" => (T<string> + !| T<string>) ^-> T<obj>

            // parser customization

            "parse.isAlpha" => T<string> * T<string> * T<string> ^-> T<bool>

            "parse.isValidLatinOrGreek" => T<string> ^-> T<bool>

            "parse.isValidMathSymbol" => T<string> * T<string> ^-> T<bool>

            "parse.isWhitespace" => T<string> * T<int> ^-> T<bool>

            "parse.isDecimalMark" => T<string> * T<string> ^-> T<bool>

            "parse.isDigitDot" => T<string> ^-> T<bool>

            "parse.isDigit" => T<string> ^-> T<bool>

            "parse.isHexDigit" => T<string> ^-> T<bool>

            //algebra
            "derivative" => (Node + T<string>) * (SymbolNode + T<string>) * !? DerivativeOption ^-> Node

            "lsolve" => BaseNumber * BaseNumber ^-> BaseNumber

            "lsolveAll" => BaseNumber * BaseNumber ^-> BaseNumber

            "lup" => BaseNumber ^-> (Vector * Vector * Vector)

            "lusolve" => (Matrix + Vector + T<obj>) * BaseNumber ^-> BaseNumber

            "qr" => BaseNumber ^-> (Matrix * Matrix) + (Vector * Vector)

            "rationalize" => (Node + T<string>) * !? (T<bool> + T<obj>) * !? T<bool> ^-> Node + T<obj>

            "simplify" => (Node + T<string>) * !? (T<string * string> + T<string> + T<JavaScript.Function>) ^-> Node

            "slu" => Matrix * T<float> * T<float> ^-> T<obj>

            "usolve" => BaseNumber * BaseNumber ^-> BaseNumber

            "usolveAll" => BaseNumber * BaseNumber ^-> BaseNumber

            //arithmetic
            "abs" => WithTypes AllValues (fun t -> t ^-> t)

            "add" => WithTypes AllValues (fun t -> (t * t *+ t) ^-> t)

            "cbrt" => WithTypes AllValues (fun t -> t * !? T<bool> ^-> t)

            "ceil" => WithTypes AllValues (fun t -> t ^-> t)

            "cube" => WithTypes AllValues (fun t -> t ^-> t)

            "divide" => WithTypes AllValues (fun t -> t * t ^-> t)

            "dotDivide" => WithTypes AllValues (fun t -> t * t ^-> t)

            "dotMultiply" => WithTypes AllValues (fun t -> t * t ^-> t)

            "dotPow" => WithTypes AllValues (fun t -> t * t ^-> t)

            "exp" => WithTypes AllValues (fun t -> t ^-> t)

            "expm1" => WithTypes AllValues (fun t -> t ^-> t)

            "fix" => WithTypes AllValues (fun t -> t ^-> t)

            "floor" => WithTypes AllValues (fun t -> t ^-> t)

            "gcd" => WithTypes AllValues (fun t -> t * t ^-> t)

            "hypot" => WithTypes AllValues (fun t -> t *+ t ^-> t)

            "lcm" => WithTypes AllValues (fun t -> t *+ t ^-> t)

            "log" => WithTypes AllValues (fun t -> t * !? t ^-> t)

            "log10" => WithTypes AllValues (fun t -> t ^-> t)

            "log1p" => WithTypes AllValues (fun t -> t ^-> t)

            "log2" => WithTypes AllValues (fun t -> t ^-> t)

            "mod" => WithTypes AllValues (fun t -> t * t ^-> t)

            "multiply" => (WithTypes Numbers (fun t -> (t * t *+ t) ^-> t)) + (Vector * Vector *+ Vector ^-> T<float>) + (Matrix * Vector *+ Vector ^-> Vector) + (Matrix * Matrix *+ Matrix ^-> Matrix)

            "norm" =>  WithTypes AllValues (fun t -> t * !? t ^-> t)

            "nthRoot" =>  WithTypes AllValues (fun t -> t * !? t ^-> t)

            "nthRoots" => WithTypes AllValues (fun t -> t * !? t ^-> !| t)

            "pow" =>  WithTypes AllValues (fun t -> t * t ^-> t)

            "round" => WithTypes AllValues (fun t -> t * t ^-> t)

            "sign" => WithTypes AllValues (fun t -> t ^-> t)

            "sqrt" => WithTypes AllValues (fun t -> t ^-> t)

            "square" => WithTypes AllValues (fun t -> t ^-> t)

            "subtract" => WithTypes AllValues (fun t -> t * t ^-> t)

            "unaryMinus" => WithTypes AllValues (fun t -> t ^-> t)

            "unaryPlus" => WithTypes AllValues (fun t -> t ^-> t)

            "xgcd" => WithTypes AllValues (fun t -> t * t ^-> !| t)

            //bitwise
            "bitAnd" => WithTypes AllValues (fun t -> t * t ^-> t)

            "bitNot" => WithTypes AllValues (fun t -> t ^-> t)

            "bitOr" => WithTypes AllValues (fun t -> t * t ^-> t)

            "bitXor" => WithTypes AllValues (fun t -> t * t ^-> t)

            "leftShift" => WithTypes AllValues (fun t -> t * t ^-> t)

            "rightArithShift" => WithTypes AllValues (fun t -> t * t ^-> t)

            "rightLogShift" => WithTypes AllValues (fun t -> t * t ^-> t)

            //combinatorics
            "bellNumbers" => WithTypes AllValues (fun t -> t ^-> t)

            "catalan" => WithTypes AllValues (fun t -> t ^-> t)

            "composition" => WithTypes AllValues (fun t -> t * t ^-> t)

            "stirlingS2" => WithTypes AllValues (fun t -> t * t ^-> t)

            //complex
            "arg" => WithTypes AllValues (fun t -> t ^-> t)

            "conj" => WithTypes AllValues (fun t -> t ^-> t)

            "im" => WithTypes AllValues (fun t -> t ^-> t)

            "re" => WithTypes AllValues (fun t -> t ^-> t)

            //geometry
            "distance" => WithTypes Matrices (fun t -> t * t ^-> T<bigint>)

            "intersect" => WithTypes Matrices (fun t -> t * t * t * t ^-> Vector)

            //logic
            "and" => WithTypes AllValues (fun t -> t * t ^-> t)

            "not" => WithTypes AllValues (fun t -> t ^-> t)

            "or" => WithTypes AllValues (fun t -> t * t ^-> t)

            "xor" => WithTypes AllValues (fun t -> t * t ^-> t)

            //matrix
            "apply" => (BaseNumber) * T<int> * T<JavaScript.Function> ^-> (BaseNumber)

            "column" => (BaseNumber) * T<int> ^-> (BaseNumber)

            "count" => (BaseNumber + T<string>) ^-> T<int>

            "concat" => WithTypes Matrices (fun t -> !| t ^-> t)

            "cross" => WithTypes Matrices (fun t -> t * t ^-> t)

            "ctranspose" => (BaseNumber) ^-> (BaseNumber)

            "det" => WithTypes Matrices (fun t -> t ^-> T<float>)

            "diag" => WithTypes Matrices (fun t -> t * !?  (WithTypes Numbers (fun f -> f)) * !? T<string> ^-> t)

            "diff" => (BaseNumber) * !? T<int> ^-> (BaseNumber)

            "dot" => WithTypes Matrices (fun t -> t * t ^-> T<float>)            

            "eigs" => (BaseNumber) * BaseNumber ^-> T<obj>

            "expm" => Matrix ^-> Matrix

            "filter" => WithTypes Matrices (fun t -> t * (T<JavaScript.Function> + T<string>) ^-> t)

            "flatten" => WithTypes Matrices (fun t -> t ^-> t)

            "forEach" => WithTypes Matrices (fun t -> t * T<JavaScript.Function> ^-> T<unit>)

            "getMatrixDataType" => (!| Matrix + Vector) ^-> T<string>

            "identity" => WithTypes Matrices (fun t -> T<int> * T<int> ^-> (t + T<int>))

            "inv" => WithTypes AllValues (fun t -> t)

            "kron" => WithTypes Matrices (fun t -> t * t ^-> t)

            "map" => WithTypes Matrices (fun t -> t * T<JavaScript.Function> ^-> t)

            "matrixFromColumns" => !+ BaseNumber ^-> BaseNumber

            "matrixFromFunction" => (BaseNumber) * ((T<JavaScript.Function> * !? T<string> * !? T<string>) + (T<string> * !? T<string> * T<JavaScript.Function>)) ^-> (BaseNumber)            

            "matrixFromRows" => !+ BaseNumber ^-> BaseNumber

            "ones" => WithTypes Matrices (fun t -> !+ T<int> ^-> t + T<int>)

            "partitionSelect" => WithTypes Matrices (fun t -> t * T<int> * (T<string> + T<JavaScript.Function>) ^-> T<float>)

            "range" => WithTypes Matrices (fun t -> T<string> * !? (T<int> + T<bigint>) * !? (T<int> + T<bigint>) * !? (T<int> + T<bigint>) * !? T<bool> ^-> t)

            "reshape" => WithTypes Matrices (fun t -> t * !| T<int> ^-> t)

            "resize" => WithTypes Matrices (fun t -> t * t * !? (T<int> + T<string>) ^-> t)

            "rotate" => (BaseNumber) * BaseNumber * !? (BaseNumber) ^-> (BaseNumber)

            "rotationMatrix" => BaseNumber * !? (BaseNumber) * !? T<string> ^-> (BaseNumber)

            "row" => (BaseNumber) * T<int> ^-> (BaseNumber)

            "size" => WithTypes Matrices (fun t -> WithTypes AllValues (fun f -> f) ^-> t)

            "sort" => WithTypes Matrices (fun t -> t * (T<string> + T<JavaScript.Function>) ^-> t)

            "sqrtm" => (BaseNumber) ^-> (BaseNumber)

            "squeeze" => WithTypes Matrices (fun t -> t ^-> Matrix)

            "subset" => WithTypes Matrices (fun t -> (t + T<string>) * T<int> * !? (t + T<int>) * !? Index ^-> (t + T<string>))

            "trace" => WithTypes Matrices (fun t -> t ^-> T<float>)

            "transpose" => WithTypes Matrices (fun t -> t ^-> t)

            "zeros" => WithTypes Matrices (fun t -> !+ T<int> ^-> t)

            //probability
            "combinations" => (T<int> + T<bigint>) * (T<int> + T<bigint>) ^-> (T<int> + T<bigint>)

            "combinationsWithRep" => (T<int> + T<bigint>) * (T<int> + T<bigint>) ^-> (T<int> + T<bigint>)

            "factorial" => WithTypes Numbers (fun t -> t ^-> t)

            "gamma" => WithTypes Matrices (fun t -> (t + T<int>) ^-> (t + T<int>))

            "kldivergence" => WithTypes Matrices (fun t -> t * t ^-> T<int>)

            "multinomial" => !+ (T<int> + T<bigint>) ^-> (T<int> + T<bigint>)

            "permutations" => (T<int> + T<bigint>) * !? (T<int> + T<bigint>) ^-> (T<int> + T<bigint>)

            "pickRandom" => Vector * !? T<int> * !? Vector ^-> Vector

            "random" => WithTypes Matrices (fun t -> !? t * !? T<int> * !? T<int> ^-> (T<int> + t))

            "randomInt" => WithTypes Matrices (fun t -> T<int> * !? T<int> ^-> (T<int> + t))

            "randomInt" => WithTypes Matrices (fun t -> t * !? T<int> * !? T<int> ^-> (T<int> + t))

            //relational
            "compare" => WithTypes AllValues (fun t -> t * t ^-> t)

            "compareNatural" => WithTypes AllValues (fun t -> t * t ^-> t)

            "compareText" => (T<string> + BaseNumber) ^-> T<int> + BaseNumber

            "deepEqual" => WithTypes AllValues (fun t -> t * t ^-> t)

            "equal" => WithTypes AllValues (fun t -> t * t ^-> t)

            "equalText" => (T<string> + BaseNumber) ^-> T<int> + BaseNumber

            "larger" => WithTypes AllValues (fun t -> t * t ^-> t)

            "largerEq" => WithTypes AllValues (fun t -> t * t ^-> t)

            "smaller" => WithTypes AllValues (fun t -> t * t ^-> t)

            "smallerEq" => WithTypes AllValues (fun t -> t * t ^-> t)

            "unequal" => WithTypes AllValues (fun t -> t * t ^-> t)

            //set functions
            "setCartesian" => (BaseNumber) * (BaseNumber) ^-> (BaseNumber)

            "setDifference" => (BaseNumber) * (BaseNumber) ^-> (BaseNumber)

            "setDistinct" => (BaseNumber) ^-> (BaseNumber)

            "setIntersect" => (BaseNumber) * (BaseNumber) ^-> (BaseNumber)

            "setIsSubset" => (BaseNumber) * (BaseNumber) ^-> T<bool>

            "setMultiplicity" => BaseNumber * (BaseNumber) ^-> T<int>

            "setPowerset" => (BaseNumber) ^-> !| (BaseNumber)

            "setSize" => (BaseNumber) ^-> T<int>

            "setSymDifference" => (BaseNumber) * (BaseNumber) ^-> (BaseNumber)

            "setUnion" => (BaseNumber) * (BaseNumber) ^-> (BaseNumber)

            //special
            "erf" => WithTypes Matrices (fun t -> (T<int> + t) ^-> (T<int> + t))
            
            //statistics
            "mad" => WithTypes Matrices (fun t -> t ^-> T<float>)

            "max" => WithTypes Matrices (fun t -> t ^-> T<float>)
            
            "mean" => WithTypes Matrices (fun t -> t ^-> T<float>)

            "median" => WithTypes Matrices (fun t -> t ^-> T<float>)

            "min" => WithTypes Matrices (fun t -> t ^-> T<float>)

            "mode" => WithTypes Matrices (fun t -> t ^-> T<float>)

            "prod" => WithTypes Matrices (fun t -> t ^-> T<float>)

            "quantileSeq" => WithTypes Matrices (fun t -> t *+ !? (T<int> + T<bigint> + Vector) * !? T<bool> ^-> T<float>)

            "std" => WithTypes Matrices (fun t -> t * !? T<string> ^-> T<float>)
            
            "sum" => WithTypes Matrices (fun t -> t ^-> T<float>)

            "variance" => WithTypes Matrices (fun t -> t ^-> T<float>) 

            //string
            "bin" => T<string> * !? T<int> ^-> T<string>

            "format" => WithTypes AllValues (fun t -> t * !? T<int> * !? T<JavaScript.Function> ^-> T<string>)

            "hex" => T<string> * !? T<int> ^-> T<string>

            "oct" => T<string> * !? T<int> ^-> T<string>

            "print" =>  WithTypes AllValues (fun t -> T<string> * !| t * !? T<int> ^-> T<string>)

            //trigonometry
            "acos" => WithTypes AllValues (fun t -> t ^-> t)
            
            "acosh" => WithTypes AllValues (fun t -> t ^-> t)
            
            "acot" => WithTypes AllValues (fun t -> t ^-> t)
            
            "acoth" => WithTypes AllValues (fun t -> t ^-> t)
            
            "acsc" => WithTypes AllValues (fun t -> t ^-> t)
            
            "acsch" => WithTypes AllValues (fun t -> t ^-> t)
            
            "asec" => WithTypes AllValues (fun t -> t ^-> t)
            
            "asech" => WithTypes AllValues (fun t -> t ^-> t)
            
            "asin" => WithTypes AllValues (fun t -> t ^-> t)
            
            "asinh" => WithTypes AllValues (fun t -> t ^-> t)
            
            "atan" => WithTypes AllValues (fun t -> t ^-> t)
            
            "atan2" => WithTypes AllValues (fun t -> t * t ^-> t)
            
            "atanh" => WithTypes AllValues (fun t -> t ^-> t)
            
            "cos" => WithTypes AllValues (fun t -> t ^-> t)
            
            "cosh" => WithTypes AllValues (fun t -> t ^-> t)
            
            "cot" => WithTypes AllValues (fun t -> t ^-> t)
            
            "coth" => WithTypes AllValues (fun t -> t ^-> t)
            
            "csc" => WithTypes AllValues (fun t -> t ^-> t)
            
            "csch" => WithTypes AllValues (fun t -> t ^-> t)
            
            "sec" => WithTypes AllValues (fun t -> t ^-> t)
            
            "sech" => WithTypes AllValues (fun t -> t ^-> t)
            
            "sin" => WithTypes AllValues (fun t -> t ^-> t)
            
            "sinh" => WithTypes AllValues (fun t -> t ^-> t)
            
            "tan" => WithTypes AllValues (fun t -> t ^-> t)
            
            "tanh" => WithTypes AllValues (fun t -> t ^-> t)

            //unit
            "to" => (Unit.Type + BaseNumber) * (Unit.Type + BaseNumber) ^-> (Unit.Type + BaseNumber)

            //utils
            "clone" => WithTypes AllValues (fun t -> t ^-> t)

            "hasNumericValue" => T<obj> ^-> T<bool>

            "isInteger" => WithTypes AllValues (fun t -> t ^-> T<bool>)
            
            "isNaN" => WithTypes AllValues (fun t -> t ^-> T<bool>)
            
            "isNegative" => WithTypes AllValues (fun t -> t ^-> T<bool>)
            
            "isNumeric" => WithTypes AllValues (fun t -> t ^-> T<bool>)
            
            "isPositive" => WithTypes AllValues (fun t -> t ^-> T<bool>)
            
            "isPrime" => WithTypes AllValues (fun t -> t ^-> T<bool>)
            
            "isZero" => WithTypes AllValues (fun t -> t ^-> T<bool>)

            "numeric" => T<string> + BaseNumber * !? T<string> ^-> BaseNumber
            
            "typeOf" => T<obj> ^-> T<string>
        ]

    let mathProps: CodeModel.IClassMember list =
        [
            "E" =? T<float>

            "i" =? T<Complex>

            "Infinity" =? T<float>
            
            "LN2" =? T<float>
            
            "LN10" =? T<float>
            
            "LOG2E" =? T<float>
            
            "LOG10E" =? T<float>

            "NaN" =? NaNValue

            "null" =? NullValue
            
            "phi" =? T<float>
            
            "pi" =? T<float>

            "PI" =? T<float>
            
            "SQRT1_2" =? T<float>

            "SQRT2" =? T<float>

            "tau" =? T<float>

            "undefined" =? T<obj>

            "version" =? T<string>   
        ]

    let MathInstance =
        Class "MathInstance"
        |+> Static [
            "create" => !? Config.Type ^-> TSelf
            |> WithComment "Create and configure a math engine."
        ]
        |+> Instance (List.concat [mathOps; mathProps])

    let MathClass =
        Class "math"
        |+> Static (
            List.concat [
                [
                    "create" => !? Config.Type ^-> MathInstance.Type
                    |> WithComment "Create and configure a math engine."
                ]
                mathOps
                mathProps
            ]
        )

    Chain
        |+> Instance [
            //algebra
            "derivative" => (SymbolNode + T<string>) * !? DerivativeOption ^-> Chain.Type

            "lsolve" => BaseNumber ^-> Chain.Type

            "lsolveAll" => BaseNumber ^-> Chain.Type

            "lup" => T<unit> ^-> Chain.Type

            "lusolve" => BaseNumber ^-> Chain.Type

            "qr" => T<unit> ^-> Chain.Type

            "rationalize" => !? (T<bool> + T<obj>) * !? T<bool> ^-> Chain.Type

            "simplify" => !? (T<string * string> + T<string> + T<JavaScript.Function>) ^-> Chain.Type

            "slu" => T<float> * T<float> ^-> Chain.Type

            "usolve" => BaseNumber ^-> Chain.Type

            "usolveAll" => BaseNumber ^-> Chain.Type

            //arithmetic
            "abs" => T<unit> ^-> Chain.Type

            "add" => WithTypes AllValues (fun t -> (t *+ t) ^-> Chain.Type)

            "cbrt" => !? T<bool> ^-> Chain.Type

            "ceil" => T<unit> ^-> Chain.Type

            "cube" => T<unit> ^-> Chain.Type

            "divide" => WithTypes AllValues (fun t -> t ^-> Chain.Type)

            "dotDivide" => WithTypes AllValues (fun t -> t ^-> Chain.Type)

            "dotMultiply" => WithTypes AllValues (fun t -> t ^-> Chain.Type)

            "dotPow" => WithTypes AllValues (fun t -> t ^-> Chain.Type)

            "exp" => T<unit> ^-> Chain.Type

            "expm1" => T<unit> ^-> Chain.Type

            "fix" => T<unit> ^-> Chain.Type

            "floor" => T<unit> ^-> Chain.Type

            "gcd" => WithTypes AllValues (fun t -> t ^-> Chain.Type)

            "hypot" => WithTypes AllValues (fun t -> !+ t ^-> Chain.Type)

            "lcm" => WithTypes AllValues (fun t -> !+ t ^-> Chain.Type)

            "log" => WithTypes AllValues (fun t -> !? t ^-> Chain.Type)

            "log10" => T<unit> ^-> Chain.Type

            "log1p" => T<unit> ^-> Chain.Type

            "log2" => T<unit> ^-> Chain.Type

            "mod" => WithTypes AllValues (fun t -> t ^-> Chain.Type)

            "multiply" => WithTypes AllValues (fun t -> t *+ t ^-> Chain.Type)

            "norm" =>  WithTypes AllValues (fun t -> !? t ^-> Chain.Type)

            "nthRoot" =>  WithTypes AllValues (fun t -> !? t ^-> Chain.Type)

            "nthRoots" =>  WithTypes AllValues (fun t -> !? t ^-> Chain.Type)

            "pow" =>  WithTypes AllValues (fun t -> t ^-> Chain.Type)

            "round" => WithTypes AllValues (fun t -> t ^-> Chain.Type)

            "sign" => T<unit> ^-> Chain.Type

            "sqrt" => T<unit> ^-> Chain.Type

            "square" => T<unit> ^-> Chain.Type

            "subtract" => WithTypes AllValues (fun t -> t ^-> Chain.Type)

            "unaryMinus" => T<unit> ^-> Chain.Type

            "unaryPlus" => T<unit> ^-> Chain.Type

            "xgcd" => WithTypes AllValues (fun t -> t ^-> Chain.Type)

            //bitwise
            "bitAnd" => WithTypes AllValues (fun t -> t ^-> Chain.Type)

            "bitNot" => T<unit> ^-> Chain.Type

            "bitOr" => WithTypes AllValues (fun t -> t ^-> Chain.Type)

            "bitXor" => WithTypes AllValues (fun t -> t ^-> Chain.Type)

            "leftShift" => WithTypes AllValues (fun t -> t ^-> Chain.Type)

            "rightArithShift" => WithTypes AllValues (fun t -> t ^-> Chain.Type)

            "rightLogShift" => WithTypes AllValues (fun t -> t ^-> Chain.Type)

            //combinatorics
            "bellNumbers" => T<unit> ^-> Chain.Type

            "catalan" => T<unit> ^-> Chain.Type

            "composition" => WithTypes AllValues (fun t -> t ^-> Chain.Type)

            "stirlingS2" => WithTypes AllValues (fun t -> t ^-> Chain.Type)

            //complex
            "arg" => T<unit> ^-> Chain.Type

            "conj" => T<unit> ^-> Chain.Type

            "im" => T<unit> ^-> Chain.Type

            "re" => T<unit> ^-> Chain.Type

            //geometry
            "distance" => WithTypes Matrices (fun t -> t ^-> Chain.Type)

            "intersect" => WithTypes Matrices (fun t -> t * t * t ^-> Chain.Type)

            //logic
            "and" => WithTypes AllValues (fun t -> t ^-> Chain.Type)

            "not" => T<unit> ^-> Chain.Type

            "or" => WithTypes AllValues (fun t -> t ^-> Chain.Type)

            "xor" => WithTypes AllValues (fun t -> t ^-> Chain.Type)

            //matrix
            "apply" => T<int> * T<JavaScript.Function> ^-> Chain.Type

            "column" => T<int> ^-> Chain.Type

            "count" => T<unit> ^-> Chain.Type

            "concat" => WithTypes Matrices (fun t -> !+ t ^-> Chain.Type)

            "cross" => WithTypes Matrices (fun t -> t ^-> Chain.Type)

            "ctranspose" => T<unit> ^-> Chain.Type

            "det" => T<unit> ^-> Chain.Type

            "diag" => WithTypes Numbers (fun t -> !? t * !? T<string> ^-> Chain.Type)

            "diff" => !? T<int> ^-> Chain.Type

            "dot" => WithTypes Matrices (fun t -> t ^-> Chain.Type)

            "eigs" => BaseNumber ^-> Chain.Type

            "expm" => T<unit> ^-> Chain.Type

            "filter" => T<JavaScript.Function> + T<string> ^-> Chain.Type

            "flatten" => T<unit> ^-> Chain.Type

            "forEach" => T<JavaScript.Function> ^-> Chain.Type

            "getMatrixDataType" => T<unit> ^-> Chain.Type

            "identity" => T<int> ^-> Chain.Type

            "inv" => T<unit> ^-> Chain.Type

            "kron" => WithTypes Matrices (fun t -> t ^-> Chain.Type)

            "map" => T<JavaScript.Function> ^-> Chain.Type

            "matrixFromColumns" => T<unit> ^-> Chain.Type

            "matrixFromFunction" => ((T<JavaScript.Function> * !? T<string> * !? T<string>) + (T<string> * !? T<string> * T<JavaScript.Function>)) ^-> Chain.Type

            "matrixFromRows" => T<unit> ^-> Chain.Type

            "ones" => !+ T<int> ^-> Chain.Type

            "partitionSelect" => T<int> * (T<string> + T<JavaScript.Function>) ^-> Chain.Type

            "range" => T<string> * !? (T<int> + T<bigint>) * !? (T<int> + T<bigint>) * !? (T<int> + T<bigint>) * !? T<bool> ^-> Chain.Type

            "reshape" => !| T<int> ^-> Chain.Type

            "resize" => WithTypes Matrices (fun t -> t * !? (T<int> + T<string>) ^-> Chain.Type)

            "rotate" => BaseNumber * !? (BaseNumber) ^-> Chain.Type

            "rotationMatrix" => !? (BaseNumber) * !? T<string> ^-> Chain.Type

            "row" => T<int> ^-> Chain.Type

            "size" => WithTypes AllValues (fun f -> f ^-> Chain.Type)

            "sort" => T<string> + T<JavaScript.Function> ^-> Chain.Type

            "sqrtm" => T<unit> ^-> Chain.Type

            "squeeze" => T<unit> ^-> Chain.Type

            "subset" => WithTypes Matrices (fun t -> (t + T<string>) * T<int> * !? (t + T<int>) * !? Index ^-> Chain.Type)

            "trace" => T<unit> ^-> Chain.Type

            "transpose" => T<unit> ^-> Chain.Type

            "zeros" => !+ T<int> ^-> Chain.Type

            //probability
            "combinations" => (T<int> + T<bigint>) ^-> Chain.Type

            "combinationsWithRep" => (T<int> + T<bigint>) ^-> Chain.Type

            "factorial" => T<unit> ^-> Chain.Type

            "gamma" => !? T<int> ^-> Chain.Type

            "kldivergence" => WithTypes Matrices (fun t -> t ^-> Chain.Type)

            "multinomial" => !+ (T<int> + T<bigint>) ^-> Chain.Type

            "permutations" => (T<int> + T<bigint>) ^-> Chain.Type

            "pickRandom" => !? T<int> * !? Vector ^-> Chain.Type

            "random" => !? T<int> * !? T<int> ^-> Chain.Type

            "randomInt" => !? T<int> * !? T<int> ^-> Chain.Type

            //relational
            "compare" => WithTypes AllValues (fun t -> t ^-> Chain.Type)

            "compareNatural" => WithTypes AllValues (fun t -> t ^-> Chain.Type)

            "compareText" => T<unit> ^-> Chain.Type

            "deepEqual" => WithTypes AllValues (fun t -> t ^-> Chain.Type)

            "equal" => WithTypes AllValues (fun t -> t ^-> Chain.Type)

            "equalText" => T<unit> ^-> Chain.Type

            "larger" => WithTypes AllValues (fun t -> t ^-> Chain.Type)

            "largerEq" => WithTypes AllValues (fun t -> t ^-> Chain.Type)

            "smaller" => WithTypes AllValues (fun t -> t ^-> Chain.Type)

            "smallerEq" => WithTypes AllValues (fun t -> t ^-> Chain.Type)

            "unequal" => WithTypes AllValues (fun t -> t ^-> Chain.Type)

            //set functions
            "setCartesian" => (BaseNumber) ^-> Chain.Type

            "setDifference" => (BaseNumber) ^-> Chain.Type

            "setDistinct" => T<unit> ^-> Chain.Type

            "setIntersect" => (BaseNumber) ^-> Chain.Type

            "setIsSubset" => (BaseNumber) ^-> Chain.Type

            "setMultiplicity" => (BaseNumber) ^-> Chain.Type

            "setPowerset" => T<unit> ^-> Chain.Type

            "setSize" => T<unit> ^-> Chain.Type

            "setSymDifference" => (BaseNumber) ^-> Chain.Type

            "setUnion" => (BaseNumber) ^-> Chain.Type

            //special
            "erf" => T<unit> ^-> Chain.Type
            
            //statistics
            "mad" => T<unit> ^-> Chain.Type

            "max" => T<unit> ^-> Chain.Type
            
            "mean" => T<unit> ^-> Chain.Type

            "median" => T<unit> ^-> Chain.Type

            "min" => T<unit> ^-> Chain.Type

            "mode" => T<unit> ^-> Chain.Type

            "prod" => T<unit> ^-> Chain.Type

            "quantileSeq" => !+ (T<int> + T<bigint> + Vector) * !? T<bool> ^-> Chain.Type

            "std" => !? T<string> ^-> Chain.Type
            
            "sum" => T<unit> ^-> Chain.Type

            "variance" => T<unit> ^-> Chain.Type 

            //string
            "bin" => !? T<int> ^-> Chain.Type

            "format" => !? T<int> * !? T<JavaScript.Function> ^-> Chain.Type

            "hex" => !? T<int> ^-> Chain.Type

            "oct" => !? T<int> ^-> Chain.Type

            "print" =>  WithTypes AllValues (fun t -> T<string> * !| t * !? T<int> ^-> Chain.Type)

            //trigonometry
            "acos" => T<unit> ^-> Chain.Type
            
            "acosh" => T<unit> ^-> Chain.Type
            
            "acot" => T<unit> ^-> Chain.Type
            
            "acoth" => T<unit> ^-> Chain.Type
            
            "acsc" => T<unit> ^-> Chain.Type
            
            "acsch" => T<unit> ^-> Chain.Type
            
            "asec" => T<unit> ^-> Chain.Type
            
            "asech" => T<unit> ^-> Chain.Type
            
            "asin" => T<unit> ^-> Chain.Type
            
            "asinh" => T<unit> ^-> Chain.Type
            
            "atan" => T<unit> ^-> Chain.Type
            
            "atan2" => WithTypes AllValues (fun t -> t ^-> Chain.Type)
            
            "atanh" => T<unit> ^-> Chain.Type
            
            "cos" => T<unit> ^-> Chain.Type
            
            "cosh" => T<unit> ^-> Chain.Type
            
            "cot" => T<unit> ^-> Chain.Type
            
            "coth" => T<unit> ^-> Chain.Type
            
            "csc" => T<unit> ^-> Chain.Type
            
            "csch" => T<unit> ^-> Chain.Type
            
            "sec" => T<unit> ^-> Chain.Type
            
            "sech" => T<unit> ^-> Chain.Type
            
            "sin" => T<unit> ^-> Chain.Type
            
            "sinh" => T<unit> ^-> Chain.Type
            
            "tan" => T<unit> ^-> Chain.Type
            
            "tanh" => T<unit> ^-> Chain.Type

            //unit
            "to" => (Unit.Type + BaseNumber) ^-> Chain.Type

            //utils
            "clone" => T<unit> ^-> Chain.Type

            "hasNumericValue" => T<unit> ^-> Chain.Type

            "isInteger" => T<unit> ^-> Chain.Type
            
            "isNaN" => T<unit> ^-> Chain.Type
            
            "isNegative" => T<unit> ^-> Chain.Type
            
            "isNumeric" => T<unit> ^-> Chain.Type
            
            "isPositive" => T<unit> ^-> Chain.Type
            
            "isPrime" => T<unit> ^-> Chain.Type
            
            "isZero" => T<unit> ^-> Chain.Type

            "numeric" => !? T<string> ^-> Chain.Type

            //chain
            "done" => T<unit> ^-> Chain.Type

            "valueOf" => WithTypes AllValues (fun t -> T<unit> ^-> t)

            "toString" => T<unit> ^-> T<string>
        ]
        |> ignore

    let Assembly =
        Assembly [
            Namespace "WebSharper.MathJS.Resources" [
                (Resource "Js" "https://cdnjs.cloudflare.com/ajax/libs/mathjs/9.4.4/math.js")
                |> AssemblyWide
            ]
            Namespace "WebSharper.MathJS" [
                MathClass
                MathInstance
                Index
                Chain
                UnitJSON
                Unit
                BaseNumber
                DerivativeOption
                ParenthesisOptions
                ImplicitOptions
                OutputOptions
                Node
                AccessorNode          
                ArrayNode             
                AssignmentNode        
                BlockNode             
                ConditionalNode       
                ConstantNode          
                FunctionAssignmentNode
                FunctionNode          
                IndexNode             
                ObjectNode            
                OperatorNode          
                ParenthesisNode       
                RangeNode
                RelationalNode
                SymbolNode
                Config
                Options
                Parser
                NullValue
                NaNValue
            ]
        ]


[<Sealed>]
type Extension() =
    interface IExtension with
        member x.Assembly = Definition.Assembly

[<assembly: Extension(typeof<Extension>)>]
do ()
