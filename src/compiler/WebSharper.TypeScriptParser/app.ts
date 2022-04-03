import * as fs from "fs"
import * as ts from "typescript"

// main: parse and transform file

let filePath = process.argv[2]
if (filePath == null)
  throw "Please provide the path to a TypeScript definition file"

let program = ts.createProgram([filePath], { target: ts.ScriptTarget.Latest });
let checker = program.getTypeChecker();

let output = program.getSourceFiles().map(transformFile)
let outputPath = process.argv[3] ?? (filePath + ".json")

fs.writeFileSync(outputPath, JSON.stringify(output, undefined, 2))

process.exit(0)

// output shape

interface TSParameter {
  Name: string
  Type: TSType
}
interface TSSimpleType {
  Kind: 'simple'
  Type: string
}
interface TSArrayType {
  Kind: 'array'
  ElementType: TSType
}
interface TSTupleType {
  Kind: 'tuple'
  ElementTypes: TSType[]
}
interface TSFunctionOrNewType {
  Kind: 'function' | 'new'
  Parameters: TSParameter[]
  ReturnType: TSType
}
interface TSUnionOrIntersectionType {
  Kind: 'union' | 'intersection'
  Types: TSType[]
}
interface TSConditionalType {
  Kind: 'conditional'
  CheckType: TSType
  ExtendsType: TSType
  TrueType: TSType
  FalseType: TSType
}
interface TSTypeLiteral {
  Kind: 'object'
  Members: TSTypeElement[]
}
interface TSTypeReference {
  Kind: 'typeref' | 'typeparamref'
  Type: string
  Arguments?: TSType[]
}
interface TSTypePredicate {
  Kind: 'predicate'
  Type: TSType
  Parameter: string
}
interface TSIndexType {
  Kind: 'index'
  index: TSType
  Type: TSType
}
interface TSKeyOfOrMappedType {
  Kind: 'keyof' | 'mapped'
  Type: TSType
}
type TSType =
  | TSSimpleType
  | TSArrayType
  | TSTupleType
  | TSFunctionOrNewType
  | TSUnionOrIntersectionType
  | TSConditionalType
  | TSTypeLiteral
  | TSTypeReference
  | TSTypePredicate
  | TSIndexType
  | TSKeyOfOrMappedType
interface TSTypeParameter {
  Name: string
  Constraint?: TSType
}
interface TSTypeElement {
  Kind: 'method' | 'property' | 'new' | 'call' | 'index'
  Name?: string
  Parameters?: TSParameter[]
  TypeParameters?: TSTypeParameter[]
  Type: TSType
}
interface TSVariableStatement {
  Kind: 'vars'
  declarations: TSParameter[]
}
interface TSTypeAlias {
  Kind: 'typealias'
  Name: string
  TypeParameters?: TSTypeParameter[]
  Type: TSType
}
interface TSTypeDeclaration {
  Kind: 'class' | 'interface'
  Name: string
  TypeParameters?: TSTypeParameter[]
  Members: TSTypeElement[]
  Extends?: TSType | TSType[]
  Implements?: TSType[]
}
interface TSModuleDeclaration {
  Kind: 'module'
  Name: string
  Members: TSStatement[]
}
interface TSExportAssignment {
  Kind: 'exportassignment'
  Expression: string
}
interface TSExportDeclaration {
  Kind: 'exportdeclaration'
  Expression: string
  Name?: string
}
type TSStatement =
  | TSVariableStatement
  | TSFunctionOrNewType
  | TSTypeAlias
  | TSTypeDeclaration
  | TSModuleDeclaration
  | TSExportAssignment
  | TSExportDeclaration

interface TSFile {
    Name: string
    Statements: TSStatement[]
}

// transformer functions

function transformParameter(p: ts.ParameterDeclaration): TSParameter {
  return {
    Name: p.name.getText(),
    Type: transformType(p.type)
  }
}

function simpleType(x: string): TSSimpleType {
  return {
    Kind: 'simple',
    Type: x
  }
}

function unhandled(x: ts.Node, ctx: string) {
  throw Error(`Unhandled SyntaxKind for ${ctx}: ${x.kind} '${x.getText()}' in ${x.getSourceFile().fileName}`);
}

function transformType(x: ts.TypeNode): TSType {
  if (!x) return simpleType('any');
  if (ts.isParenthesizedTypeNode(x))
    return transformType(x.type)
  if (ts.isArrayTypeNode(x))
    return {
      Kind: 'array',
      ElementType: transformType(x.elementType)
    }
  if (ts.isTupleTypeNode(x))
    return {
      Kind: 'tuple',
      ElementTypes: x.elementTypes.map(transformType)
    }
  if (ts.isFunctionTypeNode(x))
    return {
      Kind: 'function',
      Parameters: x.parameters.map(transformParameter),
      ReturnType: transformType(x.type)
    }
  if (ts.isConstructorTypeNode(x))
    return {
      Kind: 'new',
      Parameters: x.parameters.map(transformParameter),
      ReturnType: transformType(x.type)
    }
  if (ts.isUnionTypeNode(x))
    return {
      Kind: 'union',
      Types: x.types.map(transformType)
    }
  if (ts.isIntersectionTypeNode(x))
    return {
      Kind: 'intersection',
      Types: x.types.map(transformType)
    }
  if (ts.isConditionalTypeNode(x))
    return {
      Kind: 'conditional',
      CheckType: transformType(x.checkType),
      ExtendsType: transformType(x.extendsType),
      TrueType: transformType(x.trueType),
      FalseType: transformType(x.falseType)
    }
  if (ts.isTypeLiteralNode(x))
    return {
      Kind: 'object',
      Members: x.members.map(transformTypeElement)
    }
  if (ts.isIndexedAccessTypeNode(x))
    return {
      Kind: 'index',
      index: transformType(x.indexType),
      Type: transformType(x.objectType)
    }
  if (ts.isTypeReferenceNode(x))
    if (x.typeArguments)
      return {
        Kind: 'typeref',
        Type: x.typeName.getText(),
        Arguments: x.typeArguments.map(transformType)
      }
    else {
      let t: ts.Type = checker.getTypeAtLocation(x)
      if (t && t.flags & ts.TypeFlags.TypeParameter)
        return {
          Kind: 'typeparamref',
          Type: x.typeName.getText()
        }
      else
        return simpleType(x.typeName.getText())
    }
  if (ts.isTypePredicateNode(x))
    return {
      Kind: 'predicate',
      Type: transformType(x.type),
      Parameter: x.parameterName.getText()
    }
  if (ts.isTypeOperatorNode(x))
    return {
      Kind: 'keyof',
      Type: transformType(x.type)
    }
  if (ts.isMappedTypeNode(x))
    return {
      Kind: 'mapped',
      Type: transformType(x.type)
    }
  let res = x.getText()
  if (res.indexOf('<') > 0)
    unhandled(x, "type");
  return simpleType(x.getText())
}

function transformTypeElement(x: ts.TypeElement): TSTypeElement {
  if (ts.isMethodSignature(x))
    return {
      Kind: 'method',
      Name: x.name.getText(),
      Parameters: x.parameters.map(transformParameter),
      Type: transformType(x.type)
    }
  if (ts.isPropertySignature(x))
    return {
      Kind: 'property',
      Name: x.name.getText(),
      Type: transformType(x.type)
    }
  if (ts.isConstructSignatureDeclaration(x))
    return {
      Kind: 'new',
      Parameters: x.parameters.map(transformParameter),
      Type: transformType(x.type)
    }
  if (ts.isCallSignatureDeclaration(x))
    return {
      Kind: 'call',
      Parameters: x.parameters.map(transformParameter),
      Type: transformType(x.type)
    }
  if (ts.isIndexSignatureDeclaration(x))
    return {
      Kind: 'index',
      Parameters: x.parameters.map(transformParameter),
      Type: transformType(x.type)
    }
  unhandled(x, "type");
}

function transformTypeParameter(x: ts.TypeParameterDeclaration): TSTypeParameter {
  return {
    Name: x.name.getText(),
    Constraint: x.constraint && transformType(x.constraint)
  }
}

function transformClassElement(x: ts.ClassElement): TSTypeElement {
  if (ts.isMethodDeclaration(x))
    return {
      Kind: 'method',
      Name: x.name.getText(),
      Parameters: x.parameters.map(transformParameter),
      TypeParameters: x.typeParameters?.map(transformTypeParameter),
      Type: transformType(x.type)
    }
  if (ts.isConstructorDeclaration(x))
    return {
      Kind: 'new',
      Parameters: x.parameters.map(transformParameter),
      Type: transformType(x.type)
    }
  if (ts.isPropertyDeclaration(x))
    return {
      Kind: 'property',
      Name: x.name.getText(),
      Type: transformType(x.type)
    }
  unhandled(x, "type");
}

function transformExpessionWithTypeArguments(x: ts.ExpressionWithTypeArguments): TSType {
  if (x.typeArguments)
    return {
      Kind: 'typeref',
      Type: x.expression.getText(),
      Arguments: x.typeArguments.map(transformType)
    }
  else
    return simpleType(x.expression.getText())
}

function transformStatement(x: ts.Statement): TSStatement {
  if (ts.isVariableStatement(x))
    return {
      Kind: 'vars',
      declarations: x.declarationList.declarations.map(d => ({ Name: d.name.getText(), Type: transformType(d.type) }))
    }
  if (ts.isFunctionDeclaration(x))
    return {
      Kind: 'function',
      Parameters: x.parameters.map(transformParameter),
      ReturnType: transformType(x.type)
    }
  if (ts.isInterfaceDeclaration(x))
    return {
      Kind: 'interface',
      Name: x.name.text,
      Members: x.members.map(transformTypeElement),
      Extends: x.heritageClauses && x.heritageClauses[0].types.map(transformExpessionWithTypeArguments)
    }
  if (ts.isClassDeclaration(x)) {
    let ext: TSType[] =
      x.heritageClauses &&
      [].concat([],
        x.heritageClauses.filter(c => c.token == ts.SyntaxKind.ExtendsKeyword).map(c => c.types.map(transformExpessionWithTypeArguments))
      )
    let impl =
      x.heritageClauses &&
      [].concat([],
        x.heritageClauses.filter(c => c.token == ts.SyntaxKind.ImplementsKeyword).map(c => c.types.map(transformExpessionWithTypeArguments))
      )
    return {
      Kind: 'class',
      Name: x.name.text,
      Members: x.members.map(transformClassElement),
      Extends: ext && ext.length && ext,
      Implements: impl && impl.length && impl
    }
  }
  if (ts.isTypeAliasDeclaration(x))
    return {
      Kind: 'typealias',
      Name: x.name.text,
      Type: transformType(x.type)
    }
  if (ts.isModuleDeclaration(x) && ts.isModuleBlock(x.body))
    return {
      Kind: 'module',
      Name: x.name.text,
      Members: x.body.statements.map(transformStatement)
    }
  if (ts.isExportAssignment(x)) {
    return {
      Kind: 'exportassignment',
      Expression: x.expression.getText()
    }
  }
  if (ts.isExportDeclaration(x)) {
    return {
      Kind: 'exportdeclaration',
      Expression: x..getText()
    }
  }
  unhandled(x, "type");
}

function transformFile(x: ts.SourceFile): TSFile {
    return {
        Name: x.fileName,
        Statements: x.statements.map(transformStatement)
    }
}
