import * as fs from "fs"
import * as ts from "typescript"

// main: parse and transform file

let filePath = process.argv[2]
if (filePath == null)
  throw "Please provide the path to a TypeScript definition file"

let code = fs.readFileSync(filePath).toString()
let program = ts.createProgram([filePath], { target: ts.ScriptTarget.Latest });
let checker = program.getTypeChecker()
let sourceFile = program.getSourceFiles()[0]

let output = sourceFile.statements.map(transformStatement)
let outputPath = filePath + ".json"

fs.writeFileSync(outputPath, JSON.stringify(output, undefined, 2))

process.exit(0)

// output shape

interface OutputParameter {
  name: string
  type: OutputType
}
interface OutputSimpleType {
  kind: 'simple'
  type: string
}
interface OutputArrayType {
  kind: 'array'
  elementType: OutputType
}
interface OutputTupleType {
  kind: 'tuple'
  elementTypes: OutputType[]
}
interface OutputFunctionOrNewType {
  kind: 'function' | 'new'
  parameters: OutputParameter[]
  returnType: OutputType
}
interface OutputUnionOrIntersectionType {
  kind: 'union' | 'intersection'
  types: OutputType[]
}
interface OutputTypeLiteral {
  kind: 'object'
  members: OutputTypeElement[]
}
interface OutputTypeReference {
  kind: 'typeref' | 'typeparamref'
  type: string
  arguments?: OutputType[]
}
interface OutputTypePredicate {
  kind: 'predicate'
  type: OutputType
  parameter: string
}
interface OutputIndexType {
  kind: 'index'
  index: OutputType
  type: OutputType
}
interface OutputKeyOfOrMappedType {
  kind: 'keyof' | 'mapped'
  type: OutputType
}
type OutputType =
  | OutputSimpleType
  | OutputArrayType
  | OutputTupleType
  | OutputFunctionOrNewType
  | OutputUnionOrIntersectionType
  | OutputTypeLiteral
  | OutputTypeReference
  | OutputTypePredicate
  | OutputIndexType
  | OutputKeyOfOrMappedType
interface OutputTypeParameter {
  name: string
  constraint?: OutputType
}
interface OutputTypeElement {
  kind: 'method' | 'property' | 'new' | 'call' | 'index'
  name?: string
  parameters?: OutputParameter[]
  typeParameters?: OutputTypeParameter[]
  type: OutputType
}
interface OutputVariableStatement {
  kind: 'vars'
  declarations: OutputParameter[]
}
interface OutputTypeAlias {
  kind: 'typealias'
  name: string
  typeParameters?: OutputTypeParameter[]
  type: OutputType
}
interface OutputTypeDeclaration {
  kind: 'class' | 'interface'
  name: string
  typeParameters?: OutputTypeParameter[]
  members: OutputTypeElement[]
  extends?: OutputType | OutputType[]
  implements?: OutputType[]
}
interface OutputModuleDeclaration {
  kind: 'module'
  name: string
  members: OutputStatement[]
}
type OutputStatement =
  | OutputVariableStatement
  | OutputFunctionOrNewType
  | OutputTypeAlias
  | OutputTypeDeclaration
  | OutputModuleDeclaration

// transformer functions

function transformParameter(p: ts.ParameterDeclaration): OutputParameter {
  return {
    name: p.name.getText(),
    type: transformType(p.type)
  }
}

function simpleType(x: string): OutputSimpleType {
  return {
    kind: 'simple',
    type: x
  }
}

function transformType(x: ts.TypeNode): OutputType {
  if (!x) return simpleType('any');
  if (ts.isParenthesizedTypeNode(x))
    return transformType(x.type)
  if (ts.isArrayTypeNode(x))
    return {
      kind: 'array',
      elementType: transformType(x.elementType)
    }
  if (ts.isTupleTypeNode(x))
    return {
      kind: 'tuple',
      elementTypes: x.elementTypes.map(transformType)
    }
  if (ts.isFunctionTypeNode(x))
    return {
      kind: 'function',
      parameters: x.parameters.map(transformParameter),
      returnType: transformType(x.type)
    }
  if (ts.isConstructorTypeNode(x))
    return {
      kind: 'new',
      parameters: x.parameters.map(transformParameter),
      returnType: transformType(x.type)
    }
  if (ts.isUnionTypeNode(x))
    return {
      kind: 'union',
      types: x.types.map(transformType),
    }
  if (ts.isIntersectionTypeNode(x))
    return {
      kind: 'intersection',
      types: x.types.map(transformType),
    }
  if (ts.isTypeLiteralNode(x))
    return {
      kind: 'object',
      members: x.members.map(transformTypeElement)
    }
  if (ts.isIndexedAccessTypeNode(x))
    return {
      kind: 'index',
      index: transformType(x.indexType),
      type: transformType(x.objectType)
    }
  if (ts.isTypeReferenceNode(x))
    if (x.typeArguments)
      return {
        kind: 'typeref',
        type: x.typeName.getText(),
        arguments: x.typeArguments.map(transformType)
      }
    else {
      let t: ts.Type = checker.getTypeAtLocation(x)
      if (t && t.flags & ts.TypeFlags.TypeParameter)
        return {
          kind: 'typeparamref',
          type: x.typeName.getText(),
        }
      else
        return simpleType(x.typeName.getText())
    }
  if (ts.isTypePredicateNode(x))
    return {
      kind: 'predicate',
      type: transformType(x.type),
      parameter: x.parameterName.getText()
    }
  if (ts.isTypeOperatorNode(x))
    return {
      kind: 'keyof',
      type: transformType(x.type)
    }
  if (ts.isMappedTypeNode(x))
    return {
      kind: 'mapped',
      type: transformType(x.type)
    }
  let res = x.getText()
  if (res.indexOf('<') > 0) throw Error("needsBetterTypeParsing:" + x.kind)
  return simpleType(x.getText())
}

function transformTypeElement(x: ts.TypeElement): OutputTypeElement {
  if (ts.isMethodSignature(x))
    return {
      kind: 'method',
      name: x.name.getText(),
      parameters: x.parameters.map(transformParameter),
      type: transformType(x.type)
    }
  if (ts.isPropertySignature(x))
    return {
      kind: 'property',
      name: x.name.getText(),
      type: transformType(x.type)
    }
  if (ts.isConstructSignatureDeclaration(x))
    return {
      kind: 'new',
      parameters: x.parameters.map(transformParameter),
      type: transformType(x.type)
    }
  if (ts.isCallSignatureDeclaration(x))
    return {
      kind: 'call',
      parameters: x.parameters.map(transformParameter),
      type: transformType(x.type)
    }
  if (ts.isIndexSignatureDeclaration(x))
    return {
      kind: 'index',
      parameters: x.parameters.map(transformParameter),
      type: transformType(x.type)
    }
  throw { nonParsedTypeElementKind: x.kind }
}

function transformTypeParameter(x: ts.TypeParameterDeclaration): OutputTypeParameter {
  return {
    name: x.name.getText(),
    constraint: x.constraint && transformType(x.constraint)
  }
}

function transformClassElement(x: ts.ClassElement): OutputTypeElement {
  if (ts.isMethodDeclaration(x))
    return {
      kind: 'method',
      name: x.name.getText(),
      parameters: x.parameters.map(transformParameter),
      typeParameters: x.typeParameters.map(transformTypeParameter),
      type: transformType(x.type)
    }
  if (ts.isConstructorDeclaration(x))
    return {
      kind: 'new',
      parameters: x.parameters.map(transformParameter),
      type: transformType(x.type)
    }
  if (ts.isPropertyDeclaration(x))
    return {
      kind: 'property',
      name: x.name.getText(),
      type: transformType(x.type)
    }
  throw { nonParsedClassElementKind: x.kind }
}

function transformExpessionWithTypeArguments(x: ts.ExpressionWithTypeArguments): OutputType {
  if (x.typeArguments)
    return {
      kind: 'typeref',
      type: x.expression.getText(),
      arguments: x.typeArguments.map(transformType)
    }
  else
    return simpleType(x.expression.getText())
}

function transformStatement(x: ts.Statement): OutputStatement {
  if (ts.isVariableStatement(x))
    return {
      kind: 'vars',
      declarations: x.declarationList.declarations.map(d => ({ name: d.name.getText(), type: transformType(d.type) }))
    }
  if (ts.isFunctionDeclaration(x))
    return {
      kind: 'function',
      parameters: x.parameters.map(transformParameter),
      returnType: transformType(x.type)
    }
  if (ts.isInterfaceDeclaration(x))
    return {
      kind: 'interface',
      name: x.name.text,
      members: x.members.map(transformTypeElement),
      extends: x.heritageClauses && x.heritageClauses[0].types.map(transformExpessionWithTypeArguments)
    }
  if (ts.isClassDeclaration(x)) {
    let ext: OutputType[] =
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
      kind: 'class',
      name: x.name.text,
      members: x.members.map(transformClassElement),
      extends: ext && ext.length && ext,
      implements: impl && impl.length && impl
    }
  }
  if (ts.isTypeAliasDeclaration(x))
    return {
      kind: 'typealias',
      name: x.name.text,
      type: transformType(x.type)
    }
  if (ts.isModuleDeclaration(x) && ts.isModuleBlock(x.body))
    return {
      kind: 'module',
      name: x.name.text,
      members: x.body.statements.map(transformStatement)
    }
  throw { nonParsedStatementKind: x.kind }
}
