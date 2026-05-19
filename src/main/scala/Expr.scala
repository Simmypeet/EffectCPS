package expr

import lowerExpr.LowerExpr

import scala.Predef.{String as ScalaString}

enum Value {
    case Num(n: Int)
    case String(s: ScalaString)
    case Lambda(params: ScalaString, body: Expr)
    case Array(elements: List[Value])
    case Add(v1: Value, v2: Value)
    case Index(array: Value, index: Value)
}

enum Expr {
    case App(func: Value, arg: Value)
    case Let(name: Option[ScalaString], expr: Expr, body: Expr)
    case Return(value: Value)
    case Do(label: ScalaString, arg: Value)
    case Handle(expr: Expr, handler: Handler)
}

/// <returnClause> ::= 'return' <ident> '->' <expr>
case class ReturnClause(param: ScalaString, body: Expr)

/// <operationClause> ::= 'case' <ident> '(' <ident>, <ident> ')' '->' <expr>
case class OperationClause(
    label: ScalaString,
    param: ScalaString,
    resumption: ScalaString,
    body: Expr
)

/// <handler> ::= '{' <returnClause> <operationClause>* '}'
case class Handler(
    returnClause: ReturnClause,
    operationClause: List[OperationClause]
)
