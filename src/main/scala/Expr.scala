package expr

enum Expr {
    case Num(n: Int)
    case String(s: String)
    case Add(e1: Expr, e2: Expr)
    case Lambda(params: List[String], body: Expr)
    case Var(name: String)
    case App(func: Expr, args: List[Expr])
    case Let(name: String, value: Expr, body: Expr)
    case Array(elements: List[Expr])
    case Do(label: String, body: Expr, handler: Handler)
}

case class ReturnClause(param: String, body: Expr)
case class OperationClause(
    label: String,
    param: String,
    resumption: String,
    body: Expr
)

case class Handler(
    returnClause: ReturnClause,
    operationClause: List[OperationClause]
)
