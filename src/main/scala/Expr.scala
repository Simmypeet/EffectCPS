package expr

enum Expr {
    case Num(n: Int)
    case String(s: String)

    /// <add> ::= <expr> '+' <expr>
    case Add(e1: Expr, e2: Expr)

    /// <lambda> ::= '\' <ident>+ '.' <expr>
    case Lambda(params: List[String], body: Expr)

    /// <parens> ::= '(' <expr> ')'
    case Parens(expr: Expr)

    /// <var> ::= <ident>
    case Var(name: String)

    /// <app> ::= <expr> '(' <expr>+ ')'
    case App(func: Expr, args: List[Expr])

    /// <let> ::= ('let' <ident> '=')? <expr> ';' <expr>
    case Let(name: Option[String], value: Expr, body: Expr)

    /// <tuple> ::= '[' <expr> (',' <expr>)* ']'
    case Tuple(elements: List[Expr])

    /// <index> ::= <expr> '[' <expr> ']'
    case Index(array: Expr, index: Expr)

    /// <handle> ::= 'handle' <expr> 'with' <handler>
    case Handle(body: Expr, handler: Handler)

    /// <do> ::= 'do' <ident> <expr>
    case Do(label: String, arg: Expr)
}

/// <returnClause> ::= 'return' <ident> '->' <expr>
case class ReturnClause(param: String, body: Expr)

/// <operationClause> ::= 'case' <ident> '(' <ident>, <ident> ')' '->' <expr>
case class OperationClause(
    label: String,
    param: String,
    resumption: String,
    body: Expr
)

/// <handler> ::= '{' <returnClause> <operationClause>* '}'
case class Handler(
    returnClause: ReturnClause,
    operationClause: List[OperationClause]
)
