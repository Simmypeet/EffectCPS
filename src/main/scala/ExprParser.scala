package expr

import scala.Predef.{String as ScalaString}

object ExprParser {
  final case class ParseError(message: ScalaString)
      extends IllegalArgumentException(message)

  def parse(source: ScalaString): Expr =
    Parser(Tokenizer.tokenize(source)).parseProgram()

  private enum TokenKind {
    case Identifier
    case Number
    case String
    case LParen
    case RParen
    case LBracket
    case RBracket
    case LBrace
    case RBrace
    case Comma
    case Semicolon
    case Plus
    case PlusPlus
    case Equals
    case EqualsEquals
    case Arrow
    case FatArrow
    case Eof
  }

  private case class Token(
      kind: TokenKind,
      lexeme: ScalaString,
      line: Int,
      column: Int
  )

  private object Tokenizer {
    def tokenize(source: ScalaString): Vector[Token] = {
      val tokens = Vector.newBuilder[Token]
      var index = 0
      var line = 1
      var column = 1

      def currentChar: Char = source.charAt(index)

      def advance(): Char = {
        val char = source.charAt(index)
        index += 1

        if char == '\n' then
          line += 1
          column = 1
        else column += 1

        char
      }

      def add(
          kind: TokenKind,
          lexeme: ScalaString,
          tokenLine: Int,
          tokenColumn: Int
      ): Unit =
        tokens += Token(kind, lexeme, tokenLine, tokenColumn)

      def skipLineComment(): Unit =
        while index < source.length && currentChar != '\n' do advance()

      def readWhile(predicate: Char => Boolean): ScalaString = {
        val start = index
        while index < source.length && predicate(currentChar) do advance()
        source.substring(start, index)
      }

      def readString(tokenLine: Int, tokenColumn: Int): Unit = {
        advance()
        val builder = new StringBuilder
        var closed = false

        while index < source.length && !closed do
          val char = advance()
          char match
            case '"' =>
              closed = true
            case '\\' =>
              if index >= source.length then
                throw ParseError(
                  s"Unterminated escape sequence at $tokenLine:$tokenColumn"
                )

              val escaped = advance()
              val decoded = escaped match
                case '\\'  => '\\'
                case '"'   => '"'
                case 'n'   => '\n'
                case 'r'   => '\r'
                case 't'   => '\t'
                case 'b'   => '\b'
                case 'f'   => '\f'
                case other =>
                  throw ParseError(
                    s"Unsupported escape sequence \\$other at $line:${column - 1}"
                  )

              builder.append(decoded)
            case other =>
              builder.append(other)

        if !closed then
          throw ParseError(
            s"Unterminated string literal at $tokenLine:$tokenColumn"
          )

        add(TokenKind.String, builder.result(), tokenLine, tokenColumn)
      }

      while index < source.length do
        currentChar match
          case c if Character.isWhitespace(c) =>
            advance()

          case '/'
              if index + 1 < source.length && source.charAt(index + 1) == '/' =>
            skipLineComment()

          case '"' =>
            readString(line, column)

          case c if Character.isDigit(c) =>
            val tokenLine = line
            val tokenColumn = column
            val number = readWhile(Character.isDigit)
            add(TokenKind.Number, number, tokenLine, tokenColumn)

          case c if Character.isLetter(c) || c == '_' =>
            val tokenLine = line
            val tokenColumn = column
            val ident =
              readWhile(ch => Character.isLetterOrDigit(ch) || ch == '_')
            add(TokenKind.Identifier, ident, tokenLine, tokenColumn)

          case '(' =>
            add(TokenKind.LParen, "(", line, column)
            advance()

          case ')' =>
            add(TokenKind.RParen, ")", line, column)
            advance()

          case '[' =>
            add(TokenKind.LBracket, "[", line, column)
            advance()

          case ']' =>
            add(TokenKind.RBracket, "]", line, column)
            advance()

          case '{' =>
            add(TokenKind.LBrace, "{", line, column)
            advance()

          case '}' =>
            add(TokenKind.RBrace, "}", line, column)
            advance()

          case ',' =>
            add(TokenKind.Comma, ",", line, column)
            advance()

          case ';' =>
            add(TokenKind.Semicolon, ";", line, column)
            advance()

          case '+'
              if index + 1 < source.length && source.charAt(index + 1) == '+' =>
            add(TokenKind.PlusPlus, "++", line, column)
            advance()
            advance()

          case '+' =>
            add(TokenKind.Plus, "+", line, column)
            advance()

          case '='
              if index + 1 < source.length && source.charAt(index + 1) == '=' =>
            add(TokenKind.EqualsEquals, "==", line, column)
            advance()
            advance()

          case '='
              if index + 1 < source.length && source.charAt(index + 1) == '>' =>
            add(TokenKind.FatArrow, "=>", line, column)
            advance()
            advance()

          case '=' =>
            add(TokenKind.Equals, "=", line, column)
            advance()

          case '-'
              if index + 1 < source.length && source.charAt(index + 1) == '>' =>
            add(TokenKind.Arrow, "->", line, column)
            advance()
            advance()

          case other =>
            throw ParseError(s"Unexpected character '$other' at $line:$column")

      tokens += Token(TokenKind.Eof, "", line, column)
      tokens.result()
    }
  }

  private case class Parser(tokens: Vector[Token]) {
    private var index = 0

    def parseProgram(): Expr = {
      val expr = parseExpr()
      expect(TokenKind.Eof, "end of file")
      expr
    }

    private def parseExpr(): Expr =
      if acceptKeyword("let") then parseLet()
      else if acceptKeyword("handle") then parseHandle()
      else if acceptKeyword("if") then parseIfElse()
      else if acceptKeyword("return") then Expr.Return(parseValue())
      else if acceptKeyword("do") then parseDo()
      else parseApp()

    private def parseLet(): Expr = {
      val name = parseBindingName()
      expect(TokenKind.Equals, "=")
      val boundExpr = parseExpr()
      expectKeyword("in")
      val body = parseExpr()
      Expr.Let(name, boundExpr, body)
    }

    private def parseBindingName(): Option[ScalaString] = {
      val name = expectIdentifier()
      if name == "_" then None else Some(name)
    }

    private def parseHandle(): Expr = {
      val handledExpr = parseExpr()
      expectKeyword("with")
      Expr.Handle(handledExpr, parseHandler())
    }

    private def parseIfElse(): Expr = {
      val cond = parseValue()
      expectKeyword("then")
      val thenBranch = parseExpr()
      expectKeyword("else")
      val elseBranch = parseExpr()
      Expr.IfElse(cond, thenBranch, elseBranch)
    }

    private def parseDo(): Expr = {
      val label = expectIdentifier()
      expect(TokenKind.LParen, "(")
      val arg = parseValue()
      expect(TokenKind.RParen, ")")
      Expr.Do(label, arg)
    }

    private def parseApp(): Expr = {
      val func = parseValue()
      expect(TokenKind.LParen, "(")
      val arg = parseValue()
      expect(TokenKind.RParen, ")")
      Expr.App(func, arg)
    }

    private def parseHandler(): Handler = {
      expect(TokenKind.LBrace, "{")
      val returnClause = parseHandlerReturnClause()
      consumeSemicolons()

      val operationClauses = List.newBuilder[OperationClause]
      while !check(TokenKind.RBrace) do
        operationClauses += parseOperationClause()
        consumeSemicolons()

      expect(TokenKind.RBrace, "}")
      Handler(returnClause, operationClauses.result())
    }

    private def parseHandlerReturnClause(): ReturnClause = {
      expectKeyword("return")
      val param = expectIdentifier()
      expect(TokenKind.Arrow, "->")
      ReturnClause(param, parseExpr())
    }

    private def parseOperationClause(): OperationClause = {
      expectKeyword("case")
      val label = expectIdentifier()
      expect(TokenKind.LParen, "(")
      val param = expectIdentifier()
      expect(TokenKind.Comma, ",")
      val resumption = expectIdentifier()
      expect(TokenKind.RParen, ")")
      expect(TokenKind.Arrow, "->")
      OperationClause(label, param, resumption, parseExpr())
    }

    private def parseValue(): Value =
      parseEquality()

    private def parseEquality(): Value = {
      var left = parseConcat()

      while accept(TokenKind.EqualsEquals) do
        val right = parseConcat()
        left = Value.Equality(left, right)

      left
    }

    private def parseConcat(): Value = {
      var left = parseAdd()

      while accept(TokenKind.PlusPlus) do
        val right = parseAdd()
        left = Value.Concat(left, right)

      left
    }

    private def parseAdd(): Value = {
      var left = parsePostfix()

      while accept(TokenKind.Plus) do
        val right = parsePostfix()
        left = Value.Add(left, right)

      left
    }

    private def parsePostfix(): Value = {
      var value = parsePrimaryValue()

      while accept(TokenKind.LBracket) do
        val indexExpr = parseValue()
        expect(TokenKind.RBracket, "]")
        value = Value.Index(value, indexExpr)

      value
    }

    private def parsePrimaryValue(): Value =
      if acceptKeyword("fn") then parseLambda()
      else if accept(TokenKind.Number) then
        Value.Num(Integer.parseInt(previous.lexeme))
      else if accept(TokenKind.String) then Value.String(previous.lexeme)
      else if accept(TokenKind.Identifier) then Value.Var(previous.lexeme)
      else if accept(TokenKind.LBracket) then parseArray()
      else if accept(TokenKind.LParen) then
        val value = parseValue()
        expect(TokenKind.RParen, ")")
        value
      else throw error("value")

    private def parseLambda(): Value = {
      val param = expectIdentifier()
      expect(TokenKind.FatArrow, "=>")
      Value.Lambda(param, parseExpr())
    }

    private def parseArray(): Value = {
      val elements = List.newBuilder[Value]

      if !check(TokenKind.RBracket) then
        elements += parseValue()
        while accept(TokenKind.Comma) do elements += parseValue()

      expect(TokenKind.RBracket, "]")
      Value.Array(elements.result())
    }

    private def current: Token = tokens(index)

    private def previous: Token = tokens(index - 1)

    private def check(kind: TokenKind): Boolean =
      current.kind == kind

    private def accept(kind: TokenKind): Boolean =
      if check(kind) then
        index += 1
        true
      else false

    private def acceptKeyword(keyword: ScalaString): Boolean =
      if current.kind == TokenKind.Identifier && current.lexeme == keyword then
        index += 1
        true
      else false

    private def expect(kind: TokenKind, expected: ScalaString): Token =
      if accept(kind) then previous else throw error(expected)

    private def expectKeyword(keyword: ScalaString): Unit =
      if !acceptKeyword(keyword) then throw error(s"'$keyword'")

    private def expectIdentifier(): ScalaString =
      if accept(TokenKind.Identifier) then previous.lexeme
      else throw error("identifier")

    private def consumeSemicolons(): Unit =
      while accept(TokenKind.Semicolon) do ()

    private def error(expected: ScalaString): ParseError = {
      val found =
        if current.kind == TokenKind.Eof then "end of file"
        else s"'${current.lexeme}'"

      ParseError(
        s"Expected $expected at ${current.line}:${current.column}, found $found"
      )
    }
  }
}
