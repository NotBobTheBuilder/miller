package miller

import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.syntactical.StdTokenParsers

import miller.AST._

object Parsing extends StdTokenParsers {
  type Tokens = StdLexical

  val lexical = new StdLexical

  lexical.delimiters ++= "[]{}()+-*/!&^;,:=%<>".split("") ++ "== >= <=".split(" ")
  lexical.reserved ++= "function var return for while if break switch case true false".split(" ")

  def jsOpParser[T <: Op](symbol: String, apply: (Expr, Expr, Position) => T): Parser[T] =
    jsExpr ~ (symbol ~> jsExpr) :> { case (pos, lhs ~ rhs) => apply(lhs, rhs, pos) }

  def jsLiteralParser[T <: Value](token: String, apply: (Position) => T): Parser[Value] =
    Parser[T]{
      (in) => (token: Parser[String]) :> { case (pos, s) => apply(pos) } apply in
    }

  def jsFactor: Parser[Expr] = "(" ~> jsOp <~ ")" | jsFunctionCall | jsIdentifier | jsLiteral

  def jsTerm : Parser[Expr] =
    jsFactor ~ "*" ~ jsExpr   :> { case (pos, x ~ "*" ~ y)  => Mul(x, y, pos) } |
      jsFactor ~ "/" ~ jsExpr :> { case (pos, x ~ "/" ~ y)  => Div(x, y, pos) } | jsFactor

  def jsOp : Parser[Expr] =
    jsTerm ~ "<" ~ jsExpr     :> { case (pos, x ~ "<" ~ y)  => Lt(x, y, pos)} |
      jsTerm ~ ">" ~ jsExpr   :> { case (pos, x ~ ">" ~ y)  => Gt(x, y, pos)} |
      jsTerm ~ "<=" ~ jsExpr  :> { case (pos, x ~ "<=" ~ y) => LtEq(x, y, pos)} |
      jsTerm ~ ">=" ~ jsExpr  :> { case (pos, x ~ ">=" ~ y) => GtEq(x, y, pos)} |
      jsTerm ~ "==" ~ jsExpr  :> { case (pos, x ~ "==" ~ y) => Eq(x, y, pos)} |
      jsTerm ~ "+" ~ jsExpr   :> { case (pos, x ~ "+" ~ y)  => Add(x, y, pos) } |
      jsTerm ~ "-" ~ jsExpr   :> { case (pos, x ~ "-" ~ y)  => Sub(x, y, pos) } | jsTerm

  def jsParams          = "(" ~ repsep(ident, ",") ~ ")"                  ^^ { case "(" ~ ps ~ ")" => ps }
  def jsBlock           = "{" ~ (jsStatement*) ~ "}"                      ^^ { case "{" ~ ss ~ "}" => ss }
  def jsFunction        = "function" ~ (ident?) ~ jsParams ~ jsBlock      :> { case (pos, "function" ~ name ~ ps ~ b) => JSFunction(name, ps, b, pos)}
  def jsFunctionCall    = jsIdentifier ~ "(" ~ repsep(jsExpr, ",") ~ ")"  :> { case (pos, e ~ "(" ~ params ~ ")") => JSCall(e, params, pos)}

  def jsString          = stringLit   :> { case (pos, s) => LiteralStr(s, pos)}
  def jsNumber          = numericLit  :> { case (pos, n) => LiteralNum(n.toInt, 0, pos)}

  def jsNull            = jsLiteralParser("null", Null.apply)
  def jsFalse           = jsLiteralParser("false", False.apply)
  def jsTrue            = jsLiteralParser("true", True.apply)
  def jsUndefined       = jsLiteralParser("undefined", Undefined.apply)
  def jsThis            = jsLiteralParser("this", This.apply)

  def jsIdentifier      = ident :> { case (pos, i) => Ident(i, pos) }
  def jsBracketExpr     = "(" ~> jsExpr <~ ")"

  def jsReturn          = "return" ~> jsExpr                    :> { case (pos, e)            => Return(e, pos) }
  def jsDeclareWithVal  = "var" ~> jsIdentifier ~ "=" ~ jsExpr  :> { case (pos, id ~ "=" ~ e) => Declare(id.name, Some(e), pos)}
  def jsDeclareNoVal    = "var" ~> jsIdentifier                 :> { case (pos, id)           => Declare(id.name, None, pos) }
  def jsAssign          = jsIdentifier ~ ("=" ~> jsExpr)        :> { case (pos, id ~ value)   => Assign(id.name, value, pos) }
  def jsDeclare         = jsDeclareWithVal | jsDeclareNoVal

  def jsWhile           = "while" ~> jsBracketExpr ~ jsBlock    :> { case (pos, cond ~ block) => While(cond, block, pos) }
  def jsIf              = "if" ~> jsBracketExpr ~ jsBlock       :> { case (pos, cond ~ block) => If(cond, block, pos) }
  def jsIfElse          = jsIf ~ "else" ~ jsBlock               :> { case (pos2, If(c, b, pos) ~ "else" ~ fblock) => IfElse(c, b, fblock, pos2.copy(startLine = pos.startLine, startCol = pos.startCol)) }

  def jsLiteral:    Parser[Value]     = jsString | jsNumber | jsNull | jsFalse | jsTrue | jsUndefined | jsThis
  def jsExpr:       Parser[Expr]      = jsOp | jsLiteral | jsIdentifier | jsBracketExpr | jsFunction
  def jsStatement:  Parser[Statement] = (jsReturn | jsDeclare | jsAssign | jsWhile | jsIfElse | jsIf ) <~ (";"?) | jsExpr
  def jsProgram = (jsStatement*) :> { case (pos, ss) => Program(ss, pos) }

  def parse(in: String): ASTf.Program = ASTf.Program.program2f(jsProgram(new lexical.Scanner (in)).get)

  class PositionedParser[T](parser: Parser[T]) {
    def :>[U](p: ((Position, T)) => U): Parser[U] = Parser { in =>
      parser(in) match {
        case Success(t, in1) => Success(p(Position(in.pos.line, in.pos.column, in1.pos.line, in1.pos.column), t), in1)
        case ns: NoSuccess => ns
      }
    }
  }

  implicit def parser2positionedParser[T](p: Parser[T]): PositionedParser[T] = new PositionedParser(p)
}
