package miller

import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.syntactical.StdTokenParsers

import miller.AST._

object Parsing extends StdTokenParsers {
  type Tokens = StdLexical

  val lexical = new StdLexical

  lexical.delimiters ++= "[]{}()+-*/!&^;,:=%<>".split("") ++ "== >= <=".split(" ")
  lexical.reserved ++= "function var return for while if break switch case true false".split(" ")

  def jsOpParser[T <: Op](symbol: String, apply: (Expr, Expr) => T): Parser[T] =
    jsExpr ~ (symbol ~> jsExpr) ^^ { case lhs ~ rhs => apply(lhs, rhs) }

  def jsLiteralParser[T <: Value](token: String, apply: () => T): Parser[Value] =
    Parser[T]{
      (in) => token ^^ { (s: String) => apply() } apply in
    }

  def jsFactor: Parser[Expr] = "(" ~> jsOp <~ ")" | jsFunctionCall | jsIdentifier | jsLiteral

  def jsTerm : Parser[Expr] =
    jsFactor ~ "*" ~ jsExpr   ^^ { case x ~ "*" ~ y => Mul(x, y) } |
      jsFactor ~ "/" ~ jsExpr ^^ { case x ~ "/" ~ y => Div(x, y) } | jsFactor

  def jsOp : Parser[Expr] =
    jsTerm ~ "<" ~ jsExpr     ^^ { case x ~ "<" ~ y => Lt(x, y)} |
      jsTerm ~ ">" ~ jsExpr   ^^ { case x ~ ">" ~ y => Gt(x, y)} |
      jsTerm ~ "<=" ~ jsExpr  ^^ { case x ~ "<=" ~ y => LtEq(x, y)} |
      jsTerm ~ ">=" ~ jsExpr  ^^ { case x ~ ">=" ~ y => GtEq(x, y)} |
      jsTerm ~ "==" ~ jsExpr  ^^ { case x ~ "==" ~ y => Eq(x, y)} |
      jsTerm ~ "+" ~ jsExpr   ^^ { case x ~ "+" ~ y => Add(x, y) } |
      jsTerm ~ "-" ~ jsExpr   ^^ { case x ~ "-" ~ y => Sub(x, y) } | jsTerm

  def jsParams          = "(" ~ repsep(ident, ",") ~ ")"                  ^^ { case "(" ~ ps ~ ")" => ps }
  def jsBlock           = "{" ~ (jsStatement*) ~ "}"                      ^^ { case "{" ~ ss ~ "}" => ss }
  def jsFunction        = "function" ~ (ident?) ~ jsParams ~ jsBlock      ^^ { case "function" ~ name ~ ps ~ b => JSFunction(name, ps, b)}
  def jsFunctionCall    = jsIdentifier ~ "(" ~ repsep(jsExpr, ",") ~ ")"  ^^ { case e ~ "(" ~ params ~ ")" => JSCall(e, params)}

  def jsString          = stringLit   ^^ { s => LiteralStr(s)}
  def jsNumber          = numericLit  ^^ { n => LiteralNum(n.toInt, 0)}

  def jsNull            = jsLiteralParser("null", Null.apply)
  def jsFalse           = jsLiteralParser("false", False.apply)
  def jsTrue            = jsLiteralParser("true", True.apply)
  def jsUndefined       = jsLiteralParser("undefined", Undefined.apply)
  def jsThis            = jsLiteralParser("this", This.apply)

  def jsIdentifier      = ident ^^ { i => Ident(i) }
  def jsBracketExpr     = "(" ~> jsExpr <~ ")"

  def jsReturn          = "return" ~> jsExpr                    ^^ { case e => Return(e) }
  def jsDeclareWithVal  = "var" ~> jsIdentifier ~ "=" ~ jsExpr  ^^ { case id ~ "=" ~ e => Declare(id.name, e)}
  def jsDeclareNoVal    = "var" ~> jsIdentifier                 ^^ { case id => Declare(id.name, Undefined()) }
  def jsAssign          = jsIdentifier ~ ("=" ~> jsExpr)        ^^ { case id ~ value => Assign(id.name, value) }
  def jsDeclare         = jsDeclareWithVal | jsDeclareNoVal

  def jsWhile           = "while" ~> jsBracketExpr ~ jsBlock    ^^ { case cond ~ block => While(cond, block) }
  def jsIf              = "if" ~> jsBracketExpr ~ jsBlock       ^^ { case cond ~ block => If(cond, block) }
  def jsIfElse          = jsIf ~ "else" ~ jsBlock               ^^ { case If(c, b) ~ "else" ~ fblock => IfElse(c, b, fblock) }

  def jsLiteral:    Parser[Value]     = jsString | jsNumber | jsNull | jsFalse | jsTrue | jsUndefined | jsThis
  def jsExpr:       Parser[Expr]      = jsOp | jsLiteral | jsIdentifier | jsBracketExpr | jsFunction
  def jsStatement:  Parser[Statement] = (jsReturn | jsDeclare | jsAssign | jsWhile | jsIfElse | jsIf ) <~ (";"?) | jsExpr
  def jsProgram = (jsStatement*) ^^ { ss => Program.apply(ss) }

  def parse(in: String): ASTf.Program = jsProgram(new lexical.Scanner (in)).get
}
