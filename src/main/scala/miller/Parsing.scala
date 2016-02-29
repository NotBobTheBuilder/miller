package miller

import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.syntactical.StdTokenParsers
import scala.util.parsing.combinator.PackratParsers
import scala.language.{postfixOps, implicitConversions}

import miller.AST._

object Parsing extends PackratParsers with StdTokenParsers {
  type Tokens = StdLexical
  type P[T] = PackratParser[T]

  val lexical = new StdLexical

  lexical.delimiters ++=  "== != === !== >= <= && || += -= |= ^= &= *= /= %=".split(" ") ++
                          "[]{}()+-*/!&^;,:=%<>?".split("")
  lexical.reserved ++= "function var return for while if break switch case true false".split(" ")

  def jsCommas: Parser[Expr] =        jsBOrEq

  def jsBOrEq: Parser[Expr] =         jsBXorEq ~ ("|=" ~> jsBXorEq?)            ::> { case (pos, x, y) => BinOrEq(x, y, pos) }
  def jsBXorEq: Parser[Expr] =        jsBAndEq ~ ("^=" ~> jsBAndEq?)            ::> { case (pos, x, y) => BinXorEq(x, y, pos) }
  def jsBAndEq: Parser[Expr] =        jsURShiftEq ~ ("&=" ~> jsURShiftEq?)      ::> { case (pos, x, y) => BinAndEq(x, y, pos) }
  def jsURShiftEq: Parser[Expr] =     jsRShiftEq ~ (">>>=" ~> jsRShiftEq?)      ::> { case (pos, x, y) => URShiftEq(x, y, pos) }
  def jsRShiftEq: Parser[Expr] =      jsLShiftEq ~ (">>=" ~> jsLShiftEq?)       ::> { case (pos, x, y) => RShiftEq(x, y, pos) }
  def jsLShiftEq: Parser[Expr] =      jsSubEq ~ ("<<=" ~> jsSubEq?)             ::> { case (pos, x, y) => LShiftEq(x, y, pos) }
  def jsSubEq: Parser[Expr] =         jsAddEq ~ ("-=" ~> jsAddEq?)              ::> { case (pos, x, y) => SubEq(x, y, pos) }
  def jsAddEq: Parser[Expr] =         jsModEq ~ ("+=" ~> jsModEq?)              ::> { case (pos, x, y) => AddEq(x, y, pos) }
  def jsModEq: Parser[Expr] =         jsMulEq ~ ("%=" ~> jsMulEq?)              ::> { case (pos, x, y) => ModEq(x, y, pos) }
  def jsMulEq: Parser[Expr] =         jsDivEq ~ ("*=" ~> jsDivEq?)              ::> { case (pos, x, y) => MulEq(x, y, pos) }
  def jsDivEq: Parser[Expr] =         jsAssign ~ ("/=" ~> jsAssign?)            ::> { case (pos, x, y) => DivEq(x, y, pos) }

  def jsAssign: Parser[Expr] =        jsTernary ~ ("=" ~> jsTernary?)           ::> { case (pos, x, y) => Assign(x, y, pos) }

  def jsTernary: Parser[Expr] =       jsOr ~ (("?" ~> jsOr) ~ (":" ~> jsOr)?)   ::> { case (pos, x, y ~ z) => Ternary(x, y, z, pos) }

  def jsOr: Parser[Expr] =            jsAnd ~ ("||" ~> jsAnd?)                  ::> { case (pos, x, y) => Or(x, y, pos) }
  def jsAnd: Parser[Expr] =           jsBOr ~ ("&&" ~> jsBOr?)                  ::> { case (pos, x, y) => And(x, y, pos) }
  def jsBOr: Parser[Expr] =           jsBXor ~ ("|" ~> jsBXor?)                 ::> { case (pos, x, y) => BinOr(x, y, pos) }
  def jsBXor: Parser[Expr] =          jsBAnd ~ ("^" ~> jsBAnd?)                 ::> { case (pos, x, y) => BinXor(x, y, pos) }
  def jsBAnd: Parser[Expr] =          jsNEEq ~ ("&" ~> jsNEEq?)                 ::> { case (pos, x, y) => BinAnd(x, y, pos) }

  def jsNEEq: Parser[Expr] =          jsEEq ~ ("!==" ~> jsEEq?)                 ::> { case (pos, x, y) => NEEq(x, y, pos) }
  def jsEEq: Parser[Expr] =           jsNEq ~ ("===" ~> jsNEq?)                 ::> { case (pos, x, y) => EEq(x, y, pos) }
  def jsNEq: Parser[Expr] =           jsEq ~ ("!=" ~> jsEq?)                    ::> { case (pos, x, y) => NEq(x, y, pos) }
  def jsEq: Parser[Expr] =            jsInstanceOf ~ ("==" ~> jsInstanceOf?)    ::> { case (pos, x, y) => Eq(x, y, pos) }

  def jsInstanceOf: Parser[Expr] =    jsIn ~ ("instanceof" ~> jsIn?)            ::> { case (pos, x, y) => InstanceOf(x, y, pos) }
  def jsIn: Parser[Expr] =            jsGtEq ~ ("in" ~> jsGtEq?)                ::> { case (pos, x, y) => In(x, y, pos) }
  def jsGtEq: Parser[Expr] =          jsGt ~ (">=" ~> jsGt?)                    ::> { case (pos, x, y) => GtEq(x, y, pos) }
  def jsGt: Parser[Expr] =            jsLtEq ~ (">" ~> jsLtEq?)                 ::> { case (pos, x, y) => Gt(x, y, pos) }
  def jsLtEq: Parser[Expr] =          jsLt ~ ("<=" ~> jsLt?)                    ::> { case (pos, x, y) => LtEq(x, y, pos) }
  def jsLt: Parser[Expr] =            jsURShift ~ ("<" ~> jsURShift?)           ::> { case (pos, x, y) => Lt(x, y, pos) }

  def jsURShift: Parser[Expr] =       jsRShift ~ (">>>" ~> jsRShift?)           ::> { case (pos, x, y) => URShift(x, y, pos) }
  def jsRShift: Parser[Expr] =        jsLShift ~ (">>" ~> jsLShift?)            ::> { case (pos, x, y) => RShift(x, y, pos) }
  def jsLShift: Parser[Expr] =        jsSub ~ ("<<" ~> jsSub?)                  ::> { case (pos, x, y) => LShift(x, y, pos) }

  def jsSub: Parser[Expr] =           jsAdd ~ ("-" ~> jsAdd?)                   ::> { case (pos, x, y)  => Sub(x, y, pos) }
  def jsAdd: Parser[Expr] =           jsMod ~ ("+" ~> jsMod?)                   ::> { case (pos, x, y)  => Add(x, y, pos) }

  def jsMod: Parser[Expr] =           jsMul ~ ("%" ~> jsMul?)                   ::> { case (pos, x, y)  => Mod(x, y, pos) }
  def jsMul: Parser[Expr] =           jsDiv ~ ("*" ~> jsDiv?)                   ::> { case (pos, x, y)  => Mul(x, y, pos) }
  def jsDiv: Parser[Expr] =           jsDelete ~ ("/" ~> jsDelete?)             ::> { case (pos, x, y)  => Div(x, y, pos) }

  def jsDelete: Parser[Expr] =        "delete" ~> jsCommas                      :> { case (pos, exp) => Delete(exp, pos) }  | jsVoid
  def jsVoid: Parser[Expr] =          "void" ~> jsCommas                        :> { case (pos, exp) => Void(exp, pos) }    | jsTypeOf
  def jsTypeOf: Parser[Expr] =        "typeof" ~> jsCommas                      :> { case (pos, exp) => TypeOf(exp, pos) }  | jsPreSub
  def jsPreSub: Parser[Expr] =        "--" ~> jsCommas                          :> { case (pos, exp) => PreSub(exp, pos) }  | jsPreInc
  def jsPreInc: Parser[Expr] =        "++" ~> jsCommas                          :> { case (pos, exp) => PreInc(exp, pos) }  | jsUSub
  def jsUSub: Parser[Expr] =          "-" ~> jsCommas                           :> { case (pos, exp) => USub(exp, pos) }    | jsUAdd
  def jsUAdd: Parser[Expr] =          "+" ~> jsCommas                           :> { case (pos, exp) => UAdd(exp, pos) }    | jsBitNot
  def jsBitNot: Parser[Expr] =        "~" ~> jsCommas                           :> { case (pos, exp) => BitNot(exp, pos) }  | jsNot
  def jsNot: Parser[Expr] =           "!" ~> jsCommas                           :> { case (pos, exp) => Not(exp, pos) }     | jsPostDec

  def jsPostDec: Parser[Expr] =       jsPostInc ~ ("--"?)                       ::> { case (pos, e, _) => PostDec(e, pos) }
  def jsPostInc: Parser[Expr] =       jsNewNoArgs ~ ("++"?)                     ::> { case (pos, e, _) => PostInc(e, pos) }

  def jsNewNoArgs: Parser[Expr] =     "new" ~> jsCommas | jsFunctionCall
  def jsFunctionCall: Parser[Expr] =  jsIdentifier ~ jsParams                   :> { case (pos, e ~ params) => JSCall(e, params, pos) } | jsNewArgs

  def jsNewArgs: Parser[Expr] =       "new" ~> jsCommas ~ jsParams              :> { case (pos, con ~ ps) => New(con, ps, pos) } | jsCompMem
  def jsCompMem: Parser[Expr] =       jsMember ~ ("[" ~> jsCommas <~ "]").?     ::> { case (pos, exp, prop) => CompMem(exp, prop, pos) }
  def jsMember: Parser[Expr] =        jsLiteral ~ ("." ~> ident?)               ::> { case (pos, exp, i) => Member(exp, i, pos) }

  def jsLiteral: Parser[Expr] =       jsString | jsNumber | jsNull | jsFalse | jsTrue | jsUndefined | jsThis | jsBracket

  def jsBracket: Parser[Expr] =       "(" ~> jsCommas <~ ")" | jsCommas

  def jsLiteralParser[T <: Value](token: String, apply: (Position) => T): Parser[T] =
    Parser[T] {
      (token: Parser[String]) :> { case (pos, s) => apply(pos) }
    }

  //def jsArray           = "[" ~ repsep(jsExpr, ",") ~ "]"                 :> { case (pos, "[" ~ elems ~ "]") =>  }
  def jsParamsDef       = "(" ~> repsep(ident, ",") <~ ")"
  def jsParams          = "(" ~> repsep(jsBracket, ",") <~ ")"
  def jsBlock           = "{" ~> (jsStatement*) <~ "}"

  def jsString          = stringLit   :> { case (pos, s) => LiteralStr(s, pos)}
  def jsNumber          = numericLit  :> { case (pos, n) => LiteralNum(n.toInt, 0, pos)}

  def jsNull            = jsLiteralParser("null", Null)
  def jsFalse           = jsLiteralParser("false", False)
  def jsTrue            = jsLiteralParser("true", True)
  def jsUndefined       = jsLiteralParser("undefined", Undefined)
  def jsThis            = jsLiteralParser("this", This)

  def jsFunction        = "function" ~ (ident?) ~ jsParamsDef ~ jsBlock :> { case (pos, "function" ~ name ~ ps ~ b) => JSFunction(name, ps, b, pos) }

  def jsIdentifier      = ident :> { case (pos, i) => Ident(i, pos) }

  def jsReturn          = "return" ~> jsExpr                    :> { case (pos, e)            => Return(e, pos) }
  def jsDeclareWithVal  = jsIdentifier ~ "=" ~ jsExpr           :> { case (pos, id ~ "=" ~ e) => Declare(id.name, Some(e), pos)}
  def jsDeclareNoVal    = jsIdentifier                          :> { case (pos, id)           => Declare(id.name, None, pos) }
  def jsDeclare         = "var" ~> (jsDeclareWithVal | jsDeclareNoVal)

  def jsCond            = "(" ~> (jsCommas <~ ")")

  def jsWhile           = "while" ~> jsCond ~ jsBlock    :> { case (pos, cond ~ block) => While(cond, block, pos) }
  def jsIf              = "if" ~> jsCond ~ jsBlock       :> { case (pos, cond ~ block) => If(cond, block, pos) }
  def jsIfElse          = jsIf ~ "else" ~ jsBlock        :> { case (pos2, If(c, b, pos) ~ "else" ~ fblock) => IfElse(c, b, fblock, pos2.copy(startLine = pos.startLine, startCol = pos.startCol)) }

  def jsExpr: Parser[Expr] =            jsCommas
  def jsStatement: Parser[Statement] =  jsReturn | jsDeclare | jsWhile | jsIfElse | jsIf | jsExpr

  def jsProgram: Parser[Program] =      jsStatement :> { case (pos, ss) => Program(Seq(ss), pos) }
  def parse(in: String): ASTf.Program = ASTf.Program.program2f(jsProgram(new lexical.Scanner(in)).get)

  class PositionedParser[T](parser: Parser[T]) {
    def :>[U](p: ((Position, T)) => U): Parser[U] = Parser { in =>
      parser(in) match {
        case Success(t, in1) => Success(p(Position(in.pos.line, in.pos.column, in1.pos.line, in1.pos.column), t), in1)
        case ns: NoSuccess => ns
      }
    }
  }
  implicit def parser2positionedParser[T](p: Parser[T]): PositionedParser[T] = new PositionedParser(p)

  class FirstFirstParser[T](parser: Parser[~[Expr, Option[T]]]) {
    def ::>(p: ((Position, Expr, T)) => Expr): Parser[Expr] = parser :> {
      case (pos, e ~ Some(e1)) => p((pos, e, e1))
      case (pos, e ~ None) => e
    }
  }
  implicit def parser2FirstFirstParser[T](p: Parser[~[Expr, Option[T]]]): FirstFirstParser[T] = new FirstFirstParser(p)


}
