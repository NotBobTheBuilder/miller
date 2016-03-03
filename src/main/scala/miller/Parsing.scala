package miller

import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.syntactical.StdTokenParsers

import scala.language.{postfixOps, implicitConversions}

import miller.AST._

object Parsing extends StdTokenParsers {
  type Tokens = StdLexical

  val lexical = new StdLexical

  lexical.delimiters ++=  "== != === !== >= <= && || += -= |= ^= &= *= /= %=".split(" ") ++
                          "[]{}()+-*/!&^;,:=%<>?".split("")
  lexical.reserved ++= "function var return for while if break switch case true false".split(" ")

  def jsCommas: Parser[Expr] =        jsBOrEq ~ ("," ~> jsCommas?)              ::> { case (head, tail, pos) => CommaList(Seq(head, tail), pos) }

  def jsExprTop: Parser[Expr] =       jsBOrEq

  def jsBOrEq: Parser[Expr] =         jsBXorEq ~ ("|=" ~> jsBOrEq?)             ::> BinOrEq.tupled
  def jsBXorEq: Parser[Expr] =        jsBAndEq ~ ("^=" ~> jsBXorEq?)            ::> BinXorEq.tupled
  def jsBAndEq: Parser[Expr] =        jsURShiftEq ~ ("&=" ~> jsBAndEq?)         ::> BinAndEq.tupled
  def jsURShiftEq: Parser[Expr] =     jsRShiftEq ~ (">>>=" ~> jsURShiftEq?)     ::> URShiftEq.tupled
  def jsRShiftEq: Parser[Expr] =      jsLShiftEq ~ (">>=" ~> jsRShiftEq?)       ::> RShiftEq.tupled
  def jsLShiftEq: Parser[Expr] =      jsSubEq ~ ("<<=" ~> jsLShiftEq?)          ::> LShiftEq.tupled
  def jsSubEq: Parser[Expr] =         jsAddEq ~ ("-=" ~> jsSubEq?)              ::> SubEq.tupled
  def jsAddEq: Parser[Expr] =         jsModEq ~ ("+=" ~> jsAddEq?)              ::> AddEq.tupled
  def jsModEq: Parser[Expr] =         jsMulEq ~ ("%=" ~> jsModEq?)              ::> ModEq.tupled
  def jsMulEq: Parser[Expr] =         jsDivEq ~ ("*=" ~> jsMulEq?)              ::> MulEq.tupled
  def jsDivEq: Parser[Expr] =         jsAssign ~ ("/=" ~> jsDivEq?)             ::> DivEq.tupled

  def jsAssign: Parser[Expr] =        jsTernary ~ ("=" ~> jsAssign?)            ::> Assign.tupled

  def jsTernary: Parser[Expr] =       jsOr ~ (("?" ~> jsTernary) ~
                                              (":" ~> jsTernary)?)              ::> { case (x, y ~ z, pos) => Ternary(x, y, z, pos) }

  def jsOr: Parser[Expr] =            jsAnd ~ ("||" ~> jsOr?)                   ::> Or.tupled
  def jsAnd: Parser[Expr] =           jsBOr ~ ("&&" ~> jsAnd?)                  ::> And.tupled
  def jsBOr: Parser[Expr] =           jsBXor ~ ("|" ~> jsBOr?)                  ::> BinOr.tupled
  def jsBXor: Parser[Expr] =          jsBAnd ~ ("^" ~> jsBXor?)                 ::> BinXor.tupled
  def jsBAnd: Parser[Expr] =          jsNEEq ~ ("&" ~> jsBAnd?)                 ::> BinAnd.tupled

  def jsNEEq: Parser[Expr] =          jsEEq ~ ("!==" ~> jsNEEq?)                ::> NEEq.tupled
  def jsEEq: Parser[Expr] =           jsNEq ~ ("===" ~> jsEEq?)                 ::> EEq.tupled
  def jsNEq: Parser[Expr] =           jsEq ~ ("!=" ~> jsNEq?)                   ::> NEq.tupled
  def jsEq: Parser[Expr] =            jsInstanceOf ~ ("==" ~> jsEq?)            ::> Eq.tupled

  def jsInstanceOf: Parser[Expr] =    jsIn ~ ("instanceof" ~> jsInstanceOf?)    ::> InstanceOf.tupled
  def jsIn: Parser[Expr] =            jsGtEq ~ ("in" ~> jsIn?)                  ::> In.tupled
  def jsGtEq: Parser[Expr] =          jsGt ~ (">=" ~> jsGtEq?)                  ::> GtEq.tupled
  def jsGt: Parser[Expr] =            jsLtEq ~ (">" ~> jsGt?)                   ::> Gt.tupled
  def jsLtEq: Parser[Expr] =          jsLt ~ ("<=" ~> jsLtEq?)                  ::> LtEq.tupled
  def jsLt: Parser[Expr] =            jsURShift ~ ("<" ~> jsLt?)                ::> Lt.tupled

  def jsURShift: Parser[Expr] =       jsRShift ~ (">>>" ~> jsURShift?)          ::> URShift.tupled
  def jsRShift: Parser[Expr] =        jsLShift ~ (">>" ~> jsRShift?)            ::> RShift.tupled
  def jsLShift: Parser[Expr] =        jsSub ~ ("<<" ~> jsLShift?)               ::> LShift.tupled

  def jsSub: Parser[Expr] =           jsAdd ~ ("-" ~> jsSub?)                   ::> Sub.tupled
  def jsAdd: Parser[Expr] =           jsMod ~ ("+" ~> jsAdd?)                   ::> Add.tupled

  def jsMod: Parser[Expr] =           jsMul ~ ("%" ~> jsMod?)                   ::> Mod.tupled
  def jsMul: Parser[Expr] =           jsDiv ~ ("*" ~> jsMul?)                   ::> Mul.tupled
  def jsDiv: Parser[Expr] =           jsDelete ~ ("/" ~> jsDiv?)                ::> Div.tupled

  def jsDelete: Parser[Expr] =        "delete" ~> jsCommas                      :>  Delete.tupled   | jsVoid
  def jsVoid: Parser[Expr] =          "void" ~> jsCommas                        :>  Void.tupled     | jsTypeOf
  def jsTypeOf: Parser[Expr] =        "typeof" ~> jsCommas                      :>  TypeOf.tupled   | jsPreSub
  def jsPreSub: Parser[Expr] =        "--" ~> jsCommas                          :>  PreDec.tupled   | jsPreInc
  def jsPreInc: Parser[Expr] =        "++" ~> jsCommas                          :>  PreInc.tupled   | jsUSub
  def jsUSub: Parser[Expr] =          "-" ~> jsCommas                           :>  USub.tupled     | jsUAdd
  def jsUAdd: Parser[Expr] =          "+" ~> jsCommas                           :>  UAdd.tupled     | jsBitNot
  def jsBitNot: Parser[Expr] =        "~" ~> jsCommas                           :>  BitNot.tupled   | jsNot
  def jsNot: Parser[Expr] =           "!" ~> jsCommas                           :>  Not.tupled      | jsPostDec

  def jsPostDec: Parser[Expr] =       jsPostInc ~ ("--"?)                       ::> { case (e, _, pos) => PostDec(e, pos) }
  def jsPostInc: Parser[Expr] =       jsNewNoArgs ~ ("++"?)                     ::> { case (e, _, pos) => PostInc(e, pos) }

  def jsNewNoArgs: Parser[Expr] =     "new" ~> jsCommas | jsFunctionCall
  def jsFunctionCall: Parser[Expr] =  jsNewArgs ~ ((jsParams*)?)                ::> { case (e, ps, pos) => ps.foldLeft(e)((e, ps) => JSCall(e, ps._1, ps._2)) }

  def jsNewArgs: Parser[Expr] =       "new" ~> jsCommas ~ jsParams              :>  { case (con ~ ps, pos) => New(con, ps._1, pos) }         | jsCompMem
  def jsCompMem: Parser[Expr] =       jsMember ~ ("[" ~> jsCommas <~ "]").?     ::> CompMem.tupled
  def jsMember: Parser[Expr] =        jsLiteral ~ ("." ~> ident?)               ::> Member.tupled

  def jsLiteral: Parser[Expr] =       jsIdentifier |
                                      jsString |
                                      jsNumber |
                                      jsNull |
                                      jsFalse |
                                      jsTrue |
                                      jsUndefined |
                                      jsThis |
                                      jsFunction |
                                      jsArray |
                                      jsObject |
                                      jsBracket

  def jsBracket: Parser[Expr] =       "(" ~> jsCommas <~ ")"

  def jsLiteralParser[T <: Value](token: String, apply: (Position) => T): Parser[T] =
    Parser[T] { (token: Parser[String]) :> { case (s, pos) => apply(pos) } }

  def jsParamsDef =                   "(" ~> (repsep(ident, ",") <~ ")")
  def jsParams =                      "(" ~> (repsep(jsExprTop, ",") <~ ")")    :>  { case (ps, pos) => (ps, pos) }
  def jsBlock =                       "{" ~> (jsStmt*) <~ "}"

  def jsString =                      stringLit                                 :>  LiteralStr.tupled
  def jsNumber =                      numericLit                                :>  { case (n, pos) => LiteralNum(n.toInt, 0, pos)}

  def jsNull =                        jsLiteralParser("null", Null)
  def jsFalse =                       jsLiteralParser("false", False)
  def jsTrue =                        jsLiteralParser("true", True)
  def jsUndefined =                   jsLiteralParser("undefined", Undefined)
  def jsThis =                        jsLiteralParser("this", This)

  def jsFunction =                    "function" ~> (ident?) ~
                                        jsParamsDef ~ jsBlock                   :>  { case (name ~ ps ~ b, pos) => JSFunction(name, ps, b, pos) }

  def jsIdentifier =                  ident                                     :>  Ident.tupled

  def objPair =                       jsExpr ~ (":" ~> jsExpr)                  :>  { case (k ~ v, pos) => (k, v) }

  def jsObject =                      "{" ~> (repsep(objPair, ",") <~ "}")      :>  JsObject.tupled
  def jsArray =                       "[" ~> (repsep(jsExprTop, ",") <~ "]")    :>  JsArray.tupled

  def jsReturn =                      "return" ~> jsExpr                        :>  Return.tupled
  def jsDeclareWithVal =              jsIdentifier ~ "=" ~ jsExpr               :>  { case (id ~ "=" ~ e, pos) => Declare(id.name, Some(e), pos)}
  def jsDeclareNoVal =                jsIdentifier                              :>  { case (id, pos)           => Declare(id.name, None, pos) }
  def jsDeclare =                     "var" ~> (jsDeclareWithVal | jsDeclareNoVal)

  def jsCond =                        "(" ~> (jsCommas <~ ")")

  def jsWhile =                       "while" ~> jsCond ~ jsBlock               :>  { case (cond ~ block, pos) => While(cond, block, pos) }
  def jsIf =                          "if" ~> jsCond ~ jsBlock                  :>  { case (cond ~ block, pos) => If(cond, block, pos) }
  def jsIfElse =                      jsIf ~ ("else" ~> jsBlock)                :>  { case (If(c, b, pos) ~ fblock, pos2) => IfElse(c, b, fblock, pos union pos2) }

  def jsProgram: Parser[Program] =    (jsStmt*)                                 :>  Program.tupled

  def jsExpr: Parser[Expr] =          jsCommas
  def jsStmt: Parser[Statement] =     (jsReturn | jsDeclare | jsWhile | jsIfElse | jsIf | jsExpr) <~ (";"?)

  def parse(in: String): ASTf.Program = ASTf.Program.program2f(jsProgram(new lexical.Scanner(in)).get)

  implicit class PositionedParser[T](parser: Parser[T]) {
    def :>[U](p: ((T, Position)) => U): Parser[U] = Parser { in =>
      parser(in) match {
        case Success(t, in1) => Success(p(t, Position(in.pos.line, in.pos.column, in1.pos.line, in1.pos.column)), in1)
        case ns: NoSuccess => ns
      }
    }
  }

  implicit class FirstFirstParser[T](parser: Parser[~[Expr, Option[T]]]) {
    def ::>(p: ((Expr, T, Position)) => Expr): Parser[Expr] = parser :> {
      case (e ~ Some(e1), pos) => p((e, e1, pos))
      case (e ~ None, pos) => e
    }
  }
}
