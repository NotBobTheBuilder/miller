package miller

import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.syntactical.StdTokenParsers

import scala.language.{postfixOps, implicitConversions}

import miller.AST._

import scala.util.parsing.input.CharArrayReader._

trait JsTokens { this: StdLexical =>
  case class RegexLit(chars: String, flags: String) extends Token
}

trait JavaScriptParsers extends StdTokenParsers with PackratParsers {
  type Tokens = StdLexical
  type P[T] = PackratParser[T]

  def regexLit: Parser[(String, String)] = elem("string literal", _.isInstanceOf[lexical.RegexLit]) ^^ {i => val r = i.asInstanceOf[lexical.RegexLit]; (r.chars, r.flags)}

  val lexical: StdLexical with JsTokens = new StdLexical with JsTokens {
    override def token: Parser[Token] =
      ( identChar ~ rep( identChar | digit )                ^^ { case first ~ rest => processIdent(first :: rest mkString "") }
        | digit ~ rep( digit )                              ^^ { case first ~ rest => NumericLit(first :: rest mkString "") }
        | '\'' ~ rep( chrExcept('\'', '\n', EofCh) ) ~ '\'' ^^ { case '\'' ~ chars ~ '\'' => StringLit(chars mkString "") }
        | '\"' ~ rep( chrExcept('\"', '\n', EofCh) ) ~ '\"' ^^ { case '\"' ~ chars ~ '\"' => StringLit(chars mkString "") }
        | EofCh                                             ^^^ EOF
        | '\'' ~> failure("unclosed string literal")
        | '\"' ~> failure("unclosed string literal")
        | delim
//        | ('/' ~ log(rep(chrExcept('/', '\n', EofCh) ))("repbit") ~ '/' ~
//          log(rep(('g': Parser[Elem]) | 'i' | 'm' | 'u' | 'y'))("after")             ^^ { case '/' ~ chars ~ '/' ~ flags => RegexLit(chars mkString "", flags mkString "")})
        | '/' ^^^ Keyword("/")
        | failure("illegal character")
        )
  }

  lexical.delimiters ++= "== != === !== >= <= && || += -= |= ^= &= *= /= %= ++ --".split(" ") ++ "[]{}()+-*!&^;,.:=%<>?".split("")
  lexical.reserved ++= "function var return for in instanceof while if else break switch case true false delete void typeof new continue try catch finally throw".split(" ")
}

object Parsing extends JavaScriptParsers {

  lazy val jsCommas: P[Expr] =        jsBOrEq ~ ("," ~> jsCommas?)              ::> { case (head, tail, pos) => CommaList(Seq(head, tail), pos) }

  lazy val jsBOrEq: P[Expr] =         jsBXorEq ~ ("|=" ~> jsBOrEq?)             ::> BinOrEq.tupled
  lazy val jsBXorEq: P[Expr] =        jsBAndEq ~ ("^=" ~> jsBXorEq?)            ::> BinXorEq.tupled
  lazy val jsBAndEq: P[Expr] =        jsURShiftEq ~ ("&=" ~> jsBAndEq?)         ::> BinAndEq.tupled
  lazy val jsURShiftEq: P[Expr] =     jsRShiftEq ~ (">>>=" ~> jsURShiftEq?)     ::> URShiftEq.tupled
  lazy val jsRShiftEq: P[Expr] =      jsLShiftEq ~ (">>=" ~> jsRShiftEq?)       ::> RShiftEq.tupled
  lazy val jsLShiftEq: P[Expr] =      jsSubEq ~ ("<<=" ~> jsLShiftEq?)          ::> LShiftEq.tupled
  lazy val jsSubEq: P[Expr] =         jsAddEq ~ ("-=" ~> jsSubEq?)              ::> SubEq.tupled
  lazy val jsAddEq: P[Expr] =         jsModEq ~ ("+=" ~> jsAddEq?)              ::> AddEq.tupled
  lazy val jsModEq: P[Expr] =         jsMulEq ~ ("%=" ~> jsModEq?)              ::> ModEq.tupled
  lazy val jsMulEq: P[Expr] =         jsDivEq ~ ("*=" ~> jsMulEq?)              ::> MulEq.tupled
  lazy val jsDivEq: P[Expr] =         jsAssign ~ ("/=" ~> jsDivEq?)             ::> DivEq.tupled

  lazy val jsAssign: P[Expr] =        jsTernary ~ ("=" ~> jsAssign?)            ::> Assign.tupled

  lazy val jsTernary: P[Expr] =       jsOr ~ ((("?" ~> jsTernary) ~
                                              (":" ~> jsTernary))?)             ::> { case (x, y ~ z, pos) => Ternary(x, y, z, pos) }

  lazy val jsOr: P[Expr] =            jsAnd ~ ("||" ~> jsOr?)                   ::> Or.tupled
  lazy val jsAnd: P[Expr] =           jsBOr ~ ("&&" ~> jsAnd?)                  ::> And.tupled
  lazy val jsBOr: P[Expr] =           jsBXor ~ ("|" ~> jsBOr?)                  ::> BinOr.tupled
  lazy val jsBXor: P[Expr] =          jsBAnd ~ ("^" ~> jsBXor?)                 ::> BinXor.tupled
  lazy val jsBAnd: P[Expr] =          jsNEEq ~ ("&" ~> jsBAnd?)                 ::> BinAnd.tupled

  lazy val jsNEEq: P[Expr] =          jsEEq ~ ("!==" ~> jsNEEq?)                ::> NEEq.tupled
  lazy val jsEEq: P[Expr] =           jsNEq ~ ("===" ~> jsEEq?)                 ::> EEq.tupled
  lazy val jsNEq: P[Expr] =           jsEq ~ ("!=" ~> jsNEq?)                   ::> NEq.tupled
  lazy val jsEq: P[Expr] =            jsInstanceOf ~ ("==" ~> jsEq?)            ::> Eq.tupled

  lazy val jsInstanceOf: P[Expr] =    jsIn ~ ("instanceof" ~> jsInstanceOf?)    ::> InstanceOf.tupled
  lazy val jsIn: P[Expr] =            jsGtEq ~ ("in" ~> jsIn?)                  ::> In.tupled
  lazy val jsGtEq: P[Expr] =          jsGt ~ (">=" ~> jsGtEq?)                  ::> GtEq.tupled
  lazy val jsGt: P[Expr] =            jsLtEq ~ (">" ~> jsGt?)                   ::> Gt.tupled
  lazy val jsLtEq: P[Expr] =          jsLt ~ ("<=" ~> jsLtEq?)                  ::> LtEq.tupled
  lazy val jsLt: P[Expr] =            jsURShift ~ ("<" ~> jsLt?)                ::> Lt.tupled

  lazy val jsURShift: P[Expr] =       jsRShift ~ (">>>" ~> jsURShift?)          ::> URShift.tupled
  lazy val jsRShift: P[Expr] =        jsLShift ~ (">>" ~> jsRShift?)            ::> RShift.tupled
  lazy val jsLShift: P[Expr] =        jsSub ~ ("<<" ~> jsLShift?)               ::> LShift.tupled

  lazy val jsSub: P[Expr] =           jsAdd ~ ("-" ~> jsSub?)                   ::> Sub.tupled
  lazy val jsAdd: P[Expr] =           jsMod ~ ("+" ~> jsAdd?)                   ::> Add.tupled

  lazy val jsMod: P[Expr] =           jsMul ~ ("%" ~> jsMod?)                   ::> Mod.tupled
  lazy val jsMul: P[Expr] =           jsDiv ~ ("*" ~> jsMul?)                   ::> Mul.tupled
  lazy val jsDiv: P[Expr] =           jsDelete ~ ("/" ~> jsDiv?)                ::> Div.tupled

  lazy val jsDelete: P[Expr] =        "delete" ~> jsVoid                        :>  Delete.tupled   | jsVoid
  lazy val jsVoid: P[Expr] =          "void" ~> jsTypeOf                        :>  Void.tupled     | jsTypeOf
  lazy val jsTypeOf: P[Expr] =        "typeof" ~> jsPreSub                      :>  TypeOf.tupled   | jsPreSub
  lazy val jsPreSub: P[Expr] =        "--" ~> jsPreInc                          :>  PreDec.tupled   | jsPreInc
  lazy val jsPreInc: P[Expr] =        "++" ~> jsUSub                            :>  PreInc.tupled   | jsUSub
  lazy val jsUSub: P[Expr] =          "-" ~> jsUAdd                             :>  USub.tupled     | jsUAdd
  lazy val jsUAdd: P[Expr] =          "+" ~> jsBitNot                           :>  UAdd.tupled     | jsBitNot
  lazy val jsBitNot: P[Expr] =        "~" ~> jsNot                              :>  BitNot.tupled   | jsNot
  lazy val jsNot: P[Expr] =           "!" ~> jsPostDec                          :>  Not.tupled      | jsPostDec

  lazy val jsPostDec: P[Expr] =       (jsPostInc <~ "--")                       :>  { case (e, pos) => PostDec(e, pos) } | jsPostInc
  lazy val jsPostInc: P[Expr] =       (jsNewNoArgs <~ "++")                     :>  { case (e, pos) => PostInc(e, pos) } | jsNewNoArgs

  lazy val jsNewNoArgs: P[Expr] =     "new" ~> jsExprTop | jsFunctionCall

  lazy val jsFunctionCall: P[Expr] =  jsNewArgs ~ jsParams.?                    ::> { case (e, ps, pos) => JsCall(e, ps._1, pos) }

  lazy val jsNewArgs: P[Expr] =       "new" ~> jsExprTop ~ jsParams             :>  { case (con ~ ps, pos) => New(con, ps._1, pos) }         | jsCompMem
  lazy val jsCompMem: P[Expr] =       jsMember ~ ("[" ~> jsExprTop <~ "]").?    ::> CompMem.tupled
  lazy val jsMember: P[Expr] =        jsLiteral ~ ("." ~> ident?)               ::> Member.tupled

  lazy val jsBracket: P[Expr] =       "(" ~> jsExprTop <~ ")"

  lazy val jsLiteral: P[Expr] =       jsFunction |
                                      jsRegex |
                                      jsString |
                                      jsNumber |
                                      jsNull |
                                      jsFalse |
                                      jsTrue |
                                      jsUndefined |
                                      jsThis |
                                      jsArray |
                                      jsObject |
                                      jsFunctionCall |
                                      jsBracket |
                                      jsIdentifier

  lazy val jsParamsDef =              "(" ~> (repsep(ident, ",") <~ ")")
  lazy val jsParams =                 "(" ~> (repsep(jsExprTop, ",") <~ ")")      :>  { case (ps, pos) => (ps, pos) }
  lazy val jsBlock =                  ("{" ~> (jsStmt*) <~ "}") |
                                        (jsStmt ^^ {i => Seq(i)})


  lazy val rchars = (('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9') ++ ".+*?()[]{}".toCharArray).map(_.toString).foldLeft("a": Parser[String])(_ | _)

  lazy val jsRegex =                  regexLit                                 :>  { case (fs, pos) => LiteralRegExp(fs._1, fs._2, pos) }
  lazy val jsString =                 stringLit                                 :>  LiteralStr.tupled
  lazy val jsNumber =                 numericLit                                :>  { case (n, pos) => LiteralNum(n.toInt, 0, pos)}

  lazy val jsNull =                   jsLiteralParser("null", Null)
  lazy val jsFalse =                  jsLiteralParser("false", False)
  lazy val jsTrue =                   jsLiteralParser("true", True)
  lazy val jsUndefined =              jsLiteralParser("undefined", Undefined)
  lazy val jsThis =                   jsLiteralParser("this", This)

  lazy val jsFunction =               "function" ~> (ident?) ~
                                      jsParamsDef ~ jsBlock                     :>  { case (name ~ ps ~ b, pos) => JsFunction(name, ps, b, pos) }

  lazy val jsIdentifier =             ident                                     :>  Ident.tupled

  lazy val objPair =                  (stringLit | numericLit | ident) ~ (":" ~> jsExprTop)                  :>  { case (k ~ v, pos) => (k, v) }

  lazy val jsObject =                 "{" ~> (repsep(objPair, ",") <~ "}")      :>  JsObject.tupled
  lazy val jsArray =                  "[" ~> (repsep(jsExprTop, ",") <~ "]")    :>  JsArray.tupled

  lazy val jsReturn =                 "return" ~> jsExpr                        :>  Return.tupled
  lazy val jsDeclareWithVal =         jsIdentifier ~ "=" ~ jsExpr               ^^  { case (id ~ "=" ~ e) => (id.name, Some(e)) }
  lazy val jsDeclareNoVal =           jsIdentifier                              ^^  { case (id)           => (id.name, None) }
  lazy val jsDeclare =                "var" ~> repsep(jsDeclareWithVal | jsDeclareNoVal, ",") :> Declare.tupled

  lazy val jsCond =                   "(" ~> (jsCommas <~ ")")

  lazy val jsWhile =                  "while" ~> jsCond ~ jsBlock               :>  { case (cond ~ block, pos) => While(cond, block, pos) }

  lazy val jsFor =                    "for" ~> jsForCond ~ jsBlock              :>  { case (cond ~ block, pos) => Undefined(pos) }

  lazy val jsForInit =                (jsDeclare | jsExpr)?
  lazy val jsForCond =                "(" ~> jsForInit ~ (";" ~> (jsExpr?)) ~
                                         (";" ~> (jsExpr?) <~ ")")              :>  { case (dec ~ comp ~ exp, pos) => Undefined(pos) }


  lazy val jsIf =                     "if" ~> jsCond ~ jsBlock                  :>  { case (cond ~ block, pos) => If(cond, block, pos) }
  lazy val jsIfElse =                 jsIf ~ ("else" ~> jsBlock)                :>  { case (If(c, b, pos) ~ fblock, pos2) => IfElse(c, b, fblock, pos union pos2) }

  lazy val jsSwitch =                 "switch" ~> jsCond ~ jsSwitchBlock        :> { case (cond ~ block, pos) => Undefined(pos)}
  lazy val jsSwitchBlock =            "{" ~> (jsCase.* <~ "}")                  :> { case (cases, pos) => Undefined(pos) }
  lazy val jsCase =                   "case" ~> (jsExpr <~ ":") ~
                                         ((jsStmt*) <~ (jsBreak?))              :> { case (cond ~ block, pos) => Undefined(pos)}
  lazy val jsBreak =                  "break" ~ (";"?)


  lazy val jsProgram: P[Program] =    (jsStmt*)                                 :>  Program.tupled

  lazy val jsExprTop: P[Expr] =       jsBOrEq
  lazy val jsExpr: P[Expr] =          jsCommas
  lazy val jsStmt: P[Statement] =     (jsReturn | jsDeclare | jsWhile | jsSwitch | jsFor | jsIfElse | jsIf | jsExpr) <~ (";"?)

  def parse(in: String): ASTf.Program = ASTf.Program.program2f(jsProgram(new Parsing.PackratReader[lexical.Token](new lexical.Scanner(in))).get)

  def jsLiteralParser[T <: Value](token: String, apply: (Position) => T): P[T] =
    Parser[T] { (token: Parser[String]) :> { case (s, pos) => apply(pos) } }

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
