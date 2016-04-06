package miller

import miller.AST._

import scala.util.parsing.combinator.{Parsers, PackratParsers}
import scala.util.parsing.input.CharArrayReader
import CharArrayReader.EofCh

import scala.language.{ postfixOps, implicitConversions }

object Parsing extends PackratParsers with ParsingUtils {

  type P[T] = PackratParser[T]

  val reserved = "function var return for instanceof in while if else break switch case default true false delete void typeof new continue try catch finally throw".split(" ")
  lazy val rchars = (('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9') ++ ".+*?()[]{}".toCharArray).map(_.toString).foldLeft("a": Parser[String])(_ | _)

  lazy val ident: P[String] =               wsOpt(
                                              alphaUnder ~ (alphaUnder | digit).* ^^ { case a ~ bs => (a +: bs).mkString }
                                            ) filter (word => !reserved.contains(word))

  lazy val dqString: P[String] =            '\"' ~ charExcept('\"', '\n', EofCh).* ~ '\"' ^^ { case '\"' ~ cs ~ '\"' => cs.mkString }
  lazy val sqString: P[String] =            '\'' ~ charExcept('\'', '\n', EofCh).* ~ '\'' ^^ { case '\'' ~ cs ~ '\'' => cs.mkString }
  lazy val stringLit: P[String] =           wsOpt(dqString | sqString)

  lazy val numericLit: P[String] =          wsOpt(digit.+ ^^ { case ss => ss.mkString})
  lazy val regexLit: P[(String, String)] =  wsOpt(
                                                '/'
                                                ~ charExcept('*', '/', '\n', EofCh)
                                                ~ (('\\' ~ '/') | charExcept('/', '\n', EofCh)).*
                                                ~ '/'
                                                ~ ((('g': Parser[Char]) | 'i' | 'm' | 'u' | 'y').* ^^ (_.toSet.mkString))
                                                                                        ^^ { case '/' ~ t ~ cs ~ '/' ~ fs => ((t +: cs).mkString, fs) })

  lazy val jsCommas: P[Expr] =              jsBOrEq ~ ("," ~> jsCommas?)                ::> { case (head, tail, pos) => CommaList(Seq(head, tail), pos) }

  lazy val jsBOrEq: P[Expr] =               jsBXorEq ~ ("|=" ~> jsBOrEq?)               ::> BinOrEq.tupled
  lazy val jsBXorEq: P[Expr] =              jsBAndEq ~ ("^=" ~> jsBXorEq?)              ::> BinXorEq.tupled
  lazy val jsBAndEq: P[Expr] =              jsURShiftEq ~ ("&=" ~> jsBAndEq?)           ::> BinAndEq.tupled
  lazy val jsURShiftEq: P[Expr] =           jsRShiftEq ~ (">>>=" ~> jsURShiftEq?)       ::> URShiftEq.tupled
  lazy val jsRShiftEq: P[Expr] =            jsLShiftEq ~ (">>=" ~> jsRShiftEq?)         ::> RShiftEq.tupled
  lazy val jsLShiftEq: P[Expr] =            jsSubEq ~ ("<<=" ~> jsLShiftEq?)            ::> LShiftEq.tupled
  lazy val jsSubEq: P[Expr] =               jsAddEq ~ ("-=" ~> jsSubEq?)                ::> SubEq.tupled
  lazy val jsAddEq: P[Expr] =               jsModEq ~ ("+=" ~> jsAddEq?)                ::> AddEq.tupled
  lazy val jsModEq: P[Expr] =               jsMulEq ~ ("%=" ~> jsModEq?)                ::> ModEq.tupled
  lazy val jsMulEq: P[Expr] =               jsDivEq ~ ("*=" ~> jsMulEq?)                ::> MulEq.tupled
  lazy val jsDivEq: P[Expr] =               jsAssign ~ ("/=" ~> jsDivEq?)               ::> DivEq.tupled

  lazy val jsAssign: P[Expr] =              jsTernary ~ ("=" ~> jsAssign?)              ::> Assign.tupled

  lazy val jsTernary: P[Expr] =             jsOr ~ ((("?" ~> jsTernary) ~
                                                (":" ~> jsTernary))?)                   ::> { case (x, y ~ z, pos) => Ternary(x, y, z, pos) }

  lazy val jsOr: P[Expr] =                  jsAnd ~ ("||" ~> jsOr?)                     ::> Or.tupled
  lazy val jsAnd: P[Expr] =                 jsBOr ~ ("&&" ~> jsAnd?)                    ::> And.tupled
  lazy val jsBOr: P[Expr] =                 jsBXor ~ ("|" ~> jsBOr?)                    ::> BinOr.tupled
  lazy val jsBXor: P[Expr] =                jsBAnd ~ ("^" ~> jsBXor?)                   ::> BinXor.tupled
  lazy val jsBAnd: P[Expr] =                jsNEEq ~ ("&" ~> jsBAnd?)                   ::> BinAnd.tupled

  lazy val jsNEEq: P[Expr] =                jsEEq ~ ("!==" ~> jsNEEq?)                  ::> NEEq.tupled
  lazy val jsEEq: P[Expr] =                 jsNEq ~ ("===" ~> jsEEq?)                   ::> EEq.tupled
  lazy val jsNEq: P[Expr] =                 jsEq ~ ("!=" ~> jsNEq?)                     ::> NEq.tupled
  lazy val jsEq: P[Expr] =                  jsInstanceOf ~ ("==" ~> jsEq?)              ::> Eq.tupled

  lazy val jsInstanceOf: P[Expr] =          jsIn ~ ("instanceof" ~> jsInstanceOf?)      ::> InstanceOf.tupled
  lazy val jsIn: P[Expr] =                  jsGtEq ~ (('i'~'n'~ whitespace.+) ~> jsIn?) ::> In.tupled
  lazy val jsGtEq: P[Expr] =                jsGt ~ (">=" ~> jsGtEq?)                    ::> GtEq.tupled
  lazy val jsGt: P[Expr] =                  jsLtEq ~ (">" ~> jsGt?)                     ::> Gt.tupled
  lazy val jsLtEq: P[Expr] =                jsLt ~ ("<=" ~> jsLtEq?)                    ::> LtEq.tupled
  lazy val jsLt: P[Expr] =                  jsURShift ~ ("<" ~> jsLt?)                  ::> Lt.tupled

  lazy val jsURShift: P[Expr] =             jsRShift ~ (">>>" ~> jsURShift?)            ::> URShift.tupled
  lazy val jsRShift: P[Expr] =              jsLShift ~ (">>" ~> jsRShift?)              ::> RShift.tupled
  lazy val jsLShift: P[Expr] =              jsSub ~ ("<<" ~> jsLShift?)                 ::> LShift.tupled

  lazy val jsSub: P[Expr] =                 jsAdd ~ ("-" ~> jsSub?)                     ::> Sub.tupled
  lazy val jsAdd: P[Expr] =                 jsMod ~ ("+" ~> jsAdd?)                     ::> Add.tupled

  lazy val jsMod: P[Expr] =                 jsMul ~ ("%" ~> jsMod?)                     ::> Mod.tupled
  lazy val jsMul: P[Expr] =                 jsDiv ~ ("*" ~> jsMul?)                     ::> Mul.tupled
  lazy val jsDiv: P[Expr] =                 jsDelete ~ ("/" ~> jsDiv?)                  ::> Div.tupled

  lazy val jsDelete: P[Expr] =              "delete" ~> jsVoid                          :>  Delete.tupled   | jsVoid
  lazy val jsVoid: P[Expr] =                "void" ~> jsVoid                            :>  Void.tupled     | jsTypeOf
  lazy val jsTypeOf: P[Expr] =              "typeof" ~> jsTypeOf                        :>  TypeOf.tupled   | jsPreSub
  lazy val jsPreSub: P[Expr] =              "--" ~> jsPostDec                           :>  PreDec.tupled   | jsPreInc
  lazy val jsPreInc: P[Expr] =              "++" ~> jsPostDec                           :>  PreInc.tupled   | jsUSub
  lazy val jsUSub: P[Expr] =                "-" ~> jsPostDec                            :>  USub.tupled     | jsUAdd
  lazy val jsUAdd: P[Expr] =                "+" ~> jsPostDec                            :>  UAdd.tupled     | jsBitNot
  lazy val jsBitNot: P[Expr] =              "~" ~> jsBitNot                             :>  BitNot.tupled   | jsNot
  lazy val jsNot: P[Expr] =                 "!" ~> jsNot                                :>  Not.tupled      | jsPostDec

  lazy val jsPostDec: P[Expr] =             (jsPostInc <~ "--")                         :>  { case (e, pos) => PostDec(e, pos) } | jsPostInc
  lazy val jsPostInc: P[Expr] =             (jsNewNoArgs <~ "++")                       :>  { case (e, pos) => PostInc(e, pos) } | jsNewNoArgs

  lazy val jsNewNoArgs: P[Expr] =           "new" ~> jsExprTop | jsFunctionCall

  lazy val jsFunctionCall: P[Expr] =        jsNewArgs ~ jsParams.?                      ::> { case (e, ps, pos) => JsCall(e, ps._1, pos) }

  lazy val jsNewArgs: P[Expr] =             "new" ~> jsExprTop ~ jsParams               :>  { case (con ~ ps, pos) => New(con, ps._1, pos) }         | jsCompMem
  lazy val jsCompMem: P[Expr] =             jsMember ~ ("[" ~> jsExprTop <~ "]").?      ::> CompMem.tupled
  lazy val jsMember: P[Expr] =              jsLiteral ~ ("." ~> ident?)                 ::> Member.tupled

  lazy val jsBracket: P[Expr] =             "(" ~> jsExprTop <~ ")"

  lazy val jsLiteral: P[Expr] =             wsOpt(jsFunction
                                                  | jsRegex
                                                  | jsString
                                                  | jsNumber
                                                  | jsNull
                                                  | jsFalse
                                                  | jsTrue
                                                  | jsUndefined
                                                  | jsThis
                                                  | jsArray
                                                  | jsObject
                                                  | jsFunctionCall
                                                  | jsBracket
                                                  | jsIdentifier
                                                )

  lazy val jsParamsDef: P[Seq[String]] =    "(" ~> (repsep(ident, ",") <~ ")")
  lazy val jsParams: P[(Seq[Expr], Position)] =
                                            "(" ~> (repsep(jsExprTop, ",") <~ ")")      :>  { case (ps, pos) => (ps, pos) }
  lazy val jsBlock: P[Seq[Statement]] =     (("{" ~> blockStmts <~ "}")
                                            | (jsStmt ^^ {i => Seq(i)})
                                            | (jsContinue | jsBreak) ^^^ Seq()
                                            )

  lazy val jsRegex: P[LiteralRegExp] =      regexLit                                    :>  { case ((reg, fs), pos) => LiteralRegExp(reg, fs, pos) }
  lazy val jsString: P[LiteralStr] =        stringLit                                   :>  LiteralStr.tupled
  lazy val jsNumber: P[LiteralNum] =        numericLit                                  :>  { case (n, pos) => LiteralNum(n, "0", pos)}

  lazy val jsNull: P[Null] =                jsLiteralParser("null", Null)
  lazy val jsFalse: P[False]  =             jsLiteralParser("false", False)
  lazy val jsTrue: P[True] =                jsLiteralParser("true", True)
  lazy val jsUndefined: P[Undefined] =      jsLiteralParser("undefined", Undefined)
  lazy val jsThis: P[This] =                jsLiteralParser("this", This)

  lazy val jsFunction: P[JsFunction] =      wsOpt("function") ~> (ident?) ~
                                              jsParamsDef ~ jsBlock                     :>  { case (name ~ ps ~ b, pos) => JsFunction(name, ps, b, pos) }

  lazy val jsIdentifier: P[Ident] =         ident                                       :>  Ident.tupled

  lazy val objPair: P[(String, Expr)] =     (stringLit | numericLit | ident) ~
                                              (":" ~> jsExprTop)                        :>  { case (k ~ v, pos) => (k, v) }

  lazy val jsObject: P[JsObject] =          "{" ~> (repsep(objPair, ",") <~ "}")        :>  { case (es, pos) => JsObject(es.toMap, pos) }
  lazy val jsArray: P[JsArray] =            "[" ~> (repsep(jsExprTop, ",") <~ "]")      :>  JsArray.tupled

  lazy val jsReturn: P[Return] =            "return" ~> jsExpr                          :>  Return.tupled
  lazy val jsDeclareWithVal =               ident ~ "=" ~ jsExprTop                     ^^  { case (id ~ "=" ~ e) => (id, Some(e)) }
  lazy val jsDeclareNoVal =                 ident                                       ^^  { case (id)           => (id, None) }
  lazy val jsDeclare: P[Declare] =          "var" ~> repsep(wsOpt(jsDeclareWithVal | jsDeclareNoVal), ",") :> Declare.tupled

  lazy val jsCond: P[Expr] =                "(" ~> (jsCommas <~ ")")

  lazy val jsWhile: P[While] =              "while" ~> jsCond ~ jsBlock                 :>  { case (cond ~ block, pos) => While(cond, block, pos) }

  lazy val jsFor: P[Statement] =            "for" ~> (jsFor1 | jsFor3)                  :>  {
                                                                                              case (cond: JsFor, pos) => cond.copy(pos = cond.pos.union(pos))
                                                                                              case (cond: JsForIn, pos) => cond.copy(pos = cond.pos.union(pos))
                                                                                            }

  lazy val jsForInit: P[Option[Statement]] =(jsDeclare | jsExpr)?

  lazy val jsFor3: P[JsFor] =               "(" ~> jsForInit ~ (";" ~> (jsExpr?)) ~
                                              (";" ~> (jsExpr?) <~ ")") ~ jsBlock       :>  { case (dec ~ comp ~ exp ~ block, pos) => JsFor(dec, comp, exp, block, pos) }

  lazy val jsFor1: P[JsForIn] =             ("(" ~> (
                                              "var" ~>  ident
                                                    ~   "in"
                                                    ~   jsExprTop
                                            ) <~ ")") ~ jsBlock                         :>  { case (i ~ "in" ~ exp ~ b, pos) => JsForIn(i, exp, b, pos) }

  lazy val jsIf: P[If] =                    "if" ~> jsCond ~ jsBlock                    :>  { case (cond ~ block, pos) => If(cond, block, pos) }
  lazy val jsIfElse: P[IfElse] =            jsIf ~ ("else" ~> jsBlock)                  :>  { case (If(c, b, pos) ~ fblock, pos2) => IfElse(c, b, fblock, pos union pos2) }

  lazy val jsTry: P[Statement] =            "try" ~> jsBlock ~ "catch" ~ "(" ~ ident ~ ")" ~ jsBlock :> { case (b ~ _ ~ _ ~ id ~ _ ~ b2, pos) => Undefined(pos) }

  lazy val jsSwitch: P[Statement] =         "switch" ~> jsCond ~ jsSwitchBlock          :>  { case (cond ~ block, pos) => Undefined(pos)}
  lazy val jsSwitchBlock: P[Statement] =    "{" ~> ((jsCase | jsDefault).* <~ "}")      :>  { case (cases, pos) => Undefined(pos) }
  lazy val jsCase: P[Statement] =           "case" ~> (jsExpr <~ ":") ~
                                                (blockStmts <~ (jsBreak?))              :>  { case (cond ~ block, pos) => Undefined(pos)}
  lazy val jsDefault: P[Statement] =        (wsOpt("default") ~ ":") ~
                                                (blockStmts <~ (jsBreak?))              :>  { case (_ ~ block, pos) => Undefined(pos)}

  lazy val jsBreak: P[Any] =                "break" ~ (";"?)
  lazy val jsContinue: P[Any] =             "continue" ~ (";"?)

  lazy val jsThrow: P[Statement] =          "throw" ~> jsExpr                           :>  { case (e, pos) => Undefined(pos) }

  lazy val jsProgram: P[Program] =          jsStmt.*                                    :>  { case (prog, pos) => Program(prog, pos) }

  lazy val jsExprTop: P[Expr] =             jsBOrEq
  lazy val jsExpr: P[Expr] =                jsCommas
  lazy val jsStmt: P[Statement] =           (jsReturn | jsDeclare | jsWhile | jsSwitch | jsTry | jsFor | jsIfElse | jsIf | jsThrow | jsExpr) <~ (";"?)

  lazy val blockStmts: P[Seq[Statement]] =  jsStmt.* <~ ((jsBreak | jsContinue)?)


  private def reader(s: String): PackratReader[Char] = new PackratReader(new CharArrayReader(s.toCharArray))

  def parseAST(in: String): AST.Program = jsProgram(reader(in)).get
  def parse(in: String)(implicit scope: ScopeStack = new ScopeStack()): ASTf.Program = ASTf.Program.program2f(jsProgram(reader(in)).get)(scope)

  def parseExpr(in: String)(implicit st: ScopeStack): AST.Expr = {
    try {
      jsExpr(reader(in)).get
    } catch {
      case e: Exception =>
        println(s"Got $e when working with input $in")
        throw e
    }
  }
}

trait ParsingUtils extends PackratParsers with Parsers  {
  type Elem = Char

  lazy val alphaUnder =            (('a' to 'z') ++ ('A' to 'Z') :+ '$').foldLeft('_': Parser[Char])(_ | _)
  lazy val digit =                 ('1' to '9').foldLeft('0': Parser[Char])(_ | _)

  def jsLiteralParser[T <: Value](token: Parser[String], apply: (Position) => T) =
    Parser[T] { token ~ not(alphaUnder | digit) :> (res => apply(res._2)) }

  implicit class PositionedParser[T](parser: Parser[T]) {
    def :>[U](p: ((T, Position)) => U): Parser[U] = Parser { in =>
      parser(in) match {
        case Success(t, in1) => Success(p(t, Position(in.pos.line, in.pos.column, in1.pos.line, in1.pos.column)), in1)
        case ns: NoSuccess => ns
      }
    }
  }

  implicit class LeftRecursiveParser[U, T](parser: Parser[U ~ Option[T]]) {
    def ::>(p: ((U, T, Position)) => U): Parser[U] = parser :> {
      case (e ~ Some(e1), pos) => p((e, e1, pos))
      case (e ~ None, pos) => e
    }
  }

  def charExcept(chars: Char*) = acceptIf(!chars.contains(_))(_ => "")

  def whitespace: Parser[Any] = (
    acceptIf(Set(' ', '\n').contains)(_ => " unexpected").+
      | '/' ~ '*' ~ commentBlockEnd
      | '/' ~ '/' ~ rep( charExcept(EofCh, '\n') )
      | '/' ~ '*' ~ failure("unclosed comment")
    )

  def commentBlockEnd: Parser[Any] = (
    '*' ~ '/'  ^^ { case _ => ' ' }
    | charExcept(EofCh) ~ commentBlockEnd
  )

  // TODO: the position returned by this seems to include the trailing whitespace. Figure that out.
  def wsOpt[T](p: Parser[T]): Parser[T] = whitespace.* ~> (p <~ whitespace.*)
  def wsReq(p: String): Parser[String] = whitespace.* ~> (p <~ whitespace.+)

  implicit def string2Parser(s: String): Parser[String] = wsOpt(acceptSeq(s).map(_ => s))
}
