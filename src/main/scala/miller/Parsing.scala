package miller

import miller.AST._

import scala.util.parsing.combinator.{Parsers, PackratParsers}
import scala.util.parsing.input.CharArrayReader
import CharArrayReader.EofCh

import scala.language.{ postfixOps, implicitConversions }

object Parsing extends PackratParsers with ParsingUtils {

  type P[T] = PackratParser[T]

  val reserved = "function var return for in instanceof while if else break switch case true false delete void typeof new continue try catch finally throw".split(" ").toSet
  lazy val rchars = (('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9') ++ ".+*?()[]{}".toCharArray).map(_.toString).foldLeft("a": Parser[String])(_ | _)


  lazy val alphaUnder: P[Char] =            (('a' to 'z') ++ ('A' to 'Z')).foldLeft('_': Parser[Char])(_ | _)
  lazy val digit: P[Char] =                 ('1' to '9').foldLeft('0': Parser[Char])(_ | _)

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
                                                ~ charExcept('/', '\n', EofCh).*
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
  lazy val jsIn: P[Expr] =                  jsGtEq ~ (wsReq("in") ~> jsIn?)             ::> In.tupled
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

  lazy val jsParamsDef =                    "(" ~> (repsep(ident, ",") <~ ")")
  lazy val jsParams =                       "(" ~> (repsep(jsExprTop, ",") <~ ")")      :>  { case (ps, pos) => (ps, pos) }
  lazy val jsBlock =                        (("{" ~> blockStmts <~ "}")
                                            | (jsStmt ^^ {i => Seq(i)})
                                            | (jsContinue | jsBreak) ^^^ Seq()
                                            )

  lazy val jsRegex =                        regexLit                                    :>  { case ((reg, fs), pos) => LiteralRegExp(reg, fs, pos) }
  lazy val jsString =                       stringLit                                   :>  LiteralStr.tupled
  lazy val jsNumber =                       numericLit                                  :>  { case (n, pos) => LiteralNum(n, "0", pos)}

  lazy val jsNull =                         jsLiteralParser("null", Null)
  lazy val jsFalse =                        jsLiteralParser("false", False)
  lazy val jsTrue =                         jsLiteralParser("true", True)
  lazy val jsUndefined =                    jsLiteralParser("undefined", Undefined)
  lazy val jsThis =                         jsLiteralParser("this", This)

  lazy val jsFunction =                     wsOpt("function") ~> (ident?) ~
    jsParamsDef ~ jsBlock                         :>  { case (name ~ ps ~ b, pos) => JsFunction(name, ps, b, pos) }

  lazy val jsIdentifier =                   ident                                       :>  Ident.tupled

  lazy val objPair =                        (stringLit | numericLit | ident) ~
                                              (":" ~> jsExprTop)                        :>  { case (k ~ v, pos) => (k, v) }

  lazy val jsObject =                       "{" ~> (repsep(objPair, ",") <~ "}")        :>  { case (es, pos) => JsObject(es.toMap, pos) }
  lazy val jsArray =                        "[" ~> (repsep(jsExprTop, ",") <~ "]")      :>  JsArray.tupled

  lazy val jsReturn =                       "return" ~> jsExpr                          :>  Return.tupled
  lazy val jsDeclareWithVal =               ident ~ "=" ~ jsExprTop                     ^^  { case (id ~ "=" ~ e) => (id, Some(e)) }
  lazy val jsDeclareNoVal =                 ident                                       ^^  { case (id)           => (id, None) }
  lazy val jsDeclare =                      "var" ~> repsep(wsOpt(jsDeclareWithVal | jsDeclareNoVal), ",") :> Declare.tupled

  lazy val jsCond =                         "(" ~> (jsCommas <~ ")")

  lazy val jsWhile =                        "while" ~> jsCond ~ jsBlock                 :>  { case (cond ~ block, pos) => While(cond, block, pos) }

  lazy val jsFor =                          "for" ~> (jsFor1 | jsFor3)                  :>  { case (cond, pos) => cond.copy(pos = cond.pos.union(pos)) }

  lazy val jsForInit =                      (jsDeclare | jsExpr)?

  lazy val jsFor3 =                         "(" ~> jsForInit ~ (";" ~> (jsExpr?)) ~
                                              (";" ~> (jsExpr?) <~ ")") ~ jsBlock       :>  { case (dec ~ comp ~ exp ~ block, pos) => ??? }

  lazy val jsFor1 =                         ("(" ~> (
                                              "var" ~>  ident
                                                    ~   "in"
                                                    ~   jsExprTop
                                            ) <~ ")") ~ jsBlock                         :>  { case (i ~ "in" ~ exp ~ b, pos) => JsForIn(i, exp, b, pos) }

  lazy val jsIf =                           "if" ~> jsCond ~ jsBlock                    :>  { case (cond ~ block, pos) => If(cond, block, pos) }
  lazy val jsIfElse =                       jsIf ~ ("else" ~> jsBlock)                  :>  { case (If(c, b, pos) ~ fblock, pos2) => IfElse(c, b, fblock, pos union pos2) }

  lazy val jsTry =                          "try" ~> jsBlock ~ "catch" ~ "(" ~ ident ~ ")" ~ jsBlock :> { case (b ~ _ ~ _ ~ id ~ _ ~ b2, pos) => Undefined(pos) }

  lazy val jsSwitch =                       "switch" ~> jsCond ~ jsSwitchBlock          :>  { case (cond ~ block, pos) => Undefined(pos)}
  lazy val jsSwitchBlock =                  "{" ~> (jsCase.* <~ "}")                    :>  { case (cases, pos) => Undefined(pos) }
  lazy val jsCase =                         "case" ~> (jsExpr <~ ":") ~
                                                (blockStmts <~ (jsBreak?))              :>  { case (cond ~ block, pos) => Undefined(pos)}

  lazy val jsBreak =                        "break" ~ (";"?)
  lazy val jsContinue =                     "continue" ~ (";"?)

  lazy val jsThrow =                        "throw" ~> jsExpr                           :>  { case (e, pos) => Undefined(pos) }

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

trait ParsingUtils extends Parsers {
  type Elem = Char

  def jsLiteralParser[T <: Value](token: Parser[String], apply: (Position) => T) =
    Parser[T] { token :> (res => apply(res._2)) }

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
