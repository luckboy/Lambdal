package pl.luckboy.lambdal
import scala.collection.immutable.PagedSeq
import scala.util.parsing.input.PagedSeqReader
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

/**
 * @author Łukasz Szpakowski
 */
object Parser extends StandardTokenParsers
{
  import Language._
  
  lexical.delimiters ++= List("=", ";", "(", ")", "\\", ".", ",")
  lexical.reserved ++= List("true", "false")
  
  case class Def(ident: String, expr: Expr)
  
  def definition = ident ~ "=" ~ expr ~ ";"              ^^ { case id ~ "=" ~ e ~ ";" => Def(id, e) }
  def file = definition *
  
  def expr: Parser[Expr] = app | simpleExpr
  def simpleExpr = value | variable | lambda | "(" ~> expr <~ ")"
  def app = simpleExpr ~ (simpleExpr +)                  ^^ { case e ~ es => es.foldLeft(e)(App(_, _)) }
  def lambda = "\\" ~ ident ~ "." ~ expr                 ^^ { case "\\" ~ id ~ "." ~ e => Lambda(id, e) }
  def value = boolVal | intVal | stringVal | pairVal
  def boolVal = ("true" | "false")                       ^^ { case s => BoolVal(s == "true") }
  def intVal = numericLit                                ^^ { case s => IntVal(s.toInt) }
  def stringVal = stringLit                              ^^ { case s => StringVal(s) }
  def pairVal = "(" ~> expr ~ "," ~ expr <~ ")"          ^^ { case e1 ~ "," ~ e2 => PairExpr(e1, e2) }
  def variable = ident                                   ^^ { case s => Var(s) }
  
  def parse(s: String) =
    phrase(file)(new lexical.Scanner(s))
    
  def parse(in: java.io.Reader) = {
    val reader = new PagedSeqReader(PagedSeq.fromReader(in))
    phrase(file)(new lexical.Scanner(reader))
  }
}