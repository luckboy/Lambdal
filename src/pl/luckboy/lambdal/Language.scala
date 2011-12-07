package pl.luckboy.lambdal
import scala.util.parsing.input.Positional

/**
 * @author Łukasz Szpakowski
 */
object Language
{
  trait Expr extends Positional
  case class Lambda(arg: String, expr: Expr) extends Expr
  case class App(fun: Expr, arg: Expr) extends Expr
  case class Var(ident: String) extends Expr
  case class PairExpr(fst: Expr, snd: Expr) extends Expr
  
  trait Value extends Expr
  {
    def apply(arg: Value): Value = BoolVal(false)
  }
  
  case class BoolVal(b: Boolean) extends Value with Expr
  {
    override def apply(arg: Value) = BuiltinFun { case arg2 => if(b) arg else arg2 }
    
    override def toString = b.toString
  }
  
  case class IntVal(i: Int) extends Value with Expr
  {
    override def apply(arg: Value) = BuiltinFun { case arg2 => (1 to i).foldLeft(arg2)((v, _) => arg(v)) }

    override def toString = i.toString
  }
  
  case class StringVal(s: String) extends Value with Expr
  {
    override def apply(arg: Value) = 
      arg match { 
        case IntVal(i) => if(i < s.length) StringVal(s.charAt(i).toString) else BoolVal(false)
      }

    override def toString = s
  }
  
  case class PairVal(fst: Value, snd: Value) extends Value with Expr
  {
    override def apply(arg: Value) = arg(fst)(snd)    

    override def toString = (fst, snd).toString
  }

  implicit def booleanToBoolVal(b: Boolean) = BoolVal(b)
  implicit def intToIntVal(i: Int) = IntVal(i)
  implicit def stringToStringVal(s: String) = StringVal(s)
  
  case class BuiltinFun(f: PartialFunction[Value, Value]) extends Value with Expr
  {
    override def apply(arg: Value) = if(f.isDefinedAt(arg)) f(arg) else BoolVal(false)
    
    override def toString = "<BuiltinFun>"
  }
  
  implicit def partialFunctionToBuiltinFun(f: PartialFunction[Value, Value]) = BuiltinFun(f)
  
  val builtinFuns: Map[String, Expr] = Map[String, Value](
      ("neg" -> BuiltinFun {
        case IntVal(x) => -x
      }),
      ("plus" -> BuiltinFun {
        case IntVal(x)    => BuiltinFun { case IntVal(y) => x + y }
        case StringVal(s) => BuiltinFun { case StringVal(t) => s + t }
      }),
      ("sub" -> BuiltinFun {
        case IntVal(x) => BuiltinFun { case IntVal(y) => x - y }
      }),
      ("mul" -> BuiltinFun {
        case IntVal(x)    => BuiltinFun { case IntVal(y) => x * y }
        case StringVal(s) => BuiltinFun { case IntVal(y) => s * y }
      }),
      ("div" -> BuiltinFun {
        case IntVal(x) => BuiltinFun { case IntVal(y) => x / y }
      }),
      ("mod" -> BuiltinFun {
        case IntVal(x) => BuiltinFun { case IntVal(y) => x % y }
      }),
      ("eq" -> BuiltinFun {
        case v => BuiltinFun { case w => v == w }
      }),
      ("neq" -> BuiltinFun {
        case v => BuiltinFun { case w => v != w }
      }),
      ("lt" -> BuiltinFun {
        case IntVal(x)    => BuiltinFun { case IntVal(y)    => x < y }
        case StringVal(s) => BuiltinFun { case StringVal(t) => s < t }
      }),
      ("lteq" -> BuiltinFun {
        case IntVal(x)    => BuiltinFun { case IntVal(y)    => x <= y }
        case StringVal(s) => BuiltinFun { case StringVal(t) => s <= t }
      }),
      ("gt" -> BuiltinFun {
        case IntVal(x)    => BuiltinFun { case IntVal(y)    => x > y }
        case StringVal(s) => BuiltinFun { case StringVal(t) => s > t }
      }),
      ("gteq" -> BuiltinFun {
        case IntVal(x)    => BuiltinFun { case IntVal(y)    => x >= y }
        case StringVal(s) => BuiltinFun { case StringVal(t) => s >= t }
      }),
      ("not" -> BuiltinFun {
        case BoolVal(x) => !x
      }),  
      ("and" -> BuiltinFun {
        case BoolVal(x) => BuiltinFun { case BoolVal(y) => x & y }
      }),
      ("or" -> BuiltinFun {
        case BoolVal(x) => BuiltinFun { case BoolVal(y) => x | y }
      }),
      ("fst" -> BuiltinFun {
        case PairVal(fst, snd) => fst
      }),
      ("snd" -> BuiltinFun {
        case PairVal(fst, snd) => snd
      }))
}