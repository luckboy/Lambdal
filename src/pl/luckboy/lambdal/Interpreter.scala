package pl.luckboy.lambdal
import java.io.BufferedReader
import java.io.PrintStream
import java.io.EOFException
import scala.util.control.Exception._

/**
 * @author Łukasz Szpakowski
 */
object Interpreter 
{  
  import Language._
  import Parser.Def
  
  private case object RecExpr extends Expr
  
  private case class Fun(lambda: Lambda, env: Map[String, Expr]) extends Value
  {
    override def apply(arg: Value): Value = 
      lambda match { 
      	case Lambda(id, e) => eval(e)(env + (id -> arg))
      }
    
    override def toString = "<Fun>"
  }
  
  def makeEnv(defs: List[Def]) =
    defs.foldLeft(builtinFuns) { 
      case (env, Def(id, e)) => if(!env.contains(id)) env + (id -> e) else throw new Exception("redefined variable")
    }
  
  def eval(expr: Expr)(env: Map[String, Expr]): Value =
    expr match {
      case BoolVal(b)          => BoolVal(b)
      case IntVal(i)           => IntVal(i)
      case StringVal(s)        => StringVal(s)
      case PairVal(a, b)       => PairVal(a, b)
      case lambda@Lambda(_, _) => Fun(lambda, env)
      case Var(id)             => eval(env.get(id).getOrElse(false))(env + (id -> RecExpr))
      case App(f, a)           => eval(f)(env)(eval(a)(env))
      case PairExpr(a, b)      => PairVal(eval(a)(env), eval(b)(env))
      case RecExpr             => throw new Exception("recursive call variable")
    }
  
  def interp(defs: List[Def])(in: BufferedReader, out: PrintStream) = {
    val env = makeEnv(defs)
    def mainLoop(f: Value, x: Value): Unit = {
      f(x) match {
        case PairVal(StringVal(cmd), g) =>
          val y = cmd match {
            case "read"     =>
              val c = in.read
              if(c != -1) StringVal(c.toChar.toString) else  BoolVal(false)
            case "readLine" =>
              catching(classOf[EOFException]).opt(StringVal(in.readLine)).getOrElse(BoolVal(false))
            case "print"    =>
              out.print(x)
              BoolVal(true)
            case "println"  =>
              out.println(x)
              BoolVal(true)
            case _          =>
              BoolVal(false)
          }
          mainLoop(g, y)
        case _                          =>
          ()  
      }
    }
    val f = eval(env.get("main").getOrElse(throw new Exception("no main")))(env)
    mainLoop(f, true)
  }
}