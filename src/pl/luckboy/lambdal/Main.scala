package pl.luckboy.lambdal
import java.io.FileReader
import scala.util.control.Exception._

/**
 * @author Łukasz Szpakowski
 */
object Main 
{
  import Parser.Def
  import Parser.parse
  import Interpreter.interp
  
  def main(args: Array[String]): Unit = {
    if(args.isEmpty) {
      println("Usage: lambdal <file> ...")
      exit(1)
    }
    try {
      val defs = args.foldLeft(List[Def]()) {
        (defs, filename) =>
          val in = new FileReader(filename)
          ultimately(in.close)(parse(in)).get
      }
      interp(defs)(Console.in, Console.out)
    } catch {
      case e: Exception => 
        Console.err.println(e.getMessage)
        exit(1)
    }
  }
}