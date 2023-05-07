package project2

import java.io._
import scala.sys.process._

// Error reporting
trait Reporter {
  // report a warning
  def warn(s: String): Unit = System.err.println(s"Warning: $s.")
  def warn(s: String, msg: String): Unit = System.err.println(s"Warning: $s.\n" + msg)
  // report an error
  def error(s: String): Unit = System.err.println(s"Error: $s.")
  def error(s: String, msg: String): Unit = System.err.println(s"Error: $s.\n" + msg)
  // report error and halt
  def abort(s: String): Nothing = { error(s); throw new Exception()}
  def abort(s: String, msg: String): Nothing = { error(s, msg); throw new Exception()}

  def expected(s: String): Nothing = abort(s"$s expected")
  def expected(s: String, msg: String): Nothing =
    abort(s"$s expected", msg)
}

trait BugReporter {
  def BUG(msg: String) = throw new Exception(s"BUG: $msg")
}

// Utilities to emit code
trait Codegen {

  def stream: PrintWriter

   // output
   def emit(s: String): Unit = stream.print('\t' + s)
   def emitln(s: String, nTab: Int = 1): Unit = stream.println("\t" * nTab + s)
}

abstract class Reader[T] {
  def pos: Int
  def input: String
  def peek: T
  def peek1: T // second look-ahead character used for comments '//'
  def hasNext: Boolean
  def hasNext(f: T => Boolean): Boolean
  def hasNext2(f: (T,T) => Boolean): Boolean
  def next(): T
}

class BaseReader(str: String, eof: Char) extends Reader[Char] {
  var pos = 0
  def input = str
  val in = str.iterator
  var peek = if (in.hasNext) in.next() else eof
  var peek1 = if (in.hasNext) in.next() else eof
  def hasNext: Boolean = peek != eof
  def hasNext(f: Char => Boolean) = f(peek)
  def hasNext2(f: (Char,Char) => Boolean) = f(peek,peek1)
  def next() = {
    val x = peek; peek = peek1;
    peek1 = if (in.hasNext) in.next() else eof
    pos += 1
    x
  }
}

// ASM bootstrapping
class ASMRunner(snipet: String, gData: Map[Char,Int]) {

  val data = if (gData.size > 0) {
    ".data\n" + (gData map {
      case (k,v) => s"$k:\t.quad $v"
    } mkString "\n")
  } else ""

  val template =
    s"""|.text
        |#if(__APPLE__)
        |\t.global _entry_point
        |
        |_entry_point:
        |#else
        |\t.global entry_point
        |
        |entry_point:
        |#endif
        |\tpush %rbp\t# save stack frame for C convention
        |\tmov %rsp, %rbp
        |
        |\tpushq %rbx
        |\tpushq %r12
        |\tpushq %r13
        |\tpushq %r14
        |\tpushq %r15
        |
        |\t# beginning generated code
        |${snipet}
        |\t# end generated code
        |\t# %rax contains the result
        |
        |\tpopq %r15
        |\tpopq %r14
        |\tpopq %r13
        |\tpopq %r12
        |\tpopq %rbx
        |\tmov %rbp, %rsp\t# reset frame
        |\tpop %rbp
        |\tret
        |
        |$data
        |""".stripMargin

  def code = template

  def assemble = {
    val file = new File("gen/gen.s")
    val writer = new PrintWriter(file)

    writer.println(template)
    writer.flush
    writer.close

    Seq("gcc","gen/bootstrap.c","gen/gen.s","-o","gen/out").!.toInt
  }

  def run = {
    val stdout = "gen/out".!!
    // output format: Result: <res>\n
    stdout.split(" ").last.trim.toInt
  }
}
