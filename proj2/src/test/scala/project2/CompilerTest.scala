package project2

import org.scalatest._
import java.io.{ByteArrayOutputStream, PrintWriter}

// Define the stream method
trait TestOutput {
  import Language._

  val out = new ByteArrayOutputStream
  val pOut = new PrintWriter(out, true)
  def stream = pOut
  def emitCode(ast: Exp): Unit

  def code(ast: Exp) = {
    emitCode(ast)
    out.toString.stripLineEnd
  }
}

class CompilerTest extends TimedSuite {
  import Language._

  def runner(src: String, gData: Map[Char,Int] = Map()) = new ASMRunner(src, gData)

  def testCompiler(ast: Exp, res: Int) = {
    val interpreter = new X86Compiler with TestOutput

    val code = interpreter.code(ast)
    val asm = runner(code)

    assert(asm.assemble == 0, "Code generated couldn't be assembled")
    assert(asm.run == res, "Invalid result")
  }

  test("arithm") {
    testCompiler(Lit(-21), -21)
    testCompiler(Prim("-", Lit(10), Lit(2)), 8)
  }

  test("valAssign") {
    testCompiler(Let("x",Lit(2),Prim("*",Ref("x"),Ref("x"))), 4)
    testCompiler(Let("x",Unary("-",Lit(2)),Let("y",Prim("*",Lit(3),Lit(4)),Prim("+",Ref("x"),Ref("y")))), 10)
  }

  test("loop") {
  testCompiler(VarDec("x",Lit(2),VarDec("y",Lit(0),While(Cond("<",Ref("y"),Lit(1)),VarDec("x",Prim("+",Ref("x"),Lit(1)),VarAssign("y",Prim("+",Ref("y"),Lit(1)))),Ref("x")))), 2)
    testCompiler(VarDec("x",Lit(2),VarDec("y",Lit(0),While(Cond("<",Ref("y"),Lit(1)),VarDec("x",Prim("+",Ref("x"),Lit(1)),VarAssign("y",Prim("+",Ref("y"),Lit(1)))),Ref("x")))), 2)
  }

  test("varAssign") {
    testCompiler(VarDec("x",Lit(2),If(Cond(">",Ref("x"),Lit(0)),VarAssign("x",Prim("+",Ref("x"),Lit(10))),Let("y",Lit(10),VarAssign("x",Prim("+",Ref("y"),Lit(19)))))), 12)
    testCompiler(VarDec("x",Lit(2),If(Cond(">",Ref("x"),Lit(0)),VarAssign("x",Lit(10)),VarAssign("x",Lit(22)))), 10)
  }

  test("Branch") {
    testCompiler(Let("x",Unary("-",Lit(2)),Let("y",Lit(4),If(Cond(">",Ref("y"),Ref("x")),Lit(1),Lit(0)))), 1)
    testCompiler(Let("x",Lit(2),If(Cond(">",Ref("x"),Lit(0)),Lit(22),Lit(2))), 22)
  }


}
