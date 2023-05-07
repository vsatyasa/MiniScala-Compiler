package project1

import java.io._
import org.scalatest._

// Define the stream method
trait TestOutput {
  val out = new ByteArrayOutputStream
  val pOut = new PrintWriter(out, true)
  def stream = pOut
  def emitCode(ast: Exp): Unit

  def code(ast: Exp) = {
    emitCode(ast)
    out.toString.stripLineEnd
  }
}

class StackGeneratorTest extends TimedSuite {

  def runner(src: String, gData: Map[Char,Int] = Map()) = new ASMRunner(src, gData)

  // Function Helper for StackASMGenerator
  def testStackASMGenerator(ast: Exp, res: Int) = {
    val gen = new StackASMGenerator with TestOutput

    val code = gen.code(ast)
    val asm = runner(code)

    assert(asm.assemble == 0, "Code generated couldn't be assembled")
    assert(asm.run == res, "Invalid result")
  }

  test("SingleDigit") {
    testStackASMGenerator(Lit(2), 2)
  }

  test("MulAdd") {
    testStackASMGenerator(Plus(Plus(Lit(1), Lit(2)),Lit(3)), 6)
    testStackASMGenerator(Plus(Lit(1), Lit(2)), 3)
  }

  test("MulSub") {
    testStackASMGenerator(Minus(Plus(Lit(1),Lit(2)),Lit(3)), 0)
    testStackASMGenerator(Minus(Minus(Lit(1),Lit(2)),Lit(3)), -4)
  }

  test("Mulop") {
    testStackASMGenerator(Div(Times(Div(Lit(8),Lit(9)),Lit(4)),Lit(2)), 0)
    testStackASMGenerator(Plus(Plus(Lit(8),Lit(9)),Times(Lit(4),Lit(2))), 25)
  }

  // TODO more tests
}

class RegGeneratorTest extends TimedSuite {

  def runner(src: String, gData: Map[Char,Int] = Map()) = new ASMRunner(src, gData)

  // Function Helper for StackASMGenerator
  def testRegASMGenerator(ast: Exp, res: Int) = {
    val gen = new RegASMGenerator with TestOutput

    val code = gen.code(ast)
    val asm = runner(code)

    assert(asm.assemble == 0, "Code generated couldn't be assembled")
    assert(asm.run == res, "Invalid result")
  }

  test("SingleDigit") {
    testRegASMGenerator(Lit(2), 2)
  }

  test("MulAdd") {
    testRegASMGenerator(Plus(Plus(Lit(1), Lit(2)),Lit(3)), 6)
    testRegASMGenerator(Plus(Lit(1), Lit(2)), 3)
  }

  test("MulSub") {
    testRegASMGenerator(Minus(Plus(Lit(1),Lit(2)),Lit(3)), 0)
    testRegASMGenerator(Minus(Minus(Lit(1),Lit(2)),Lit(3)), -4)
  }

  test("Mulop") {
    testRegASMGenerator(Div(Times(Div(Lit(8),Lit(9)),Lit(4)),Lit(2)), 0)
    testRegASMGenerator(Plus(Plus(Lit(8),Lit(9)),Times(Lit(4),Lit(2))), 25)
  }


  // TODO more tests
}
