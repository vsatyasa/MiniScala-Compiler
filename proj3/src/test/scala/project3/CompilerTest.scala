package project3

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

  def runner(src: String) = new ASMRunner(src)

  def testCompiler(ast: Exp, res: Int) = {
    val interpreter = new X86Compiler with TestOutput

    val code = interpreter.code(ast)
    val asm = runner(code)

    assert(asm.assemble == 0, "Code generated couldn't be assembled")
    assert(asm.run == res, "Invalid result")
  }

  test("arithm") {
    testCompiler(LetRec(Nil, Lit(-21)), -21)
    testCompiler(LetRec(Nil, Prim("-", List(Lit(10), Lit(2)))), 8)
  }

  test("arithm2") {
    testCompiler(LetRec(Nil, Lit(-22)), -22)
  }


  test("GenericPrecedence") {
    testCompiler(LetRec(List(), Prim("-", List(Lit(2), Prim("*", List(Lit(4), Lit(3)))))), -10 )
  }

  test("VarSyntaxSugar") {
    testCompiler(
    LetRec(List(),VarDec("x",UnknownType,Lit(2),Let("var$1",UnknownType,VarAssign("x",Prim("+",List(Ref("x"), Lit(1)))),Ref("x")))),
    3)
  }

  test("IfConditionSyntaxSugar") {
    testCompiler(
      LetRec(List(),VarDec("x",UnknownType,Lit(2),Let("var$1",UnknownType,If(Prim(">=",List(Ref("x"), Lit(2))),VarAssign("x",Prim("+",List(Ref("x"), Lit(10)))),Lit(())),Ref("x")))),
      12
    )
  }

    test("SingleFunctionParser") {
    testCompiler(
      LetRec(List(FunDef("f",List(Arg("i",IntType,Position(0,6,0,6,0,7))),UnknownType,Ref("i"))),App(Ref("f"),List(Lit(2)))), 2
    )
  }

  test("MultipleFunctions") {
    testCompiler( 
        LetRec(List(FunDef("f",List(Arg("i",IntType,Position(0,6,0,6,0,7))),IntType,App(Ref("h"),List(Ref("i")))), FunDef("h",List(Arg("i",IntType,Position(0,33,0,33,0,34))),IntType,Prim("*",List(Ref("i"), Ref("i"))))),App(Ref("f"),List(Lit(2)))),
        4
      )
  }

    test("SingleArrayParser") {
    testCompiler(
      LetRec(List(),Let("x",ArrayType(IntType),ArrayDec(Lit(5),IntType),Let("var$1",UnitType,Prim("block-set",List(Ref("x"), Lit(2), Lit(5))),Prim("block-get",List(Ref("x"), Lit(2)))))),
      5
    )
  }

  test("DoubleArrayParser") {
    testCompiler(
LetRec(List(),Let("x",ArrayType(IntType),ArrayDec(Lit(5),IntType),Let("var$1",UnitType,Prim("block-set",List(Ref("x"), Lit(2), Lit(6))),Prim("block-get",List(Ref("x"), Lit(2)))))),
      6
    )
  }

    test("DoubleArrayParser2") {
    testCompiler(
LetRec(List(),Let("x",ArrayType(IntType),ArrayDec(Lit(5),IntType),Let("var$1",UnitType,Prim("block-set",List(Ref("x"), Lit(2), Lit(6))),Prim("block-get",List(Ref("x"), Lit(2)))))),
      6
    )
  }

}
