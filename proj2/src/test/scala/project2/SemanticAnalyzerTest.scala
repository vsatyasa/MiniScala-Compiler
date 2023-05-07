package project2

import org.scalatest._

class SemanticAnalyzerTest extends TimedSuite {
  import Language._

  def testSemanticAnalyzer(ast: Exp, nWarning: Int, nError: Int) = {
    val fakeParser = new Parser(null) {
      override def error(msg: String, pos: Position) = {}
      override def warn(msg: String, pos: Position) = {}
    }

    val analyzer = new SemanticAnalyzer(fakeParser)

    val (w, e) = analyzer.run(ast)
    assert(w == nWarning, "Incorrect number of Warnings")
    assert(e == nError, "Incorrect number of Errors")
  }

  test("29") {
    testSemanticAnalyzer(Lit(1), 0, 0)
    testSemanticAnalyzer(Prim("+", Lit(1), Lit(2)), 0, 0)
  }
  test("30"){
    testSemanticAnalyzer(Unary("*",Lit(1)),0,1)
  }
  test("31"){
    testSemanticAnalyzer(Prim("%",Lit(1),Lit(5)),0,1)
  }
  test("32"){
    testSemanticAnalyzer(VarDec("x",Lit(1),Let("x",Lit(5),Lit(4))), 1 ,0)
  }


  test("reassignmentError") {
    testSemanticAnalyzer(Let("x",Lit(2),VarAssign("x",Lit(1))), 0, 1)
  }

  test("reAssignWarn") {
    testSemanticAnalyzer(VarDec("x",Lit(1),VarDec("x",Lit(2),Ref("x"))), 1, 0)
  }

  test("unaryOpError") {
    testSemanticAnalyzer(Unary("---",Lit(1)), 0, 1)
  }

  test("unKnownBinaryOp") {
    testSemanticAnalyzer(Prim("-*-",Lit(1), Lit(1)), 0, 1)
  }

  test("unKnownBOp") {
    testSemanticAnalyzer(Cond(">>",Lit(1), Lit(1)), 0, 1)
  }

  test("unDeclaredVarError") {
    testSemanticAnalyzer(VarAssign("z",Lit(1)),0,1)
  }



}
