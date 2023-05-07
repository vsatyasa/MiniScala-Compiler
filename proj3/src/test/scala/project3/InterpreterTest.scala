package project3

import org.scalatest._

class InterpretTest extends TimedSuite {
  import Language._
  import StackVal._

  def testInterpreter(ast: Exp, res: Any) = {
    val interpreter = new StackInterpreter

    assert(res == interpreter.run(ast), "Interpreter does not return the correct value")
  }

  test("arithm") {
    testInterpreter(Lit(-21), Cst(-21))
    testInterpreter(Prim("-", List(Lit(10), Lit(2))), Cst(8))
  }

  test("arithm2") {
    testInterpreter(Lit(-22), Cst(-22))
  }

  test("GenericPrecedence") {
    testInterpreter(Prim("-", List(Lit(2), Prim("*", List(Lit(4), Lit(3))))), Cst(-10) )
  }

  test("VarSyntaxSugar") {
    testInterpreter(
    LetRec(List(),VarDec("x",UnknownType,Lit(2),Let("var$1",UnknownType,VarAssign("x",Prim("+",List(Ref("x"), Lit(1)))),Ref("x")))),
    Cst(3))
  }

  test("IfConditionSyntaxSugar") {
    testInterpreter(
      LetRec(List(),VarDec("x",UnknownType,Lit(2),Let("var$1",UnknownType,If(Prim(">=",List(Ref("x"), Lit(2))),VarAssign("x",Prim("+",List(Ref("x"), Lit(10)))),Lit(())),Ref("x")))),
      Cst(12)
    )
  }

    test("SingleFunctionParser") {
    testInterpreter(
      LetRec(List(FunDef("f",List(Arg("i",IntType,Position(0,6,0,6,0,7))),UnknownType,Ref("i"))),App(Ref("f"),List(Lit(2)))), Cst(2)
    )
  }

  test("MultipleFunctions") {
    testInterpreter( 
        LetRec(List(FunDef("f",List(Arg("i",IntType,Position(0,6,0,6,0,7))),IntType,App(Ref("h"),List(Ref("i")))), FunDef("h",List(Arg("i",IntType,Position(0,33,0,33,0,34))),IntType,Prim("*",List(Ref("i"), Ref("i"))))),App(Ref("f"),List(Lit(2)))),
        Cst(4)
      )
  }

    test("SingleArrayParser") {
    testInterpreter(
      LetRec(List(),Let("x",ArrayType(IntType),ArrayDec(Lit(5),IntType),Let("var$1",UnitType,Prim("block-set",List(Ref("x"), Lit(2), Lit(5))),Prim("block-get",List(Ref("x"), Lit(2)))))),
      Cst(5)
    )
  }

  test("DoubleArrayParser") {
    testInterpreter(
LetRec(List(),Let("x",ArrayType(IntType),ArrayDec(Lit(5),IntType),Let("var$1",UnitType,Prim("block-set",List(Ref("x"), Lit(2), Lit(6))),Prim("block-get",List(Ref("x"), Lit(2)))))),
      Cst(6)
    )
  }

  test("DoubleArrayParser2") {
    testInterpreter(
LetRec(List(),Let("x",ArrayType(IntType),ArrayDec(Lit(5),IntType),Let("var$1",UnitType,Prim("block-set",List(Ref("x"), Lit(2), Lit(6))),Prim("block-get",List(Ref("x"), Lit(2)))))),
      Cst(6)
    )
  }

}
