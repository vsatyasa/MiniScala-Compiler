package project2

import org.scalatest._

class InterpretTest extends TimedSuite {
  import Language._

  def testInterpreter(ast: Exp, res: Int) = {
    val interpreter = new StackInterpreter

    assert(res == interpreter.run(ast), "Interpreter does not return the correct value")
  }

  test("arithm") {
    testInterpreter(Lit(-21), -21)
    testInterpreter(Prim("-", Lit(10), Lit(2)), 8)
  }


  test("valAssign") {
    testInterpreter(Let("x",Lit(2),Prim("*",Ref("x"),Ref("x"))), 4)
    testInterpreter(Let("x",Unary("-",Lit(2)),Let("y",Prim("*",Lit(3),Lit(4)),Prim("+",Ref("x"),Ref("y")))), 10)
  }

  test("loop") {
  testInterpreter(VarDec("x",Lit(2),VarDec("y",Lit(0),While(Cond("<",Ref("y"),Lit(1)),VarDec("x",Prim("+",Ref("x"),Lit(1)),VarAssign("y",Prim("+",Ref("y"),Lit(1)))),Ref("x")))), 2)
    testInterpreter(VarDec("x",Lit(2),VarDec("y",Lit(0),While(Cond("<",Ref("y"),Lit(1)),VarDec("x",Prim("+",Ref("x"),Lit(1)),VarAssign("y",Prim("+",Ref("y"),Lit(1)))),Ref("x")))), 2)
  }

  test("varAssign") {
    testInterpreter(VarDec("x",Lit(2),If(Cond(">",Ref("x"),Lit(0)),VarAssign("x",Prim("+",Ref("x"),Lit(10))),Let("y",Lit(10),VarAssign("x",Prim("+",Ref("y"),Lit(19)))))), 12)
    testInterpreter(VarDec("x",Lit(2),If(Cond(">",Ref("x"),Lit(0)),VarAssign("x",Lit(10)),VarAssign("x",Lit(22)))), 10)
  }

  test("Branch") {
    testInterpreter(Let("x",Unary("-",Lit(2)),Let("y",Lit(4),If(Cond(">",Ref("y"),Ref("x")),Lit(1),Lit(0)))), 1)
    testInterpreter(Let("x",Lit(2),If(Cond(">",Ref("x"),Lit(0)),Lit(22),Lit(2))), 22)
  }

}
