package project2

import java.io._
import org.scalatest._

class ParserTest extends TimedSuite {

  import Language._

  def scanner(src: String) = new Scanner(new BaseReader(src, '\u0000'))

  def testGenericPrecedence(op: String, res: Exp) = {
    val gen = new ArithParser(scanner(op))
    val ast = gen.parseCode

    assert(ast == res, "Invalid result")
  }

  def testLetParser(op: String, res: Exp) = {
    val gen = new LetParser(scanner(op))
    val ast = gen.parseCode

    assert(ast == res, "Invalid result")
  }

  def testBranchParser(op: String, res: Exp) = {
    val gen = new BranchParser(scanner(op))
    val ast = gen.parseCode

    assert(ast == res, "Invalid result")
  }
  def testVariableParser(op: String, res: Exp) = {
    val gen = new VariableParser(scanner(op))
    val ast = gen.parseCode

    assert(ast == res, "Invalid result")
  }
  def testLoopParser(op: String, res: Exp) = {
    val gen = new LoopParser(scanner(op))
    val ast = gen.parseCode

    assert(ast == res, "Invalid result")
  }

  /*test("SingleDigit") {
    testGenericPrecedence("1", Lit(1))
  }

  test("GenericPrecedence") {
    testGenericPrecedence("2-4*3", Prim("-", Lit(2), Prim("*", Lit(4), Lit(3))))
  }*/
  test("1") {
    testGenericPrecedence(s"${(1 << 31) - 1}", Lit({
      (1 << 31) - 1
    }))
  }
  test("2") {
    testGenericPrecedence(s"${(1 << 31) - 2}", Lit({
      (1 << 31) - 2
    }))
  }
  /*test("3") {
    val thrown = intercept[Exception] {
      testGenericPrecedence(s"${(1 << 31)}", Lit({(1 << 31) - 1}))
    }
    assert(thrown != null)
  }*/
  test("3") {
    assertThrows[Exception] { // Result type: Assertion
      testGenericPrecedence(s"${(1 << 31)}", Lit({
        (1 << 31) - 1
      }))
    }
  }
  test("4") {
    testGenericPrecedence("12+5*30", Prim("+", Lit(12), Prim("*", Lit(5), Lit(30))))
  }
  test("5") {
    testGenericPrecedence("12+5*30", Prim("+", Lit(12), Prim("*", Lit(5), Lit(30))))
  }
  test("6") {
    testGenericPrecedence("12-5/30", Prim("-", Lit(12), Prim("/", Lit(5), Lit(30))))
  }
  test("7") {
    testGenericPrecedence("2*9*5*3-18/6/3", Prim("-",
      Prim("*", Prim("*", Prim("*", Lit(2), Lit(9)), Lit(5)), Lit(3)),
      Prim("/", Prim("/", Lit(18), Lit(6)), Lit(3))))
  }



  /*test("rt") {
    testGenericPrecedence("18/6/3",Prim("/",Prim("/",Lit(18),Lit(6)),Lit(3)))
  }
  test("go"){
    testGenericPrecedence("2*9*5*3",Prim("*",Prim("*",Prim("*",Lit(2),Lit(9)),Lit(5)), Lit(3)))
  }*/
  test("8") {
    0
  }
  test("9") {
    0
  }
  test("10") {
    0
  }
  test("11") {
    0
  }
  test("12") {
    0
  }
  test("13") {
    0
  }
  test("14") {
    testLetParser("val x=3;x", Let("x", Lit(3), Ref("x")))
  }
  test("15") {
    testLetParser("val x=3;val y=1;x-y", Let("x", Lit(3), Let("y", Lit(1), Prim("-", Ref("x"), Ref("y")))))
    testLetParser("val x=100;val y=1;x*5-y", Let("x", Lit(100),
      Let("y", Lit(1),
        Prim("-", Prim("*", Ref("x"), Lit(5)), Ref("y")))))
  }


  //branch parser
  test("16") {
    testBranchParser("if(3>5){1}else{2}", If(Cond(">", Lit(3), Lit(5)), Lit(1), Lit(2)))
  }
  test("17") {
    testBranchParser("if(3>5){if(9<0){1}else{2}}else{4}", If(Cond(">", Lit(3), Lit(5)),
      If(Cond("<", Lit(9), Lit(0)), Lit(1), Lit(2)),
      Lit(4)))
  }
  test("18") {
    assertThrows[Exception] {
      testBranchParser("if(1+2) 3 else 5", Lit(2))
    }
  }
  test("19") {
    assertThrows[Exception] {
      testBranchParser("if(3>5){1}", If(Cond(">", Lit(3), Lit(5)), Lit(1), Lit(4)))
    }
  }




  //variable parser

  test("20"){
    testVariableParser("var a = 4; a",VarDec("a",Lit(4),Ref("a")))
  }
  test("21"){
    testVariableParser("var a = 4; a=7",VarDec("a",Lit(4),VarAssign("a",Lit(7))))
  }
  test("22"){
    testVariableParser("var a = 4; var b=1; a=b=7",VarDec("a",Lit(4),
      VarDec("b",Lit(1),
        VarAssign("a",VarAssign("b",Lit(7))))))
  }
  test("23"){
    assertThrows[Exception] {
      testVariableParser("var a = 4 a",VarDec("a",Lit(4),Ref("a")))
    }
  }




  //Loop Parser
  test("24"){
    testLoopParser("while(5>4)0;1",While(Cond(">",Lit(5),Lit(4)),Lit(0),Lit(1)))
  }
  test("25"){
    testLoopParser("while(5>4){while(3<6)9;0};1",While(Cond(">",Lit(5),Lit(4)),
      While(Cond("<",Lit(3),Lit(6)),Lit(9),Lit(0)),Lit(1)))
  }
  test("26"){
    assertThrows[Exception] {
      testLoopParser("while(5>4)0;",While(Cond(">",Lit(5),Lit(4)),Lit(0),Lit(1)))
    }
  }
  test("27"){
    testLoopParser("var x=5;while(x>0)x=x-1;x",VarDec("x",Lit(5),
      While(Cond(">",Ref("x"),Lit(0)),VarAssign("x",Prim("-",Ref("x"),Lit(1))),Ref("x"))))
  }
  test("28"){
    testLoopParser("var x=10;var b=1;while(x>0){if(b==5)b else b=b+7};b",VarDec("x",Lit(10),
      VarDec("b",Lit(1),
        While(Cond(">",Ref("x"),Lit(0)),
          If(Cond("==",Ref("b"),Lit(5)),Ref("b"),VarAssign("b",Prim("+",Ref("b"),Lit(7)))),Ref("b")))))
  }


  test("GenericPrecedence1") {
    testGenericPrecedence("-2+3*8-2", Prim("-",Prim("+",Unary("-",Lit(2)),Prim("*",Lit(3),Lit(8))),Lit(2)))
  }

  test("GenericPrecedence2") {
    testGenericPrecedence("-2+((-3 + 3)* 8 - 2)", Prim("+",Unary("-",Lit(2)),Prim("-",Prim("*",Prim("+",Unary("-",Lit(3)),Lit(3)),Lit(8)),Lit(2))))
  }

  test("LetParser1") {
    testLetParser("val x = 2; x * x", Let("x",Lit(2),Prim("*",Ref("x"),Ref("x"))))
  }

  test("valParser2") {
      testLetParser("val x = -2; val y = 3*4; x + y", 
      Let("x",Unary("-",Lit(2)),Let("y",Prim("*",Lit(3),Lit(4)),Prim("+",Ref("x"),Ref("y"))))
    )
  }

  test("BranchParser1") {
    testBranchParser("val x = 2; if (x > 0) { 22 } else 2",
    Let("x",Lit(2),If(Cond(">",Ref("x"),Lit(0)),Lit(22),Lit(2))))
  }

  test("BranhParser2") {
    testBranchParser("val x = -2; val y = 4; if (y>x){ 1 } else 0",
    Let("x",Unary("-",Lit(2)),Let("y",Lit(4),If(Cond(">",Ref("y"),Ref("x")),Lit(1),Lit(0))))
    )
  }

  test("VarParser1") {
    testVariableParser("var x = 2; if (x > 0) { x = 10 } else { x = 22 }",
    VarDec("x",Lit(2),If(Cond(">",Ref("x"),Lit(0)),VarAssign("x",Lit(10)),VarAssign("x",Lit(22))))
    )
  }

  test("VarParser2") {
    testVariableParser("var x = 2; if (x > 0) { x = x + 10 } else { val y = 10; x = y + 19 }",
    VarDec("x",Lit(2),If(Cond(">",Ref("x"),Lit(0)),VarAssign("x",Prim("+",Ref("x"),Lit(10))),Let("y",Lit(10),VarAssign("x",Prim("+",Ref("y"),Lit(19)))))))
  }

  test("LoopParser1") {
    testLoopParser("var x = 2; var y = 0; while(y<1) {var x = x + 1; y = y + 1  }; x",
    VarDec("x",Lit(2),VarDec("y",Lit(0),While(Cond("<",Ref("y"),Lit(1)),VarDec("x",Prim("+",Ref("x"),Lit(1)),VarAssign("y",Prim("+",Ref("y"),Lit(1)))),Ref("x")))))
  }


  test("LoopParser2") {
    testLoopParser("var x = 2; var y = 0; while(y<1) {var x = x + 1; y = y + 1  }; x",
    VarDec("x",Lit(2),VarDec("y",Lit(0),While(Cond("<",Ref("y"),Lit(1)),VarDec("x",Prim("+",Ref("x"),Lit(1)),VarAssign("y",Prim("+",Ref("y"),Lit(1)))),Ref("x")))))
  }
}