package project3

import java.io._
import org.scalatest._

class ParserTest extends TimedSuite {
  import Language._

  def scanner(src: String) = new Scanner(new BaseReader(src, '\u0000'))

  def testBaseParser(op: String, res: Exp) = {
    val gen = new BaseParser(scanner(op))
    val ast = gen.parseCode

    assert(ast == LetRec(Nil, res), "Invalid result")
  }

  test("SingleDigit") {
    testBaseParser("1", Lit(1))
  }

  test("GenericPrecedence") {
    testBaseParser("2-4*3", Prim("-", List(Lit(2), Prim("*", List(Lit(4), Lit(3))))))
  }

  test("ParseType") {
    testBaseParser("val x: Int = 1; 2", Let("x", IntType, Lit(1), Lit(2)))
  }

  test("ParseOptionalType") {
    testBaseParser("val x = 1; 2", Let("x", UnknownType, Lit(1), Lit(2)))
  }

  // adding _ seperation to avoid conflicts 
  // with testing added by TA for name conflicts
  def test_syntactic_sugar_parser(op: String, res: Exp) = {
    val gen = new SyntacticSugarParser(scanner(op))
    val ast = gen.parseCode
    assert(ast ==  res, "Invalid result")
  }

  test("VarSyntaxSugar") {
    test_syntactic_sugar_parser("var x = 2; x =  x + 1; x", 
    LetRec(List(),VarDec("x",UnknownType,Lit(2),Let("var$1",UnknownType,VarAssign("x",Prim("+",List(Ref("x"), Lit(1)))),Ref("x")))))
  }

  test("IfConditionSyntaxSugar") {
    test_syntactic_sugar_parser("var x = 2; if (x > 2) x = x + 10; x",
    LetRec(List(),VarDec("x",UnknownType,Lit(2),Let("var$1",UnknownType,If(Prim(">",List(Ref("x"), Lit(2))),VarAssign("x",Prim("+",List(Ref("x"), Lit(10)))),Lit(())),Ref("x"))))
    )
  }

  // adding _ seperation to avoid conflicts 
  // with testing added by TA for name conflicts
  def test_function_parser(op: String, res: Exp) = {
    val gen = new FunctionParser(scanner(op))
    val ast = gen.parseCode
    assert(ast.toString ==  res.toString, "Invalid result")
  }


  test("SingleFunctionParser") {
    test_function_parser("def f(i: Int) = i; f(2)", 
      LetRec(List(FunDef("f",List(Arg("i",IntType,Position(0,6,0,6,0,7))),UnknownType,Ref("i"))),App(Ref("f"),List(Lit(2))))
    )
  }

  test("MultipleFunctions") {
    test_function_parser( "def f(i: Int): Int = h(i); def h(i: Int): Int = i * i; f(2)",
        LetRec(List(FunDef("f",List(Arg("i",IntType,Position(0,6,0,6,0,7))),IntType,App(Ref("h"),List(Ref("i")))), FunDef("h",List(Arg("i",IntType,Position(0,33,0,33,0,34))),IntType,Prim("*",List(Ref("i"), Ref("i"))))),App(Ref("f"),List(Lit(2))))
      )
  }


  test("MultipleFunctions2") {
    test_function_parser( "def f(i: Int): Int = h(i); def h(i: Int): Int = i * i; f(2)",
        LetRec(List(FunDef("f",List(Arg("i",IntType,Position(0,6,0,6,0,7))),IntType,App(Ref("h"),List(Ref("i")))), FunDef("h",List(Arg("i",IntType,Position(0,33,0,33,0,34))),IntType,Prim("*",List(Ref("i"), Ref("i"))))),App(Ref("f"),List(Lit(2))))
      )
  }


  test("ComplexTypeFunctionParser1") {
   test_function_parser( "def f(i: Int => Int): Int = h(i); 2",
    LetRec(List(FunDef("f",List(Arg("i",FunType(List(("x$1", IntType)), IntType),Position(0,6,0,6,0,7))), IntType,App(Ref("h"),List(Ref("i"))))),Lit(2))
   )
  }

  test("ComplexTypeFunctionParser") {
   test_function_parser( "def f(i: Int => Int): Int = h(i); 2",
    LetRec(List(FunDef("f",List(Arg("i",FunType(List(("x$1", IntType)), IntType),Position(0,6,0,6,0,7))), IntType,App(Ref("h"),List(Ref("i"))))),Lit(2))
   )
  }

  // adding _ seperation to avoid conflicts 
  // with testing added by TA for name conflicts
  def test_array_parser(op: String, res: Exp) = {
    val gen = new ArrayParser(scanner(op))
    val ast = gen.parseCode
    assert(ast.toString ==  res.toString, "Invalid result")
  }

  test("SingleArrayParser") {
    test_array_parser(
      "val x = new Array[Int](5); x(2) = 5; x(2)",
      LetRec(List(),Let("x",UnknownType,ArrayDec(Lit(5),IntType),Let("var$1",UnknownType,Prim("block-set",List(Ref("x"), Lit(2), Lit(5))),App(Ref("x"),List(Lit(2))))))
    )
  }


  test("SingleArrayParser2") {
    test_array_parser(
      "val x = new Array[Int](5); x(2) = 5; x(2)",
      LetRec(List(),Let("x",UnknownType,ArrayDec(Lit(5),IntType),Let("var$1",UnknownType,Prim("block-set",List(Ref("x"), Lit(2), Lit(5))),App(Ref("x"),List(Lit(2))))))
    )
  }

  test("DoubleArrayParser") {
    test_array_parser(
      "val x = new Array[Int](5); x(2) = 6; x(2)",
      LetRec(List(),Let("x",UnknownType,ArrayDec(Lit(5),IntType),Let("var$1",UnknownType,Prim("block-set",List(Ref("x"), Lit(2), Lit(6))),App(Ref("x"),List(Lit(2))))))
    )
  }


  test("DoubleArrayParser2") {
    test_array_parser(
      "val x = new Array[Int](5); x(2) = 6; x(2)",
      LetRec(List(),Let("x",UnknownType,ArrayDec(Lit(5),IntType),Let("var$1",UnknownType,Prim("block-set",List(Ref("x"), Lit(2), Lit(6))),App(Ref("x"),List(Lit(2))))))
    )
  }




}
