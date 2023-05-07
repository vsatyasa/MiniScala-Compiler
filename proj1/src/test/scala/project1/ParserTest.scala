package project1

import java.io._
import org.scalatest._

class ParserTest extends TimedSuite {

  def reader(src: String) = new BaseReader(src.iterator, '\u0000')
  def runner(src: String, gData: Map[Char,Int] = Map()) = new ASMRunner(src, gData)

  test("SingleDigit") {
    val gen = new SingleDigitParser(reader("4"))
    val ast = gen.parseCode

    assert(ast == Lit(4), "Invalid result")
  }

  // Function Helper for SingleAddOpParser
  def testSingleAdd(op: String, res: Exp) = {
    val gen = new SingleAddOpParser(reader(op))
    val ast = gen.parseCode

    assert(ast == res, "Invalid result")
  }

  test("SingleAddopAdd") {
    testSingleAdd("1+1", Plus(Lit(1),Lit(1)))
  }

  // Function Helper for MultipleAddOpParser
  def testMultipleAdd(op: String, res: Exp) = {
    val gen = new MultipleAddOpParser(reader(op))
    val ast = gen.parseCode

    assert(ast == res, "Invalid result")
  }

  test("MultipleAddopAdd") {
    testMultipleAdd("1", Lit(1))
    testMultipleAdd("1+2", Plus(Lit(1), Lit(2)))
    testMultipleAdd("1+2+3", Plus(Plus(Lit(1), Lit(2)),Lit(3)))
  }

  test("MultipleAddopSub") {
    testMultipleAdd("1-2", Minus(Lit(1), Lit(2)))
    testMultipleAdd("1+2-3", Minus(Plus(Lit(1),Lit(2)),Lit(3)))
    testMultipleAdd("1-2-3", Minus(Minus(Lit(1),Lit(2)),Lit(3)))
  }

  // Function Helper for ArithOpParser
  def testArith(op: String, res: Exp) = {
    val gen = new ArithOpParser(reader(op))
    val ast = gen.parseCode

    assert(ast == res, "Invalid result")
  }

  test("ArithOpMulop") {
    testArith("1", Lit(1))
    testArith("1*2", Times(Lit(1), Lit(2)))
    testArith("1/2", Div(Lit(1), Lit(2)))
  }

  test("ArithOpOperatorPercedence") {
    testArith("1+2*3", Plus(Lit(1),Times(Lit(2),Lit(3))))
    testArith("3+4/2+3", Plus(Plus(Lit(3),Div(Lit(4),Lit(2))),Lit(3)))
  }

  test("ArithOpLeftAssociativity") {
    testArith("3*4/2", Div(Times(Lit(3),Lit(4)),Lit(2)))
    testArith("8/9*4/2", Div(Times(Div(Lit(8),Lit(9)),Lit(4)),Lit(2)))
    testArith("8+9+4*2", Plus(Plus(Lit(8),Lit(9)),Times(Lit(4),Lit(2))))
  }

  // Function Helper for ArithParOpParser
  def testArithPar(op: String, res: Exp) = {
    val gen = new ArithParOpParser(reader(op))
    val ast = gen.parseCode

    assert(ast == res, "Invalid result")
  }

  test("ArithParNegLiteral") {
    testArithPar("-1", Minus(Lit(0),Lit(1)))
    testArithPar("-1+5*2", Plus(Minus(Lit(0),Lit(1)),Times(Lit(5),Lit(2))))
  }

  test("ArithParParenthesis") {
    testArithPar("(-1)", Minus(Lit(0),Lit(1)))
    testArithPar("2*(1+3+4)", Times(Lit(2),Plus(Plus(Lit(1),Lit(3)),Lit(4))))
    testArithPar("2*(1+3/4)", Times(Lit(2),Plus(Lit(1),Div(Lit(3),Lit(4)))))
  }

  test("ArithParLargeExp") {
    testArithPar("-2*(1*3/4)+6*2", Plus(Times(Minus(Lit(0),Lit(2)),Div(Times(Lit(1),Lit(3)),Lit(4))),Times(Lit(6),Lit(2))))
  }

}
