package project3

import org.scalatest._

class SemanticAnalyzerTest extends TimedSuite {
  import Language._

  def astTypeEquals(ast: Exp, tsa: Exp): Boolean = ast == tsa && ast.tp == tsa.tp && { 
    // println("\n\n", ast, ast.tp, tsa, tsa.tp)
    (ast, tsa) match {
    case (Prim(_, args), Prim(_, sgra))=>
      (args zip sgra) forall { case (arg, gra) => astTypeEquals(arg, gra) }
    case (Let(_, _, a, b), Let(_, _, c, d)) =>
      astTypeEquals(a, c) && astTypeEquals(b, d)
    case (If(cond, tBranch, eBranch), If(cond1, tBranch1, eBranch1)) =>
      astTypeEquals(cond, cond1) && astTypeEquals(tBranch, tBranch1) && astTypeEquals(eBranch, eBranch1)
    case (VarDec(_, _, a, b), VarDec(_, _, c, d)) =>
      // println("\n\n", a, a.tp, c, c.tp,"\n\n")
      astTypeEquals(a, c) && astTypeEquals(b, d)
    case (VarAssign(_, rhs), VarAssign(_, shr)) =>
      astTypeEquals(rhs, shr)
    case (While(cond, tBranch, eBranch), While(cond1, tBranch1, eBranch1)) =>
      astTypeEquals(cond, cond1) && astTypeEquals(tBranch, tBranch1) && astTypeEquals(eBranch, eBranch1)
    case (FunDef(_, _, _, fbody), FunDef(_, _, _, fbody1)) =>
      astTypeEquals(fbody, fbody1)
    case (LetRec(funs, body), LetRec(funs1, body1)) =>
      // println(body, body.tp, body1, body1.tp, (body  == body1))
      ((funs zip funs1) forall { case (arg, gra) => astTypeEquals(arg, gra) }) && astTypeEquals(body, body1)
    case (App(fun, args), App(fun1, args1)) =>
      ((args zip args1) forall { case (arg, gra) => astTypeEquals(arg, gra) }) && astTypeEquals(fun, fun1)
    case (ArrayDec(size, _), ArrayDec(size1, _)) =>
      astTypeEquals(size, size1)
    case _ => true
  }}

  def testSemanticAnalyzer(ast: Exp, tsa: Exp, nWarning: Int, nError: Int) = {
    val fakeParser = new Parser(null) {
      override def error(msg: String, pos: Position) = {}
      override def warn(msg: String, pos: Position) = {}
    }

    val analyzer = new SemanticAnalyzer(fakeParser)

    val (tast, w, e) = analyzer.run(ast)
    // print(tast, tast.tp, tsa, tsa.tp)
    assert(w == nWarning, "Incorrect number of Warnings")
    assert(e == nError, "Incorrect number of Errors")
    assert(astTypeEquals(tast, tsa), "AST does not have correct type")
  }

  test("NoErrorNoWarning") {
    testSemanticAnalyzer(Lit(1), Lit(1).withType(IntType), 0, 0)
    testSemanticAnalyzer(Prim("+", List(Lit(1), Lit(2))), Prim("+", List(Lit(1).withType(IntType), Lit(2).withType(IntType))).withType(IntType), 0, 0)
  }

  test("PrimError") {
    testSemanticAnalyzer(Prim(">", List(Lit(1), Lit(1))), Prim(">", List(Lit(1).withType(IntType), Lit(1).withType(IntType))).withType(IntType), 0, 1)
  }

  test("PrimError1") {
    testSemanticAnalyzer(Prim(">=", List(Lit(1), Lit(1))), Prim(">=", List(Lit(1).withType(IntType), Lit(1).withType(IntType))).withType(IntType), 0, 1)
  }

  test("PrimError2") {
    testSemanticAnalyzer(Prim("==", List(Lit(1), Lit(1))), Prim("==", List(Lit(1).withType(IntType), Lit(1).withType(IntType))).withType(IntType), 0, 1)
   }
  
  test("PrimError3") {
    testSemanticAnalyzer(Prim(">", List(Lit(1), Lit(1))), Prim(">", List(Lit(1).withType(IntType), Lit(1).withType(IntType))).withType(IntType), 0, 1)
  }

  test("PrimError4") {
    testSemanticAnalyzer(Prim("<", List(Lit(1), Lit(1))), Prim("<", List(Lit(1).withType(IntType), Lit(1).withType(IntType))).withType(IntType), 0, 1)
    testSemanticAnalyzer(Prim("<=", List(Lit(1), Lit(1))), Prim("<=", List(Lit(1).withType(IntType), Lit(1).withType(IntType))).withType(IntType), 0, 1)
  }

  test("typeAssignmentMisMatchError") {
    testSemanticAnalyzer(LetRec(List(),VarDec("x",BooleanType,Lit(20),Ref("x"))), 
    LetRec(List(),VarDec("x",BooleanType,Lit(20).withType(BooleanType),Ref("x").withType(IntType)).withType(IntType)).withType(IntType),0, 2)
  }

  test("DuplicateFunctionArgs") {
    testSemanticAnalyzer(
      LetRec(
        List(FunDef("f",List(Arg("x",IntType,Position(0,6,0,6,0,7)), Arg("x",IntType,Position(0,13,0,14,0,15))),UnknownType,Lit(5)).withType(FunType(List(("x",IntType), ("x",IntType)),IntType) )),
          App(Ref("f"),List(Lit(2), Lit(2)))) ,
      LetRec(
        List(
          FunDef("f",
            List(
              Arg("x",IntType,Position(0,6,0,6,0,7)), 
              Arg("x",IntType,Position(0,13,0,14,0,15))),
              IntType,
              Lit(5).withType(IntType))
                .withType(FunType(List(("x",IntType), ("x",IntType)),IntType)
          )),
            App(Ref("f").withType(FunType(List(("x",IntType), ("x",IntType)),IntType)),List(Lit(2).withType(IntType), Lit(2).withType(IntType))).withType(IntType)).withType(IntType), 0, 1
    )
  }

  test("DuplicateFunctionArgs2") {
    testSemanticAnalyzer(
      LetRec(
        List(FunDef("f",List(Arg("x",IntType,Position(0,6,0,6,0,7)), Arg("x",IntType,Position(0,13,0,14,0,15))),UnknownType,Lit(5)).withType(FunType(List(("x",IntType), ("x",IntType)),IntType) )),
          App(Ref("f"),List(Lit(2), Lit(2)))) ,
      LetRec(
        List(
          FunDef("f",
            List(
              Arg("x",IntType,Position(0,6,0,6,0,7)), 
              Arg("x",IntType,Position(0,13,0,14,0,15))),
              IntType,
              Lit(5).withType(IntType))
                .withType(FunType(List(("x",IntType), ("x",IntType)),IntType)
          )),
            App(Ref("f").withType(FunType(List(("x",IntType), ("x",IntType)),IntType)),List(Lit(2).withType(IntType), Lit(2).withType(IntType))).withType(IntType)).withType(IntType), 0, 1
    )
  }

  test("DuplicateFunctions") {
    testSemanticAnalyzer (
      LetRec(
        List(
          FunDef("f",List(),UnknownType,Lit(5)).withType(FunType(List(),IntType)), 
          FunDef("f",List(),UnknownType,Lit(10)).withType(FunType(List(),IntType))
        ),
        App(Ref("f"),List())
      ),

      LetRec(
        List(
          FunDef("f",List(),IntType,Lit(5).withType(IntType)).withType(FunType(List(),IntType)), 
          FunDef("f",List(),IntType,Lit(10).withType(IntType)).withType(FunType(List(),IntType))
        ),
        App(Ref("f").withType(FunType(List(),IntType)),List()).withType(IntType)
      ).withType(IntType),
      0,
      1
    )
  }

  test("DuplicateFunctions2") {
    testSemanticAnalyzer (
      LetRec(
        List(
          FunDef("g",List(),UnknownType,Lit(5)).withType(FunType(List(),IntType)), 
          FunDef("g",List(),UnknownType,Lit(10)).withType(FunType(List(),IntType))
        ),
        App(Ref("g"),List())
      ),

      LetRec(
        List(
          FunDef("g",List(),IntType,Lit(5).withType(IntType)).withType(FunType(List(),IntType)), 
          FunDef("g",List(),IntType,Lit(10).withType(IntType)).withType(FunType(List(),IntType))
        ),
        App(Ref("g").withType(FunType(List(),IntType)),List()).withType(IntType)
      ).withType(IntType),
      0,
      1
    )
  }



  test("ArrayDecAndGet") {
    testSemanticAnalyzer (
      LetRec(
        List(),
        Let("x",UnknownType,ArrayDec(Lit(5),IntType),
          Let("var$1",UnknownType,
            Prim("block-set",List(Ref("x"), Lit(1), Lit(5))),
            App(Ref("x"),List(Lit(1)))))),
        LetRec(
          List(),
          Let( "x", ArrayType(IntType), ArrayDec(Lit(5).withType(IntType),IntType).withType(ArrayType(IntType)),
            Let("var$1",UnitType,
              Prim("block-set",List(Ref("x").withType(ArrayType(IntType)), Lit(1).withType(IntType), Lit(5).withType(IntType))).withType(UnitType),
              Prim("block-get",List(Ref("x").withType(ArrayType(IntType)), Lit(1).withType(IntType))).withType(IntType)).withType(IntType)
          ).withType(IntType)).withType(IntType),
      0,
      0
    )
  }

    test("ArrayDecAndGet2") {
    testSemanticAnalyzer (
      LetRec(
        List(),
        Let("x",UnknownType,ArrayDec(Lit(5),IntType),
          Let("var$1",UnknownType,
            Prim("block-set",List(Ref("x"), Lit(1), Lit(5))),
            App(Ref("x"),List(Lit(1)))))),
        LetRec(
          List(),
          Let( "x", ArrayType(IntType), ArrayDec(Lit(5).withType(IntType),IntType).withType(ArrayType(IntType)),
            Let("var$1",UnitType,
              Prim("block-set",List(Ref("x").withType(ArrayType(IntType)), Lit(1).withType(IntType), Lit(5).withType(IntType))).withType(UnitType),
              Prim("block-get",List(Ref("x").withType(ArrayType(IntType)), Lit(1).withType(IntType))).withType(IntType)).withType(IntType)
          ).withType(IntType)).withType(IntType),
      0,
      0
    )
  }
  
  test("IfCondition") {
    testSemanticAnalyzer (
      LetRec(
        List(),
        VarDec("x",UnknownType,
            If(Prim(">",List(Lit(6), Lit(2))),Lit(3),Lit(2)),Ref("x"))
        ),
        LetRec(
        List(),
        VarDec("x", IntType,
            If(
              Prim(">",List(Lit(6).withType(IntType), Lit(2).withType(IntType))).withType(BooleanType),
              Lit(3).withType(IntType),Lit(2).withType(IntType)).withType(IntType),
              Ref("x").withType(IntType)).withType(IntType)
         ).withType(IntType)
        , 0, 0 
      )
  }

  test("IfCondition2") {
    testSemanticAnalyzer (
      LetRec(
        List(),
        VarDec("y",UnknownType,
            If(Prim(">",List(Lit(6), Lit(2))),Lit(3),Lit(2)),Ref("y"))
        ),
        LetRec(
        List(),
        VarDec("y", IntType,
            If(
              Prim(">",List(Lit(6).withType(IntType), Lit(2).withType(IntType))).withType(BooleanType),
              Lit(3).withType(IntType),Lit(2).withType(IntType)).withType(IntType),
              Ref("y").withType(IntType)).withType(IntType)
         ).withType(IntType)
        , 0, 0 
      )
  }

}
