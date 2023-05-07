package miniscala

import miniscala.{ SymbolicCMScalaTreeModule => S }
import miniscala.{ SymbolicCPSTreeModule => C }

object CMScalaToCPSTranslator extends (S.Tree => C.Tree) {
  def apply(tree: S.Tree): C.Tree = {
    nonTail(tree){_ =>
      val z = Symbol.fresh("c0")
      C.LetL(z, IntLit(0), C.Halt(z))
    }(Set.empty)
  }

  private def nonTail(tree: S.Tree)(ctx: Symbol=>C.Tree)(implicit mut: Set[Symbol]): C.Tree = {
    // @unchecked to avoid bogus compiler warnings
    (tree: @unchecked) match {
      case S.Let(name, _, value, body) =>
        nonTail(value)(v =>
            C.LetP(name, MiniScalaId, Seq(v), nonTail(body)(ctx)))

      // Reference of an immutable variable
      // [[n]] C = C[n] 
      case S.Ref(name) if !mut(name) =>
        ctx(name)

      case S.Prim(op , args) =>
        op match {
          case op: MiniScalaValuePrimitive =>
            val n = Symbol.fresh("c0")
            val ope = op.asInstanceOf[C.ValuePrimitive] 
            nonTail_*(args)(pArgs => C.LetP(n, op, pArgs, ctx(n)))
          case _ =>
               nonTail(S.If((tree), S.Lit(BooleanLit(true)), S.Lit(BooleanLit(false))))(ctx)
        }

      
      // vall n = l; C[n]
      case S.Lit(l) =>
        // here do we check for individual cases ?
        val n = Symbol.fresh("c0")
        C.LetL(n, l, ctx(n))
      
      // // [[val n1  = e1; e]] C = 
      // // [[e1]] (v => vallp n1 = id(v); [[e]] C)
      // case S.Let(x, _, a, b) => 
      //   nonTail(a)(aSym => 
      //     C.LetP(x, MiniScalaId, Seq(aSym), nonTail(b)(ctx)))
      
      case S.VarDec(name, _ , rhs, body) =>
        val s = Symbol.fresh("c0")
        val z = Symbol.fresh("c0")
        val d = Symbol.fresh("c0")
        val size = IntLit(1)
        val index = IntLit(0)

        val zDef = C.LetL(
          z, index, 
          nonTail(rhs)(v => C.LetP(d, MiniScalaBlockSet, Seq(name, z, v), nonTail(body)(ctx)(mut + name)))        
        )
        C.LetL(s, size, C.LetP( name, MiniScalaBlockAlloc(242), Seq(s), zDef))

      case S.VarAssign(name, rhs) =>
        // This should be based on block-get based
        // on rules specified
        val z = Symbol.fresh("z")
        val d = Symbol.fresh("d")
        C.LetL(z, IntLit(0), nonTail(rhs) (rhsSym =>
          C.LetP(d, MiniScalaBlockSet, Seq(name, z, rhsSym), ctx(d))
        ))

      // [[name]] C
      // vall z = 0
      // vallp v = blockget(name, z); C[z]
      case S.Ref(name) => // if mut(name) =>
        val z = Symbol.fresh("c0")
        val v = Symbol.fresh("c0")
        C.LetL(z, IntLit(0), 
          C.LetP(v, MiniScalaBlockGet, Seq(name, z), ctx(v))
        )

      case S.While(cd , lBody, body) => 
        val loop = Symbol.fresh("loop")
        val c = Symbol.fresh("c")
        val ct = Symbol.fresh("ct")
        val f = Symbol.fresh("f")
        val falseLit = BooleanLit(false)
        val d = Symbol.fresh("d")

        // print("ctx" + ctx + "\n")
        val cDef = C.CntDef(c, Seq(), nonTail(body)(ctx))
        val ctDef = C.CntDef(ct, Seq(), tail(lBody, loop)) // call tail for optimization

        val loopBody = C.LetC(Seq(cDef), 
          C.LetC(Seq(ctDef),
            cond(cd, ct, c)
          )
        )
        
        val loopDef = C.CntDef(loop, Seq(Symbol.fresh("d")), loopBody)
        C.LetC(Seq(loopDef), 
          C.LetL(d, UnitLit, C.AppC(loop, Seq(d)))
        )

      case S.LetRec(funs, body) => 
        val nFuns = funs.map(fun => {
          val retC = Symbol.fresh("cF")
          C.FunDef(
            fun.name, 
            retC, 
            fun.args.map(arg =>arg.name), 
            tail(fun.body, retC)) // call tail for optimization
        })
        C.LetF(nFuns, nonTail(body)(ctx))

      case S.App(fun, _, args) =>
        val c = Symbol.fresh("c")
        val r = Symbol.fresh("r")

        nonTail(fun)(v => nonTail_*(args)(vSyms => 
          C.LetC(Seq(C.CntDef(c, Seq(r), ctx(r))), 
            C.AppF(v, c, vSyms))  
        ))
      
      //  Probably amend this for 
      //  cond optimized case
      case S.If(cd, tBranch, eBranch) =>
        val c = Symbol.fresh("c")
        val ct = Symbol.fresh("ct")
        val cf = Symbol.fresh("cf")
        var r = Symbol.fresh("r")

        val cDef = C.CntDef(c, Seq(r), ctx(r))
        val ctDef = C.CntDef(ct, Seq(), tail(tBranch, c))
        val cfDef = C.CntDef(cf, Seq(), tail(eBranch, c))

        // C.LetC(Seq(cDef, ctDef, cfDef), 
        //   cond(cd, ct, cf)
        // )
        // cond(cd, ct, cf)       
        
        C.LetC(Seq(cDef), 
          C.LetC(Seq(ctDef),
            C.LetC(Seq(cfDef), 
              cond(cd, ct, cf)
            )
          )
        )



    }
  }
  
  // nonTail_* takes a sequence of S.Tree, and a continuation that takes a
  // sequence of symbols.  The sequence of symbols in the continuation
  // represents the transformed result of `trees`.  This is particularly useful
  // for the App case in nonTail.
  private def nonTail_*(trees: Seq[S.Tree])(ctx: Seq[Symbol]=>C.Tree)(implicit mut: Set[Symbol]): C.Tree =
    trees match {
      case Seq() => 
        ctx(Seq())
      case t +: ts =>
        nonTail(t)(tSym => nonTail_*(ts)(tSyms => ctx(tSym +: tSyms)))
    }

  private def tail(tree: S.Tree, c: Symbol)(implicit mut: Set[Symbol]): C.Tree = {
    // @unchecked to avoid bogus compiler warnings
    (tree: @unchecked) match {
      case S.Let(name, _, value, body) =>
        nonTail(value)(v =>
          C.LetP(name, MiniScalaId, Seq(v), tail(body, c)))

      // Reference of an immutable variable
      // [[n]] C = C[n] 
      case S.Ref(name) if !mut(name) =>
        C.AppC(c, Seq(name))

      // What about other Prim cases that flow through ?
      case S.Prim(op , args) =>
        op match {
          case op: MiniScalaValuePrimitive =>
            val n = Symbol.fresh("c0")
            val ope = op.asInstanceOf[C.ValuePrimitive] 
            nonTail_*(args)(pArgs => C.LetP(n, ope, pArgs, C.AppC(c, Seq(n))))
          case _ =>
              tail(S.If((tree), S.Lit(BooleanLit(true)), S.Lit(BooleanLit(false))) , c)
        }


      case S.Lit(l) =>
        // here do we check for individual cases ?
        val n = Symbol.fresh("c0")
        C.LetL(n, l, C.AppC(c, Seq(n)))
      
      case S.VarDec(name, _ , rhs, body) =>
        val s = Symbol.fresh("c0")
        val z = Symbol.fresh("c0")
        val d = Symbol.fresh("c0")
        val size = IntLit(1)
        val index = IntLit(0)

        val zDef = C.LetL(
          z, index, 
          nonTail(rhs)(v => C.LetP(d, MiniScalaBlockSet, Seq(name, z, v), tail(body , c)(mut + name)))        
        )
        C.LetL(s, size, C.LetP( name, MiniScalaBlockAlloc(242), Seq(s), zDef))      

      case S.VarAssign(name, rhs) =>
        val z = Symbol.fresh("c0")
        val d = Symbol.fresh("c0")
        C.LetL(z, IntLit(0), nonTail(rhs) (rhsSym =>
          C.LetP(d, MiniScalaBlockSet, Seq(name, z, rhsSym), C.AppC(c, Seq(d)))
        ))
      
      case S.Ref(name) => // if mut(name) =>
        val z = Symbol.fresh("c0")
        val v = Symbol.fresh("c0")
        C.LetL(z, IntLit(0), 
          C.LetP(v, MiniScalaBlockGet, Seq(name, z), C.AppC(c, Seq(v)))
        )        

      case S.While(cd , lBody, body) => 
        val loop = Symbol.fresh("loop")
        // val c = Symbol.fresh("c")
        val cDefSym = Symbol.fresh("c")
        val ct = Symbol.fresh("ct")
        val f = Symbol.fresh("f")
        val falseLit = BooleanLit(false)
        val d = Symbol.fresh("d")

                // print("ctx" + ctx + "\n")
        val cDef = C.CntDef(cDefSym, Seq(), tail(body, c))
        val ctDef = C.CntDef(ct, Seq(), tail(lBody ,loop))

        val loopBody = C.LetC(Seq(cDef), 
          C.LetC(Seq(ctDef),
            cond(cd, ct, cDefSym)
          )
        )
        
        val loopDef = C.CntDef(loop,Seq(Symbol.fresh("d")), loopBody)
        C.LetC(Seq(loopDef), 
          C.LetL(d, UnitLit, C.AppC(loop, Seq(d)))
        )

      case S.App(fun, ptps, args) =>

        // pass the same c which comes as a param
        nonTail(fun)(v => nonTail_*(args)(vSyms => 
            C.AppF(v, c, vSyms)
          )  
        )  

      case S.LetRec(funs, body) => 
        val nFuns = funs.map(fun => {
          val retC = Symbol.fresh("cF")
          C.FunDef(
            fun.name, 
            retC, 
            fun.args.map(arg =>arg.name), 
            tail(fun.body, retC))
        })
        C.LetF(nFuns, tail(body, c))
      
      case S.If(cd, tBranch, eBranch) =>
        val ct = Symbol.fresh("ct")
        val cf = Symbol.fresh("cf")

        val ctDef = C.CntDef(ct, Seq(), tail(tBranch, c))
        val cfDef = C.CntDef(cf, Seq(), tail(eBranch, c))

        // C.LetC(Seq(ctDef, cfDef), 
        //   cond(cd, ct, cf)
        // )

        // Bug Fixed by adding single LetC for each
        C.LetC(Seq(ctDef),
          C.LetC(Seq(cfDef), 
            cond(cd, ct, cf)
          )
        )
      
    }
  }

  private def cond(tree: S.Tree, trueC: Symbol, falseC: Symbol)(implicit mut: Set[Symbol]): C.Tree = {
    def litToCont(l: CMScalaLiteral): Symbol =
      if (l != BooleanLit(false)) trueC else falseC

    tree match {
      case S.If(condE, S.Lit(tl), S.Lit(fl)) =>
        cond(condE, litToCont(tl), litToCont(fl))

      // conditionals with atleast one constant branch
      // constant on the else part
      case S.If(condE, ifExp, S.Lit(lit)) =>
        val ic = Symbol.fresh("ic")
        val icDef = C.CntDef(ic, Seq(), cond(ifExp, trueC, falseC))
        C.LetC(Seq(icDef), cond(condE, ic, litToCont(lit)))

      // constant on the if part
      case S.If(condE, S.Lit(lit), elseExp) =>
        val ec = Symbol.fresh("ec")
        val ecDef = C.CntDef(ec, Seq(), cond(elseExp, trueC, falseC))
        C.LetC(Seq(ecDef), cond(condE, litToCont(lit), ec))

      case S.Prim(p: MiniScalaTestPrimitive, args) =>
        nonTail_*(args)(as => C.If(p, as, trueC, falseC))

      case other =>
        nonTail(other)(o =>
          nonTail(S.Lit(BooleanLit(false)))(n =>
            C.If(MiniScalaNe, Seq(o, n), trueC, falseC)))
    }
  }

  // Helper function for defining a continuation.
  // Example:
  // tempLetC("c", Seq(r), ctx(r))(k => App(f, k, as))
  private def tempLetC(cName: String, args: Seq[C.Name], cBody: C.Tree)
                      (body: C.Name=>C.Tree): C.Tree = {
    val cSym = Symbol.fresh(cName)
    C.LetC(Seq(C.CntDef(cSym, args, cBody)), body(cSym))
  }
}
