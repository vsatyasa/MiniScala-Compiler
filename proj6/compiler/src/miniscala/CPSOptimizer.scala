package miniscala

import scala.collection.mutable.{ Map => MutableMap }
import scala.annotation.varargs
// import scala.annotation.retains

abstract class CPSOptimizer[T <: CPSTreeModule { type Name = Symbol }]
  (val treeModule: T) {
  import treeModule._

  def apply(tree: Tree): Tree = {
    val simplifiedTree = fixedPoint(tree)(shrink)
    val maxSize = (size(simplifiedTree) * 1.5).toInt
    fixedPoint(simplifiedTree, 8) { t => inline(t, maxSize) }
  }

  /* Counts how many times a symbol is encountered as an applied function,
   * and how many as a value
   */
  private case class Count(applied: Int = 0, asValue: Int = 0)

  /* Local state of the optimization
   * Note: To update the state, use the with* methods
   */
  private case class State(
    /* How many times a symbol is encountered in the Tree. Note: The
     * census for the whole program gets calculated once in the
     * beginning, and passed to the initial state.
     */
    census: Map[Name, Count],
    // Name substitution that needs to be applied to the current tree
    subst: Substitution[Name] = Substitution.empty,
    // Names that have a constant value
    lEnv: Map[Name, Literal] = Map.empty,
    // The inverse of lEnv
    lInvEnv: Map[Literal, Name] = Map.empty,
    // A known block mapped to its tag and length
    bEnv: Map[Name, (Literal, Name)] = Map.empty,
    // ((p, args) -> n2) is included in eInvEnv iff n2 == p(args)
    // Note: useful for common-subexpression elimination
    eInvEnv: Map[(ValuePrimitive, Seq[Name]), Name] = Map.empty,
    // Continuations that will be inlined
    cEnv: Map[Name, CntDef] = Map.empty,
    // Functions that will be inlined
    fEnv: Map[Name, FunDef] = Map.empty) {

    // Checks whether a symbol is dead in the current state
    def dead(s: Name): Boolean =
      census get s map (_ == Count(applied = 0, asValue = 0)) getOrElse true
    // Checks whether a symbols is applied exactly once as a function
    // in the current State, and never used as a value
    def appliedOnce(s: Name): Boolean =
      census get s map (_ == Count(applied = 1, asValue = 0)) getOrElse false

    // Addas a substitution to the state
    def withSubst(from: Name, to: Name): State =
      copy(subst = subst + (from -> to))
    // Adds a Seq of substitutions to the state
    def withSubst(from: Seq[Name], to: Seq[Name]): State =
      copy(subst = subst ++ (from zip to))

    // Adds a constant to the State
    def withLit(name: Name, value: Literal) =
      copy(lEnv = lEnv + (name -> value), lInvEnv = lInvEnv + (value -> name))
    // Adds a block to the state
    def withBlock(name: Name, tag: Literal, size: Name) =
      copy(bEnv = bEnv + (name -> (tag, size)))
    // Adds a primitive assignment to the state
    def withExp(name: Name, prim: ValuePrimitive, args: Seq[Name]) =
      copy(eInvEnv = eInvEnv + ((prim, args) -> name))
    // Adds an inlinable continuation to the state
    def withCnt(cnt: CntDef) =
      copy(cEnv = cEnv + (cnt.name -> cnt))
    // Adds a Seq of inlinable continuations to the state
    def withCnts(cnts: Seq[CntDef]) =
      (this /: cnts) (_.withCnt(_))
    // Adds an inlinable function to the state
    def withFun(fun: FunDef) =
      copy(fEnv = fEnv + (fun.name -> fun))
    // Adds a Seq of inlinable functions to the state
    def withFuns(funs: Seq[FunDef]) =
      (this /: funs) (_.withFun(_))
    /*
     * The same state, with emply inverse environments.
     * Use this when entering a new FunDef, because assigned Name's may
     * come out of scope during hoisting.
     */
    def withEmptyInvEnvs =
      copy(lInvEnv = Map.empty, eInvEnv = Map.empty)
  }

  // Shrinking optimizations

  private def shrink(tree: Tree): Tree = {
    def shrinkT(tree: Tree)(implicit s: State): Tree = tree match {

      case LetL(name, value, body) => 
        // check for dead code (no shrinking is needed for value since CPS will expressions)
        // and just do let binding
        if (s.dead(name)) {
          return shrinkT(body)
        }

        // check for constant propagation
        else if (s.lInvEnv.contains(value)) {
          val reDecName = s.lInvEnv(value)
          return shrinkT(body subst Substitution(name, reDecName))(s.withSubst(name, reDecName))
        } 
        
        //  just shrink the body and do let binding
        else {
          return LetL(name, value, shrinkT(body)(s.withLit(name, value)))
        }
      
      case LetP(name, prim, args, body) if prim.isInstanceOf[MiniScalaBlockAlloc] =>
        // map the args with the substitution

        val blockAlloc = prim.asInstanceOf[MiniScalaBlockAlloc]
        val tag = blockAlloc.tag
        val newArgs = args.map(arg => s.subst(arg))

        // add the block to the state
        val newState = s.withBlock(name, IntLit(tag).asInstanceOf[Literal], newArgs(0))
        LetP(name, MiniScalaBlockAlloc(tag).asInstanceOf[ValuePrimitive], newArgs, shrinkT(body)(newState))
      
      case LetP(name, prim , args, body) if prim.isInstanceOf[CPSBlockAlloc] =>

        val blockAlloc = prim.asInstanceOf[CPSBlockAlloc]
        val tag = blockAlloc.tag
        
        // map the args with the substitution
        val newArgs = args.map(arg => s.subst(arg))

        // add the block to the state
        val newState = s.withBlock(name, tag.asInstanceOf[Literal], newArgs(0))
        LetP(name, CPSBlockAlloc(tag).asInstanceOf[ValuePrimitive], newArgs, shrinkT(body)(newState))
      
            
      case LetP(name, prim, args, body) =>
        // change the args with the substitution
        val newArgs = args.map(arg => s.subst(arg))

        if (impure(prim) || unstable(prim)) {
          return LetP(name, prim, newArgs, shrinkT(body))
        }
        
        // check for dead code not special block operations
        if (s.dead(name)) {
          return shrinkT(body)
        }


        // check for common subexpression elimination using the new args
        if (s.eInvEnv.contains((prim, newArgs))) {
          val reDecName = s.eInvEnv((prim, newArgs))
          return shrinkT(body)(s.withSubst(name, reDecName))
        }

        if (args.length == 1 ) {
          val sym1 = newArgs(0)
          if (prim == identity && s.lEnv.contains(sym1)) {
              val value1 = s.lEnv(sym1)
              return shrinkT(body subst Substitution(name, sym1))(s.withSubst(name, sym1))
          
          } else if (prim == identity && s.bEnv.contains(sym1)) {
              return shrinkT(body subst Substitution(name, sym1))(s.withSubst(name, sym1))
          
          } else if (prim == blockTag) {
              val blockName = args(0)
              if (!s.bEnv.contains(blockName)) return LetP(name, blockTag, args, shrinkT(body))
              val tag = s.bEnv.get(blockName).get._1
              return LetL(name, tag, shrinkT(body)(s.withLit(name, tag).withExp(name, blockTag, args)))

          } else if (prim == blockLength) {
              val blockName = args(0)
              if (s.bEnv.contains(blockName)) {
                val blockLengthSym = s.subst(s.bEnv.get(blockName).get._2)
                if (s.lEnv.contains(blockLengthSym)) {
                  val blockLength = s.lEnv(blockLengthSym)
                  // println("blockLength: " + blockLength)
                  return LetL(name, blockLength, shrinkT(body)(s.withLit(name, blockLength)))
                }
                return LetP(name, prim, newArgs, shrinkT(body))
              }
              return LetP(name, prim, newArgs, shrinkT(body))
          }
          
          else {
              return LetP(name, prim, newArgs, shrinkT(body))
          }
        }

        // Do the following
        // 1. Folding (Let Binding)
        // 2. Absorb and Neutralize (using absorbable, Neutral))
        // 3. Apply if same arg reduction can be applied
        if (args.length == 2) {

            // get symbols and values
            val sym1 = newArgs(0)
            val sym2 = newArgs(1)

            // folding
            if (s.lEnv.contains(sym1) && s.lEnv.contains(sym2)) {
              val value1 = s.lEnv(sym1)
              val value2 = s.lEnv(sym2)
             
              val fVal = vEvaluator(prim, Seq(value1, value2))
              return LetL(name, fVal, shrinkT(body)(s.withLit(name, fVal)))
            }

            // Left Absorb and Neutralize
            else if (s.lEnv.contains(sym1)) {
              val value1 = s.lEnv(sym1)
              
              if (leftAbsorbing.contains((value1, prim))) return shrinkT(body subst s.withSubst(name, sym1).subst)(s.withSubst(name, sym1))
              else if (leftNeutral.contains((value1, prim))) return shrinkT(body)(s.withSubst(name, sym2))
              else return LetP(name, prim, newArgs,  shrinkT(body)(s.withExp(name, prim, newArgs)))
            }

            // Right Absorb and Neutralize
            else if (s.lEnv.contains(sym2)) {
              val value2 = s.lEnv(sym2)
              
              if (rightAbsorbing.contains((prim, value2)))  return shrinkT(body subst s.withSubst(name, sym2).subst)(s.withSubst(name, sym2))
              else if (rightNeutral.contains((prim, value2))) return shrinkT(body)(s.withSubst(name, sym1))
              else return LetP(name, prim, newArgs,  shrinkT(body)(s.withExp(name, prim, newArgs)))
            }
            
            // Apply if same arg reduction can be applied
            else if (sym1 == sym2 && sameArgReduce.isDefinedAt(prim)) {
              val reduceVal = sameArgReduce(prim)
              return  LetL(name, reduceVal, shrinkT(body)(s.withExp(name, prim, newArgs)))
            }

            else {
              return LetP(name, prim, newArgs,  shrinkT(body)(s.withExp(name, prim, newArgs)))
            }
        } 
        
        return LetP(name, prim, newArgs, shrinkT(body)(s.withExp(name, prim, newArgs)))
        
      
      case If(cond, Seq(x), ifCont, elseCont)  =>
          val ifContApp = AppC(ifCont, Seq())
          val elseContApp = AppC(elseCont, Seq())
          val arg = s.subst(x)
          if (s.lEnv.contains(arg)) {
            val value = s.lEnv(arg)
            val evalVal = cEvaluator(cond, Seq(value))
            if (evalVal == true) return shrinkT(ifContApp)
            else return shrinkT(elseContApp)
          }
          return If(cond, Seq(x), ifCont, elseCont)
        
      
      case If(cond, args, ifCont, elseCont) =>
        val ifContApp = AppC(ifCont, Seq())
        val elseContApp = AppC(elseCont, Seq())
        val replacedArgs = args.map(arg => {s.subst(arg)})

        // condition evaluation
        val v1 = replacedArgs(0)
        val v2 = replacedArgs(1)

        if (s.lEnv.contains(v1) && s.lEnv.contains(v2) ) {
          val value1 = s.lEnv(v1)
          val value2 = s.lEnv(v2)

          val evalVal = cEvaluator(cond, Seq(value1, value2))
          if (evalVal == true) return shrinkT(ifContApp)
          else return shrinkT(elseContApp)
        
        } else if (v1 == v2) { 
          
          val evalVal = sameArgReduceC(cond)
          if (evalVal == true) return shrinkT(ifContApp)
          else return shrinkT(elseContApp)

        } else {
          return If(cond, replacedArgs, ifCont, elseCont)
        }
      

      case LetF(funs, body) => 
        // for each function do the dead code elemination
        // val nonDeadFuns = funs.filter(fun => !s.dead(fun.name))
        // val nonDeadAppliedOnceFuns = nonDeadFuns.filter(fun => (s.appliedOnce(fun.name)))
        // val nonDeadNonInlinedFuns = nonDeadFuns.filter(fun => (!s.appliedOnce(fun.name)))
        val nonDeadFuns = funs.filter(fun => !s.dead(fun.name))
        val nonDeadAppliedOnceFuns = nonDeadFuns.filter(fun => (s.appliedOnce(fun.name)))
        val nonDeadNonInlinedFuns = nonDeadFuns.filter(fun => (!s.appliedOnce(fun.name)))
        val nonDeadAppliedOnceFunsOptFuns = nonDeadAppliedOnceFuns
        
        // shrinking all the non-inlined funtions
        var nonDeadOptFuns: Seq[FunDef] = Seq[FunDef]()
        for (funDef <- nonDeadNonInlinedFuns) {
          val updatedState = s.withEmptyInvEnvs.withFuns(nonDeadAppliedOnceFunsOptFuns)
          val optFunDef = FunDef(funDef.name, funDef.retC, funDef.args, shrinkT(funDef.body)(updatedState))
          nonDeadOptFuns =  nonDeadOptFuns :+ optFunDef
        }
        // fix the extra LetF generated
        val shrinkedBody = shrinkT(body)(s.withFuns(nonDeadAppliedOnceFunsOptFuns))
        if (nonDeadOptFuns.length == 0) return shrinkedBody
        LetF(nonDeadOptFuns, shrinkedBody)


      case LetC(conts, body) =>
        // for each continuation do the dead code elemination
        val nonDeadConts = conts.filter(cont => !s.dead(cont.name))
        val appliedOnceConts = nonDeadConts.filter(cont => (s.appliedOnce(cont.name)))
        val nonDeadNonInlinedConts = nonDeadConts.filter(cont => (!s.appliedOnce(cont.name)))
        var nonDeadAppliedOnceOptCnts = appliedOnceConts

        var nonDeadOptConts: Seq[CntDef] = Seq[CntDef]()
        for (cntDef <- nonDeadNonInlinedConts) {
          val updatedState = s.withEmptyInvEnvs.withCnts(nonDeadAppliedOnceOptCnts)
          val optCntDef = CntDef(cntDef.name, cntDef.args, shrinkT(cntDef.body)(updatedState))
          nonDeadOptConts =  nonDeadOptConts :+ optCntDef
        }
        
        val shrinkedBody =  shrinkT(body)(s.withCnts(nonDeadAppliedOnceOptCnts))
        if (nonDeadOptConts.length == 0) return shrinkedBody
        return LetC(nonDeadOptConts, shrinkedBody)


      // // call the shrinkT on the body with new set of functions
      case AppC(cont, args) =>
        // replace the args looking up the state
        val replacedArgs = args.map(arg => s.subst(arg))
        val replacedCont = cont

        // check if the constination is just applied once.
        if (s.cEnv.contains(replacedCont)) {
          val newBody = s.cEnv(replacedCont).body
          // val updatedState = s.withEmptyInvEnvs.withSubst(s.cEnv(replacedCont).args, replacedArgs)

          val x = s.withSubst(s.cEnv(replacedCont).args, replacedArgs)
          return shrinkT(newBody subst x.subst)
        } else {
          return AppC(replacedCont, replacedArgs)
        }
      
      case AppF(func, retC, args) => 

        // replace the args looking up the state
        val replacedArgs = args.map(arg => s.subst(arg))
        val fun  = s.subst(func)
        
        // check if the function is just applied once.
        if (s.fEnv.contains(fun)) {
          val funDef = s.fEnv(fun)
          val x = s.withSubst(funDef.args, replacedArgs).withSubst(funDef.retC, retC)
          val shrinked = funDef.body subst x.subst
          return shrinkT(shrinked)(x)
        } 
        
        return AppF(fun, retC, replacedArgs)
        

      case _ =>
        tree
    }

    shrinkT(tree)(State(census(tree)))
  }


  private def GetUniqueNamesForLetFD(t: Tree): Seq[Name] =
    t match {
      case LetF(funs, body) =>
        var fun_names: Seq[Name] = Seq()
        for (fun <- funs) {
          val bodyUniqueNames = GetUniqueNames(fun.body)
          fun_names = fun_names ++ 
            Seq(fun.name) ++ 
            bodyUniqueNames ++
            Seq(fun.retC) ++
            fun.args
        }
        fun_names ++ GetUniqueNames(body) 
      case _ => Seq()
    }
  
  private def GetUniqueNamesForLetCD(t: Tree): Seq[Name] =
    t match {
      case LetC(conts, body) =>
        var cont_names: Seq[Name] = Seq()
        for (cont <- conts) {
          val bodyUniqueNames = GetUniqueNames(cont.body)
          cont_names = cont_names ++ 
            Seq(cont.name) ++ 
            bodyUniqueNames ++ 
            cont.args
        }
        cont_names ++ GetUniqueNames(body) 
      case _ => Seq()
    }
  
  private def GetUniqueNamesForLetLD(t: Tree): Seq[Name] =
    t match {
      case LetL(name, value, body) => 
        Seq(name) ++ GetUniqueNames(body)
      case _ => Seq()
    }
  
  private def GetUniqueNamesForLetPD(t: Tree): Seq[Name] =
    t match {
      case LetP(name, prim, args, body) =>
        Seq(name) ++ GetUniqueNames(body)
      case _ => Seq()
    }

  private def GetUniqueNames(t: Tree): Seq[Name] = {
    GetUniqueNamesForLetFD(t) ++ 
    GetUniqueNamesForLetCD(t) ++ 
    GetUniqueNamesForLetLD(t) ++ 
    GetUniqueNamesForLetPD(t)
  }

  // private def GetUnqNames(t: Tree): Tuple(Seq[Name], Seq[Name]) = {
  //   case LetL(n, v, body) =>
  //     Tuple(Seq(n), Seq(n.copy)) + GetUnqNames(body)
  //   case LetP(n, p, a, body) =>
  //     Tuple(Seq(n), Seq(n.copy)) + GetUnqNames(body)
  //   case _ =>
  //     ()
  // }

  private def inline(tree: Tree, maxSize: Int): Tree = {

    val fibonacci = Seq(1, 2, 3, 5, 8, 13)

    val trees = Stream.iterate((0, tree), fibonacci.length + 1) { case (i, tree) =>
      val funLimit = fibonacci(i)
      val cntLimit = i

      def inlineT(tree: Tree)(implicit s: State): Tree = tree match {

        case LetL(name, value, body) => 
          val updatedState = s.withLit(name, value)
          LetL(name, value, inlineT(body)(updatedState))
        
        case LetP(name, prim, args, body) =>
          val updatedState = s.withExp(name, prim, args)
          LetP(name, prim, args, inlineT(body)(updatedState))

        case LetC(conts, body) =>
          // Get the non dead continuations
          val nonDeadConts = conts.filter(cont => !s.dead(cont.name))
          val toBeInLineConts = nonDeadConts.filter(cont => size(cont.body) <= cntLimit)
          var inlinedConts: Seq[CntDef] = Seq[CntDef]()
          // for ( cntDef <- toBeInLineConts) {
          //   val stateCnts = conts.filter(cnt => cnt != cntDef)
          //   val optCntDef = CntDef(
          //     cntDef.name,
          //     cntDef.args,
          //     inlineT(cntDef.body)(s.withCnts(stateCnts))
          //   )
          //   inlinedConts = inlinedConts :+ optCntDef 
          // }
          LetC(conts, inlineT(body)(s.withCnts(toBeInLineConts)))
        
        case LetF(funs, body) =>
          val nonDeadFuns = funs.filter(fun => !s.dead(fun.name))
          val toBeInLineFuns = nonDeadFuns.filter(fun => size(fun.body) <= funLimit)
          var optimizedFuns: Seq[FunDef] = Seq[FunDef]()
          for (funDef <- toBeInLineFuns) {
            val stateFuns = funs.filter(fun=> fun!=funDef)
            val optFunDef = FunDef(
              funDef.name, 
              funDef.retC, 
              funDef.args, 
              inlineT(funDef.body)(s.withFuns(stateFuns))
            )
            optimizedFuns = optimizedFuns :+ optFunDef
          }
          LetF(funs, inlineT(body)(s.withFuns(optimizedFuns)))

        
        case AppC(cont, args) =>
          // shuld i replace the args here ?
          val newArgs = args.map(arg => s.subst(arg))
          if (s.cEnv.contains(cont)) {
            val cntDef = s.cEnv(cont)
            val boundVars = GetUniqueNames(cntDef.body)         
            val replacedVars = boundVars.map(x => x.copy)
            // should updates be propogated in different steps ?
            // i.e first args and then replacements
            val updatedState = s
              .withSubst(s.cEnv(cont).args, newArgs)
              .withSubst(boundVars, replacedVars)
            
            return inlineT(cntDef.body subst updatedState.subst)
          } 
          return AppC(cont, newArgs)
          
        
        case AppF(func, retC, args) =>
          val newArgs = args.map(arg => s.subst(arg))
          val retCSubst = s.subst(retC)
          if (s.fEnv.contains(func)) {
            val funDef = s.fEnv(func)
            val boundVars = GetUniqueNames(funDef.body)         
            val replacedVars = boundVars.map(x => x.copy)
            val updatedState = s.withSubst(funDef.args, newArgs)
              .withSubst(funDef.retC, retCSubst)
              .withSubst(boundVars, replacedVars)
            
            return inlineT(funDef.body subst updatedState.subst)
          } 
          
          return AppF(func, retC, newArgs)

        case _ =>
          tree
      }

      (i + 1, fixedPoint(inlineT(tree)(State(census(tree))))(shrink))
    }

    trees.takeWhile{ case (_, tree) => size(tree) <= maxSize }.last._2
  }

  // Census computation
  private def census(tree: Tree): Map[Name, Count] = {
    val census = MutableMap[Name, Count]()
    val rhs = MutableMap[Name, Tree]()

    def incAppUse(symbol: Name): Unit = {
      val currCount = census.getOrElse(symbol, Count())
      census(symbol) = currCount.copy(applied = currCount.applied + 1)
      rhs remove symbol foreach addToCensus
    }

    def incValUse(symbol: Name): Unit = {
      val currCount = census.getOrElse(symbol, Count())
      census(symbol) = currCount.copy(asValue = currCount.asValue + 1)
      rhs remove symbol foreach addToCensus
    }

    def addToCensus(tree: Tree): Unit = (tree: @unchecked) match {
      case LetL(_, _, body) =>
        addToCensus(body)
      case LetP(_, _, args, body) =>
        args foreach incValUse; addToCensus(body)
      case LetC(cnts, body) =>
        rhs ++= (cnts map { c => (c.name, c.body) }); addToCensus(body)
      case LetF(funs, body) =>
        rhs ++= (funs map { f => (f.name, f.body) }); addToCensus(body)
      case AppC(cnt, args) =>
        incAppUse(cnt); args foreach incValUse
      case AppF(fun, retC, args) =>
        incAppUse(fun); incValUse(retC); args foreach incValUse
      case If(_, args, thenC, elseC) =>
        args foreach incValUse; incValUse(thenC); incValUse(elseC)
      case Halt(arg) =>
        incValUse(arg)
    }

    addToCensus(tree)
    census.toMap
  }

  private def sameLen(formalArgs: Seq[Name], actualArgs: Seq[Name]): Boolean =
    formalArgs.length == actualArgs.length

  private def size(tree: Tree): Int = (tree: @unchecked) match {
    case LetL(_, _, body) => size(body) + 1
    case LetP(_, _, _, body) => size(body) + 1
    case LetC(cs, body) => (cs map { c => size(c.body) }).sum + size(body)
    case LetF(fs, body) => (fs map { f => size(f.body) }).sum + size(body)
    case AppC(_, _) | AppF(_, _, _) | If(_, _, _, _) | Halt(_) => 1
  }

  // Returns whether a ValuePrimitive has side-effects
  protected val impure: ValuePrimitive => Boolean
  // Returns whether different applications of a ValuePrimivite on the
  // same arguments may yield different results
  protected val unstable: ValuePrimitive => Boolean
  // Extracts the tag from a block allocation primitive
  protected val blockAllocTag: PartialFunction[ValuePrimitive, Literal]
  // Returns true for the block tag primitive
  protected val blockTag: ValuePrimitive
  // Returns true for the block length primitive
  protected val blockLength: ValuePrimitive
  // Returns true for the identity primitive
  protected val identity: ValuePrimitive

  // ValuePrimitives with their left-neutral elements
  protected val leftNeutral: Set[(Literal, ValuePrimitive)]
  // ValuePrimitives with their right-neutral elements
  protected val rightNeutral: Set[(ValuePrimitive, Literal)]
  // ValuePrimitives with their left-absorbing elements
  protected val leftAbsorbing: Set[(Literal, ValuePrimitive)]
  // ValuePrimitives with their right-absorbing elements
  protected val rightAbsorbing: Set[(ValuePrimitive, Literal)]
  // ValuePrimitives with the value equal arguments reduce to
  protected val sameArgReduce: PartialFunction[ValuePrimitive, Literal]
  // TestPrimitives with the (boolean) value equal arguments reduce to
  protected val sameArgReduceC: TestPrimitive => Boolean
  // An evaluator for ValuePrimitives
  protected val vEvaluator: PartialFunction[(ValuePrimitive, Seq[Literal]),
                                            Literal]
  // An evaluator for TestPrimitives
  protected val cEvaluator: PartialFunction[(TestPrimitive, Seq[Literal]),
                                            Boolean]
}

object CPSOptimizerHigh extends CPSOptimizer(SymbolicCPSTreeModule)
    with (SymbolicCPSTreeModule.Tree => SymbolicCPSTreeModule.Tree) {
  import treeModule._

  protected val impure: ValuePrimitive => Boolean =
    Set(MiniScalaBlockSet, MiniScalaByteRead, MiniScalaByteWrite)

  protected val unstable: ValuePrimitive => Boolean = {
    case MiniScalaBlockAlloc(_) | MiniScalaBlockGet | MiniScalaByteRead => true
    case _ => false
  }

  protected val blockAllocTag: PartialFunction[ValuePrimitive, Literal] = {
    case MiniScalaBlockAlloc(tag) => IntLit(tag)
  }
  protected val blockTag: ValuePrimitive = MiniScalaBlockTag
  protected val blockLength: ValuePrimitive = MiniScalaBlockLength

  protected val identity: ValuePrimitive = MiniScalaId

  protected val leftNeutral: Set[(Literal, ValuePrimitive)] =
    Set( (IntLit(0), MiniScalaIntAdd), 
          (IntLit(1), MiniScalaIntMul), 
          (IntLit(~0), MiniScalaIntBitwiseAnd), 
          (IntLit(0), MiniScalaIntBitwiseOr), 
          (IntLit(0), MiniScalaIntBitwiseXOr)
    )

  protected val rightNeutral: Set[(ValuePrimitive, Literal)] =
    Set((MiniScalaIntAdd, IntLit(0)), 
        (MiniScalaIntSub, IntLit(0)), 
        (MiniScalaIntMul, IntLit(1)), 
        (MiniScalaIntMod, IntLit(1)),
        (MiniScalaIntArithShiftLeft, IntLit(0)), 
        (MiniScalaIntArithShiftRight, IntLit(0)),
        (MiniScalaIntBitwiseAnd, IntLit(~0)), 
        (MiniScalaIntBitwiseOr, IntLit(0)), 
        (MiniScalaIntBitwiseXOr, IntLit(0))
      )

  protected val leftAbsorbing: Set[(Literal, ValuePrimitive)] =
    Set(
      (IntLit(0), MiniScalaIntMul), 
      (IntLit(0), MiniScalaIntBitwiseAnd), 
      (IntLit(~0), MiniScalaIntBitwiseOr),
      (IntLit(0), MiniScalaIntArithShiftLeft),
      (IntLit(0), MiniScalaIntArithShiftRight)
    )

  protected val rightAbsorbing: Set[(ValuePrimitive, Literal)] =
    Set(
      (MiniScalaIntMul, IntLit(0)), 
      (MiniScalaIntBitwiseAnd, IntLit(0)), 
      (MiniScalaIntBitwiseOr,  IntLit(~0))
    )

  protected val sameArgReduce: PartialFunction[ValuePrimitive, Literal] =
     Map( MiniScalaIntSub -> IntLit(0), 
          MiniScalaIntDiv -> IntLit(1), 
          MiniScalaIntMod -> IntLit(0), 
          MiniScalaIntBitwiseOr -> IntLit(0)
        )

  protected val sameArgReduceC: PartialFunction[TestPrimitive, Boolean] = {
    case MiniScalaIntLe | MiniScalaIntGe | MiniScalaEq => true
    case MiniScalaIntLt | MiniScalaIntGt | MiniScalaNe => false
  }

  protected val vEvaluator: PartialFunction[(ValuePrimitive, Seq[Literal]),
                                            Literal] = {
    case (MiniScalaIntAdd,  Seq(IntLit(x), IntLit(y))) => IntLit(x + y)
    case (MiniScalaIntSub,  Seq(IntLit(x), IntLit(y))) => IntLit(x - y)
    case (MiniScalaIntMul,  Seq(IntLit(x), IntLit(y))) => IntLit(x * y)
    case (MiniScalaIntDiv,  Seq(IntLit(x), IntLit(y))) if (y != 0) => IntLit(Math.floorDiv(x, y))
    case (MiniScalaIntMod,  Seq(IntLit(x), IntLit(y))) if (y != 0) => IntLit(Math.floorMod(x, y))

    case (MiniScalaIntArithShiftLeft,  Seq(IntLit(x), IntLit(y))) => IntLit(x << y)
    case (MiniScalaIntArithShiftRight, Seq(IntLit(x), IntLit(y))) => IntLit(x >> y)
    case (MiniScalaIntBitwiseAnd, Seq(IntLit(x), IntLit(y))) => IntLit(x & y)
    case (MiniScalaIntBitwiseOr, Seq(IntLit(x), IntLit(y))) => IntLit(x | y)
    case (MiniScalaIntBitwiseXOr, Seq(IntLit(x), IntLit(y))) => IntLit(x ^ y)
  }

  protected val cEvaluator: PartialFunction[(TestPrimitive, Seq[Literal]),
                                            Boolean] = {
    // print("cEvaluator")
    case (MiniScalaIntP, Seq(IntLit(_))) => true
    case (MiniScalaIntP, Seq(_)) => false
    case (MiniScalaCharP, Seq(CharLit(_))) => true
    case (MiniScalaCharP, Seq(_)) => false
    case (MiniScalaBoolP, Seq(BooleanLit(_))) => true
    case (MiniScalaBoolP, Seq(_)) => false
    case (MiniScalaUnitP, Seq(UnitLit)) => true
    case (MiniScalaUnitP, Seq(_)) => false
    case (MiniScalaBlockP, Seq(IntLit(_))) => false

    case (MiniScalaIntLt, Seq(IntLit(x), IntLit(y))) => (x < y)
    case (MiniScalaIntLe, Seq(IntLit(x), IntLit(y))) => (x <= y)
    case (MiniScalaEq, Seq(IntLit(x), IntLit(y))) => (x == y)
    case (MiniScalaNe, Seq(IntLit(x), IntLit(y))) => (x != y)
    case (MiniScalaEq, Seq(BooleanLit(x), BooleanLit(y))) => (x == y)
    case (MiniScalaNe, Seq(BooleanLit(x), BooleanLit(y))) => (x != y)
    case (MiniScalaEq, Seq(CharLit(x), CharLit(y))) => (x == y)
    case (MiniScalaNe, Seq(CharLit(x), CharLit(y))) => (x != y)
    case (MiniScalaIntGe, Seq(IntLit(x), IntLit(y))) => (x >= y)
    case (MiniScalaIntGt, Seq(IntLit(x), IntLit(y))) => (x > y)
  }
}

object CPSOptimizerLow extends CPSOptimizer(SymbolicCPSTreeModuleLow)
    with (SymbolicCPSTreeModuleLow.Tree => SymbolicCPSTreeModuleLow.Tree) {
  import treeModule._

  protected val impure: ValuePrimitive => Boolean =
    Set(CPSBlockSet, CPSByteRead, CPSByteWrite)

  protected val unstable: ValuePrimitive => Boolean = {
    case CPSBlockAlloc(_) | CPSBlockGet | CPSByteRead => true
    case _ => false
  }

  protected val blockAllocTag: PartialFunction[ValuePrimitive, Literal] = {
    case CPSBlockAlloc(tag) => tag
  }
  protected val blockTag: ValuePrimitive = CPSBlockTag
  protected val blockLength: ValuePrimitive = CPSBlockLength

  protected val identity: ValuePrimitive = CPSId

  protected val leftNeutral: Set[(Literal, ValuePrimitive)] =
    Set((0, CPSAdd), (1, CPSMul), (~0, CPSAnd), (0, CPSOr), (0, CPSXOr))

  protected val rightNeutral: Set[(ValuePrimitive, Literal)] =
    Set((CPSAdd, 0), (CPSSub, 0), (CPSMul, 1), (CPSDiv, 1),
        (CPSArithShiftL, 0), (CPSArithShiftR, 0),
        (CPSAnd, ~0), (CPSOr, 0), (CPSXOr, 0))

  protected val leftAbsorbing: Set[(Literal, ValuePrimitive)] =
    Set((0, CPSMul), (0, CPSAnd), (~0, CPSOr))
  protected val rightAbsorbing: Set[(ValuePrimitive, Literal)] =
    Set((CPSMul, 0), (CPSAnd, 0), (CPSOr, ~0))

  protected val sameArgReduce: Map[ValuePrimitive, Literal] =
    Map(CPSSub -> 0, CPSDiv -> 1, CPSMod -> 0, CPSXOr -> 0)

  protected val sameArgReduceC: PartialFunction[TestPrimitive, Boolean] = {
    case CPSLe | CPSGe | CPSEq => true
    case CPSLt | CPSGt | CPSNe => false
  }

  protected val vEvaluator: PartialFunction[(ValuePrimitive, Seq[Literal]),
                                            Literal] = {
    case (CPSAdd, Seq(x, y)) => x + y
    case (CPSSub, Seq(x, y)) => x - y
    case (CPSMul, Seq(x, y)) => x * y
    case (CPSDiv, Seq(x, y)) if (y != 0) => Math.floorDiv(x, y)
    case (CPSMod, Seq(x, y)) if (y != 0) => Math.floorMod(x, y)

    case (CPSArithShiftL, Seq(x, y)) => x << y
    case (CPSArithShiftR, Seq(x, y)) => x >> y
    case (CPSAnd, Seq(x, y)) => x & y
    case (CPSOr, Seq(x, y)) => x | y
    case (CPSXOr, Seq(x, y)) => x ^ y
  }

  protected val cEvaluator: PartialFunction[(TestPrimitive, Seq[Literal]),
                                            Boolean] = {

    case (CPSLt, Seq(x, y)) => x < y
    case (CPSLe, Seq(x, y)) => x <= y
    case (CPSEq, Seq(x, y)) => x == y
    case (CPSNe, Seq(x, y)) => x != y
    case (CPSGe, Seq(x, y)) => x >= y
    case (CPSGt, Seq(x, y)) => x > y
  }
}
