package miniscala

import BitTwiddling.bitsToIntMSBF
import miniscala.{ SymbolicCPSTreeModule => H }
import miniscala.{ SymbolicCPSTreeModuleLow => L }

/**
 * Value-representation phase for the CPS language. Translates a tree
 * with high-level values (blocks, integers, booleans, unit) and
 * corresponding primitives to one with low-level values (blocks
 * and integers only) and corresponding primitives.
 *
 * @author Michel Schinz <Michel.Schinz@epfl.ch>
 */

object CPSValueRepresenter extends (H.Tree => L.Tree) {
  def apply(tree: H.Tree): L.Tree =
    transform(tree)(Map.empty)


  // declare required bit masks
  val unitLit = bitsToIntMSBF(0, 0, 1, 0)
  val trueLit = bitsToIntMSBF(1, 1, 0, 1, 0)
  val falseLit = bitsToIntMSBF(0, 1, 0, 1, 0)
  val charbitMask = bitsToIntMSBF(1, 1, 0)
  val intBitMask = 1
  val boolBitMask = bitsToIntMSBF(1, 0, 0, 1)
  val optimized = false

  private def transform(tree: H.Tree)
                       (implicit worker: Map[Symbol, (Symbol, Seq[Symbol])])
      : L.Tree = tree match {

    // Literals
    case H.LetL(name, IntLit(value), body) =>
      val encodedVal = (value << 1) + 1
      L.LetL(name, encodedVal, transform(body))
    
    case H.LetL(name, CharLit(value), body) =>
      val charEncodedVal = (value << 3) | charbitMask
      L.LetL(name, charEncodedVal, transform(body))

    case H.LetL(name, BooleanLit(true), e) => 
      L.LetL(name, trueLit, transform(e))
    
    case H.LetL(name, BooleanLit(false), e) =>
      L.LetL(name, falseLit, transform(e))

    case H.LetL(name, UnitLit, body) =>
      L.LetL(name, unitLit, transform(body))

    // TODO: Add missing literals

    // *************** Primitives ***********************
    // Make sure you implement all possible primitives
    // (defined in MiniScalaPrimitives.scala)
    //
    // Integer primitives
    case H.LetP(name, MiniScalaId, Seq(x), body) =>
      L.LetP(name, CPSId, Seq(x), transform(body))

    case H.LetP(name, MiniScalaIntAdd, args, body) =>
      tempLetP(CPSAdd, args) { r =>
        tempLetL(1) { c1 =>
          L.LetP(name, CPSSub, Seq(r, c1), transform(body)) } }

    case H.LetP(name, MiniScalaIntSub, args, body) =>
      tempLetP(CPSSub , args) { r =>
        tempLetL(1) { c1 =>
          L.LetP(name, CPSAdd, Seq(r, c1), transform(body)) } }
  
    case H.LetP(name, MiniScalaIntMul, args, body) =>
      tempLetL(1) { c1 =>
        tempLetP(CPSSub, Seq(args(0), c1)) { nMinusOne => 
          tempLetP(CPSArithShiftR, Seq(args(1), c1)) { mShifted => 
            tempLetP(CPSMul, Seq(nMinusOne, mShifted)) { mulResul =>
              L.LetP(name, CPSAdd, Seq(mulResul, c1), transform(body))
            }
          }
        }  
      }
    
    // Just a similar derivation 
    // as that of division
    case H.LetP(name, MiniScalaIntDiv, args, body) => 
      tempLetL(1) { c1 =>
        tempLetP(CPSSub, Seq(args(0), c1)) { nMinusOne => 
          tempLetP(CPSArithShiftR, Seq(args(1), c1)) { mRiShiftedByOne => 
            tempLetP(CPSDiv, Seq(nMinusOne, mRiShiftedByOne)) { divRes => 
              L.LetP(name, CPSAdd, Seq(divRes, c1), transform(body))
            }
          }
        }
      }
    
    
    // brute force mod op
    case H.LetP(name, MiniScalaIntMod, args, body) => 
      tempLetL(1) { c1 =>
        tempLetP(CPSArithShiftR, Seq(args(0), c1)) { nRiShiftedByOne => 
          tempLetP(CPSArithShiftR, Seq(args(1), c1)) { mRiShiftedByOne => 
            tempLetP(CPSMod, Seq(nRiShiftedByOne, mRiShiftedByOne)) { modRes => 
              tempLetP(CPSArithShiftL, Seq(modRes, c1)) { modResLeShifted => 
                L.LetP(name, CPSAdd, Seq(modResLeShifted, c1), transform(body))
              }
            }
          }
        }
      }
    
    // brute force shift op
    // [[ n << m]] = (((n >> 1) << (m >> 1)) << 1) + 1
    case H.LetP(name, MiniScalaIntArithShiftLeft, args, body) => 
      tempLetL(1) { c1 =>
        tempLetP(CPSArithShiftR, Seq(args(0), c1)) { nRiShiftedByOne =>
          tempLetP(CPSArithShiftR, Seq(args(1), c1)) { mRiShiftedByOne =>
            tempLetP(CPSArithShiftL, Seq(nRiShiftedByOne, mRiShiftedByOne)) { nLeShiftedByM =>
              tempLetP(CPSArithShiftL, Seq(nLeShiftedByM, c1)) { nLeShiftedByMLeShiftedByOne =>
                L.LetP(name, CPSAdd, Seq(nLeShiftedByMLeShiftedByOne, c1), transform(body))
              }
            }
          }
        }
      }
    
    // [[ n >> m]] = (((n >> 1) >> (m >> 1)) << 1) + 1
    case H.LetP(name, MiniScalaIntArithShiftRight, args, body) => 
      tempLetL(1) { c1 => 
        tempLetP(CPSArithShiftR, Seq(args(0), c1)) { nRiShiftedByOne => 
          tempLetP(CPSArithShiftR, Seq(args(1), c1)) { mRiShiftedByOne => 
            tempLetP(CPSArithShiftR, Seq(nRiShiftedByOne, mRiShiftedByOne)) { nRiShiftedByM => 
              tempLetP(CPSArithShiftL, Seq(nRiShiftedByM, c1)) { nRiShiftedByMLeShiftedByOne => 
                L.LetP(name, CPSAdd, Seq(nRiShiftedByMLeShiftedByOne, c1), transform(body))
              }
            }
          }
        }
      }

    
    case H.LetP(name, MiniScalaIntBitwiseAnd, args, body) => 
        L.LetP(name, CPSAnd, args, transform(body)) 
    
    case H.LetP(name, MiniScalaIntBitwiseOr, args, body) => 
        L.LetP(name, CPSOr, args, transform(body)) 
    

    // Just the last bit will be unset
    // since 1 ^ 1  = 1
    // set the last bit adding 1 at the end
    case H.LetP(name, MiniScalaIntBitwiseXOr, args, body) => 
      tempLetL(1) { c1 => 
        tempLetP(CPSXOr, args) { r => 
          {
            L.LetP(name, CPSAdd, Seq(r, c1), transform(body))
          }
        }
      }
    

    // TODO: Add missing integer primitives

    // Block primitives
    // TODO: Add block primitives
    
    // valp n = block-alloc(n); e => valp n = block-alloc(n >> 1); e
    case H.LetP(name, MiniScalaBlockAlloc(tag), Seq(n), body) => 
      tempLetL(1) { c1 => 
        tempLetP(CPSArithShiftR, Seq(n, c1)) { nShifted => 
          {
            L.LetP(name, CPSBlockAlloc(tag), Seq(nShifted), transform(body))
          }
        }
      }
    
    // valp n = block-tag(n1); e => valp n = (block-tag(n1) << 1) + 1;  e
    case H.LetP(name, MiniScalaBlockTag, Seq(blockSym), body) => 
      tempLetL(1) { c1 => {
          tempLetP(CPSBlockTag, Seq(blockSym)) { blockTagValSym => {
              tempLetP(CPSArithShiftL, Seq(blockTagValSym, c1)) { blockTagValShifted => {
                  L.LetP(name, CPSAdd, Seq(blockTagValShifted, c1), transform(body))
                }
              }
            }
          }
        }
      }
    
    case H.LetP(name, MiniScalaBlockLength, Seq(blockSym), body) =>
      tempLetL(1) { c1 => 
          tempLetP(CPSBlockLength, Seq(blockSym)) {
            blockLenValSym => {
              tempLetP(CPSArithShiftL, Seq(blockLenValSym, c1)) {
                blockLenValShifted => {
                  L.LetP(name, CPSAdd, Seq(blockLenValShifted, c1), transform(body))
                }
              }
            }
          }
        }        
      
    
    // decode the indexSym and then call the block-get
    case H.LetP(name, MiniScalaBlockGet, Seq(blockSym, indexSym), body) =>
      tempLetL(1) { c1 => 
        tempLetP(CPSArithShiftR, Seq(indexSym, c1)) { indexSymShifted => 
          {
            L.LetP(name, CPSBlockGet, Seq(blockSym, indexSymShifted), transform(body))
          }
        }
      }
    
    // decode the indexSym and then call the block-set ?
    case H.LetP(name, MiniScalaBlockSet, Seq(blockSym, indexSym, valueSym), body) =>
        tempLetL(1) { c1 => 
          tempLetP(CPSArithShiftR, Seq(indexSym, c1)) { indexSymShifted => 
            {
              L.LetP(name, CPSBlockSet, Seq(blockSym, indexSymShifted, valueSym), transform(body))
            }
          }
        }
    

    // Conversion primitives int->char/char->int
    // TODO
    
    // Int to char
    case H.LetP(name, MiniScalaIntToChar, Seq(intValSym), body) => 
      tempLetL(2) { c2 => 
        tempLetP(CPSArithShiftL, Seq(intValSym, c2)) { intValSymShifted => 
          {
            L.LetP(name, CPSAdd, Seq(intValSymShifted , c2), transform(body))
          }
        }
      }
    
    case H.LetP(name, MiniScalaCharToInt, Seq(charValSym), body) => 
      tempLetL(2) { c2 => 
          L.LetP(name, CPSArithShiftR, Seq(charValSym , c2), transform(body))
      }
      


    // IO primitives
    // TODO

    case H.LetP(name, MiniScalaByteRead, args, body) =>
      // as per to the defintion mentioned in the slides
      tempLetP(CPSByteRead, args) { byteRead => 
          tempLetL(1) { c1 => {
            tempLetP(CPSArithShiftL, Seq(byteRead, c1)) { byteReadShifted => 
                L.LetP(name, CPSAdd, Seq(byteReadShifted, c1), transform(body))
            }
          }
        }
      }
    
    // right sift the encoded byte and then call the byte-write
    // cross check this
    case H.LetP(name, MiniScalaByteWrite, Seq(encodedByteValSym), body) => 
      tempLetL(1) {c1 => {
        tempLetP(CPSArithShiftR, Seq(encodedByteValSym, c1)) { decodedByteValSym => 
          {
            tempLetP(CPSByteWrite, Seq(decodedByteValSym)) { byteWriteVal => 
              {
                // TODO added to fix failing test!!!
                // cross check if this is right!!
                L.LetL(name, unitLit, transform(body))
              }
            }
          }
        }
      }
    }




    // Continuations nodes (LetC, AppC)
    case H.LetC(cntdefs, body) =>
        var transformedCntDefs = cntdefs.map(cntdef => {
          val cntDefInstance = cntdef.asInstanceOf[H.CntDef]
          val cntDefName = cntDefInstance.name
          val cntDefArgs = cntDefInstance.args
          val cntDefBody = cntDefInstance.body
          val transformedCntDefBody = transform(cntDefBody)
          L.CntDef(cntDefName, cntDefArgs, transformedCntDefBody)
        })

        L.LetC(transformedCntDefs, transform(body))
      
    case H.AppC(name, args) =>
      L.AppC(name, args)

    
    // case H.AppF(name, cnt, args) => 
    //   L.AppF(name, cnt, args)
    
    // case H.LetF(funs, body) => 
    //     val transformedFuns = funs.map(fun => {
    //       val funInstance = fun.asInstanceOf[H.FunDef]
    //       val funName = funInstance.name
    //       val funArgs = funInstance.args
    //       val funBody = funInstance.body
    //       val transformedFunBody = transform(funBody)
    //       L.FunDef(funName, funInstance.retC, funArgs, transformedFunBody)
    //     })
    //     L.LetF(transformedFuns, transform(body))
        


    // Functions nodes (LetF, AppF)
    // TODO

    case H.LetF(funs, exp) =>
      var transformedFuns = Seq[L.FunDef]()
      var funNametoFreeVarsMap = Map[Symbol, Seq[Symbol]]()
      var functionToWorkerMap = Map[Symbol, Symbol]()
      var funSymbols = Seq[Symbol]()

      funs.foreach(fun => {
        // Defined clousre worker with taking as a argument.
        
        val funInstance = fun.asInstanceOf[H.FunDef]
        val funArgs = funInstance.args
        val funNameSymbol = funInstance.name

        // add the function name to the list of functions
        funSymbols = funNameSymbol +: funSymbols 

        //Get the list of free variables
        var freeVarsForFun = freeVariables(fun)(Map.empty)
        val freeVarsForFunSeq = freeVarsForFun.toSeq

        // add it to the map so that it can be used later during fn calls
        funNametoFreeVarsMap += (funNameSymbol -> freeVarsForFunSeq)

        // create env symbol
        val envSym = Symbol.fresh("env")

        // define a set of symbols corresponding to the free variables
        val valSyms = freeVarsForFunSeq.map(freeVar => Symbol.fresh("v"))

        // create worker map for fun body 
        var workerMap = Map[Symbol, (Symbol, Seq[Symbol])]()
        workerMap += (funNameSymbol -> (envSym, Seq()))
        valSyms.zip(freeVarsForFunSeq).foreach(entry => {
          val (valSym, freeVar) = entry
          workerMap += (freeVar -> (valSym, Seq()))
        })
        
        val bodyTrans = transform(funInstance.body) subst Substitution(funNameSymbol +: freeVarsForFunSeq, envSym +: valSyms)

        // worker defintion
        val workerFun = Symbol.fresh("worker")
        functionToWorkerMap += (funNameSymbol -> workerFun)

        // wrap and create the new function body
        val workerArgs = Seq(envSym) ++ funArgs

        var i = 0
        val wrapUtilList = valSyms.map(valSym => {
          i = i + 1
          (valSym, envSym, i)
        })
        
        val WorkerBody = wrap(wrapUtilList, bodyTrans) {
          case((v, e, i), bodyTrans) => 
            tempLetL(i) { iSym => 
              L.LetP(v, CPSBlockGet, Seq(e, iSym), bodyTrans)
            }
        }
        
        val workerFunDef = L.FunDef(workerFun, funInstance.retC, workerArgs, WorkerBody)
        transformedFuns = transformedFuns :+ workerFunDef
      })

      // creating body and adding the function definitions
      val funWorkerFreeVarsMap = funNametoFreeVarsMap.map(entry => {
        val (funName, freeVars) = entry
        val workerFun = functionToWorkerMap(funName)
        (funName,  workerFun, freeVars)
      })

      // section to create temporary variables for closure
      // of each function
      var tempDecTail = transform(exp)
      funSymbols.foreach(funSymbol => {
          // get the free variables for the function
          val freeVars = funNametoFreeVarsMap(funSymbol)
          val workerSymbol = functionToWorkerMap(funSymbol)

          var blockSetList: Seq[(miniscala.Symbol, Int, miniscala.Symbol)] = Seq()
          blockSetList = freeVars.map(v => (funSymbol, freeVars.indexOf(v) + 1, v))
          blockSetList = (funSymbol, 0 , workerSymbol) +: blockSetList

          tempDecTail = wrap(blockSetList, tempDecTail) {
            // does this need to change to LetL declration for integer interal
            case((funSym, i, v), tempDecTail) =>
              tempLetL(i) {
                iSym => L.LetP(Symbol.fresh("t"), CPSBlockSet, Seq(funSym, iSym, v), tempDecTail)
              }
          }
      })

      // create the closure for each function
      val funFreeVars = funSymbols.map(funSymbol => (funSymbol, funNametoFreeVarsMap(funSymbol).size + 1))
      val letFRes = wrap(funFreeVars, tempDecTail) {
        case((funSym, size), tempDecTail) =>
          tempLetL(size) { sizeSym =>
            L.LetP(funSym, CPSBlockAlloc(202), Seq(sizeSym), tempDecTail)
          }
      }
      L.LetF(transformedFuns, letFRes)

    case H.AppF(fun, cnt, args) => 
      // get closure and call the worker
      val workerSymbol = Symbol.fresh("w")
      val actualArgs = Seq(fun) ++ args
      tempLetL(0) { zeroSym =>
        L.LetP(workerSymbol, CPSBlockGet, Seq(fun, zeroSym), 
          L.AppF(workerSymbol, cnt, actualArgs)
        )
      }


    // ********************* Conditionnals ***********************
    // Type tests
    case H.If(MiniScalaEq, args, ifExp, thenExp) => 
      L.If(CPSEq, args, ifExp, thenExp)

    case H.If(MiniScalaIntLt, args, ifExp, thenExp) => 
      L.If(CPSLt, args, ifExp, thenExp)

    case H.If(MiniScalaIntLe, args, ifExp, thenExp) =>
      L.If(CPSLe, args, ifExp, thenExp)
    
    case H.If(MiniScalaIntGt, args, ifExp, thenExp) =>
      L.If(CPSGt, args, ifExp, thenExp)
    
    case H.If(MiniScalaIntGe, args, ifExp, thenExp) =>
      L.If(CPSGe, args, ifExp, thenExp)
    
    case H.If(MiniScalaNe, args, ifExp, thenExp) =>
      L.If(CPSNe, args, ifExp, thenExp)


    case H.If(MiniScalaBlockP, Seq(a), thenC, elseC) =>
      ifEqLSB(a, Seq(0, 0), thenC, elseC)

    case H.If(MiniScalaIntP, Seq(a), thenC, elseC) =>
      ifEqLSB(a, Seq(1), thenC, elseC)

    case H.If(MiniScalaUnitP, Seq(a), thenC, elseC) =>
      ifEqLSB(a, Seq(0, 0, 1, 0), thenC, elseC)
    
    case H.If(MiniScalaCharP, Seq(a), thenC, elseC) =>
      val threeBitsSet = 7
      val charBitMask = 6
      tempLetL(threeBitsSet) { threeBitsSetSym => {
          tempLetL(charBitMask) { charBitMaskSym => {
              tempLetP(CPSAnd, Seq(a, threeBitsSetSym)) { andRes => {
                  L.If(CPSEq, Seq(andRes, charBitMaskSym), thenC, elseC)
                }
              }
            }
          }
        }
      }

    case H.If(MiniScalaBoolP, Seq(a), thenC, elseC) =>
      tempLetL(15) { fourBitsSet => {
          tempLetL(10) { boolBitMask => {
              tempLetP(CPSAnd, Seq(a, fourBitsSet)) { andRes => {
                  L.If(CPSEq, Seq(andRes, boolBitMask), thenC, elseC)
                }
              }
            }
          }
        }
      }

    // TODO: add missing cases

    // Test primitives (<, >, ==, ...)
    // Change This!


    // Halt case
    case H.Halt(v) =>
      tempLetL(1) { c1 => 
        tempLetP(CPSArithShiftR, Seq(v, c1)) { r => 
          {
            L.Halt(r)
          }
        }
      }
  }

  /*
   * Auxilary function.
   *
   * Example:
   *  // assuming we have a function with symbol f and the return continuation is rc:
   *
   *  val names = Seq("first", "second")
   *  val values = Seq(42, 112)
   *  val inner = L.AppF(f, rc, names)
   *  val res = wrap(names zip values , inner) {
   *    case ((n, v), inner) => L.LetL(n, v, inner)
   *  }
   *
   *  // res is going to be the following L.Tree
   *  L.LetL("first", 42,
   *    L.LetL("second", 112,
   *      L.AppF(f, rc, Seq("first", "second"))
   *    )
   *  )
   */
  private def wrap[T](args: Seq[T], inner: L.Tree)(createLayer: (T, L.Tree) => L.Tree) = {
    def addLayers(args: Seq[T]): L.Tree = args match {
      case h +: t => createLayer(h, addLayers(t))
      case _ => inner
    }
    addLayers(args)
  }

  private def freeVariables(tree: H.Tree)
                           (implicit worker: Map[Symbol, Set[Symbol]])
      : Set[Symbol] = tree match {
    case H.LetL(name, _, body) =>
      freeVariables(body)(worker) - name
    
    case H.LetP(name, _, args, body) =>
      freeVariables(body) - name ++ args
    
    case H.LetC(cntFuns, body) => 
      val bodyFreeVars = freeVariables(body)
      val cntFnsFreeVarsll = cntFuns.map(cntFun => {
        val cntFn = cntFun.asInstanceOf[H.CntDef]
        val cntFnFreeVars = freeVariables(cntFn.body) -- cntFn.args
        cntFnFreeVars
      })
      val cntFnsFreeVars = cntFnsFreeVarsll.flatten.toSet

      bodyFreeVars ++ cntFnsFreeVars
    
    case H.LetF(funs, body) =>
      val bodyFreeVars = freeVariables(body)
      val funsFreeVars = funs.map(fun => {
        val funFreeVars = freeVariables(fun.body) -- fun.args
        funFreeVars
      }).flatten.toSet

      val funcNms = funs.map(fun => fun.name).toSet

      bodyFreeVars ++ funsFreeVars -- funcNms
    
    case H.AppC(cnt, args) =>
      args.toSet
    
    case H.AppF(fun, cnt, args) =>
      val appFFreeVars = fun +: args 
      appFFreeVars.toSet -- Set(cnt) // remove continuation
    
    case H.If(_, args, thenC, elseC) =>
      args.toSet
  }

  private def freeVariables(cnt: H.CntDef)
                           (implicit worker: Map[Symbol, Set[Symbol]])
      : Set[Symbol] =
    freeVariables(cnt.body) -- cnt.args

  private def freeVariables(fun: H.FunDef)
                           (implicit worker: Map[Symbol, Set[Symbol]])
      : Set[Symbol] =
    freeVariables(fun.body) - fun.name -- fun.args

  // Tree builders

  /**
   * Call body with a fresh name, and wraps its resulting tree in one
   * that binds the fresh name to the given literal value.
   */
  private def tempLetL(v: Int)(body: L.Name => L.Tree): L.Tree = {
    val tempSym = Symbol.fresh("t")
    L.LetL(tempSym, v, body(tempSym))
  }

  /**
   * Call body with a fresh name, and wraps its resulting tree in one
   * that binds the fresh name to the result of applying the given
   * primitive to the given arguments.
   */
  private def tempLetP(p: L.ValuePrimitive, args: Seq[L.Name])
                      (body: L.Name => L.Tree): L.Tree = {
    val tempSym = Symbol.fresh("t")
    L.LetP(tempSym, p, args, body(tempSym))
  }

  /**
   * Generate an If tree to check whether the least-significant bits
   * of the value bound to the given name are equal to those passed as
   * argument. The generated If tree will apply continuation tC if it
   * is the case, and eC otherwise. The bits should be ordered with
   * the most-significant one first (e.g. the list (1,1,0) represents
   * the decimal value 6).
   */
  private def ifEqLSB(arg: L.Name, bits: Seq[Int], tC: L.Name, eC: L.Name)
      : L.Tree =
    tempLetL(bitsToIntMSBF(bits map { b => 1 } : _*)) { mask =>
      tempLetP(CPSAnd, Seq(arg, mask)) { masked =>
        tempLetL(bitsToIntMSBF(bits : _*)) { value =>
          L.If(CPSEq, Seq(masked, value), tC, eC) } } }
}
