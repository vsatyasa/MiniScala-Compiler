package project1

/*
 * We are going to implement a compiler that can generate code to
 * compute the value of an expression.
 *
 * One of the main differences between assembly code and code like C
 * or Scala is that there is no way to 'return' a value.
 * This is something of a problem, because when we generate code for
 *   Plus(Plus(Lit(1),Lit(2)), Lit(3)) <=> (1+2)+3
 * we first need to generate code for 1+2, then access this result,
 * and then add 3 to it. How do we access the result?
 *
 * By convention, we decide to store the result of every operation
 * in the %rax register. This means that when we go through the
 * AST and generate code, we know that the result will be in %rax.
 *
 * However, problems arise if we need to store multiple intermediate
 * result. For example:
 *  Plus(Lit(1), Plus(Lit(2),Lit(3))) <=> 1+(2+3)
 * We will generate code that loads 1 into %rax, but then we will need
 * to compute the result of 2+3...and we will save it into %rax!
 * We could put '1' in %rax, but where do we put '2'? Maybe %rcx.
 * But given that a general expression can have any level of
 * complexity, we are going to run out of registers.
 *
 * One solution is to use the stack. As a reminder, in x86 we
 * can push and pop registers to the stack:
 *   - push %rax   put the value of %rax on top of the stack
 *   - pop %rbx    load the value that is currently on the
 *                 top of the stack into %rbx
 *
 * Small example of stack usage:
 *    movq $1, %rax
 *    movq $2, %rbx
 *    pushq %rax
 *    pushq %rbx
 *    popq %rax
 *    popq %rbx
 *    rbx == 1 and rax == 2
 *
 * We are therefore going to use the stack to store intermediate
 * results rather than using other registers. The main idea is the
 * following (illustrated for the Plus operation):
 *    Plus(x, y) (x and y are arbitrarily complex)
 *
 *    <some code> // code that computes the value of x
 *                // result is stored in %rax
 *    pushq %rax  // save intermediate result
 *    <some code> // code that computes the value of y
 *                // result is stored in %rax
 *    popq %rbx   // reload value of x
 *    addq %rbx, %rax // code that computes x+y
 *                    // value is stored in %rax!!
 *
 * This idea requires another convention: we need to make sure that
 * popq %rbx gets back the value of x, and not a value added on the
 * stack by the code we can't see. So in addition to leaving the
 * result in %rax, each operator needs to clean the stack and leave it
 * the same way it found it. This means that for each push, there
 * must be an accompanying pop. Thankfully, we followed that as well!!!
 *
 * This convention is also used in the C language: in the Util.scala
 * file, the template for the generated code is using the 'ret'
 * instruction  at the end. In the C language, the calling convention
 * requires that functions leave their result into %rax when returning,
 * and the stack needs to be reset.
 *
 * example of run:
 * run "1"
 *============ OUTPUT ==============
 *.text
 *        .global entry_point
 *
 *entry_point:
 *        push %rbp       # save stack frame for C convention
 *        mov %rsp, %rbp
 *
 *        # beginning generated code
 *        movq $1, %rax
 *        # end generated code
 *        # %rax contains the result
 *
 *        mov %rbp, %rsp  # reset frame
 *        pop %rbp
 *        ret
 *
 *
 *
 *==================================
 *Result: 1
 */
abstract class StackASMGenerator extends Codegen {

  def emitCode(exp: Exp): Unit = exp match {
    // A constant number is represented with the $ prefix
    // In order to escape a $ in a String interpretation,
    // we need to double it $$, and then we add the value
    // of x: $x. As such, we end up with three $ in a row
    case Lit(x) => emitln(s"""movq $$$x, %rax""")
    case Plus(x,y) =>
      emitCode(x)
      emitln("pushq %rax")
      emitCode(y)
      emitln("popq %rbx")
      emitln("addq %rbx, %rax")
    case Minus(x,y) =>
      emitCode(x)
      emitln("pushq %rax")
      emitCode(y)
      emitln("popq %rbx")
      emitln("subq %rax, %rbx")
      emitln("mov %rbx, %rax")
    case Times(x,y) =>
      emitCode(x)
      emitln("pushq %rax")
      emitCode(y)
      emitln("popq %rbx")
      emitln("imul %rbx, %rax")
    case Div(x,y) =>
      emitCode(x)
      emitln("pushq %rax")
      emitCode(y)
      emitln("mov %rax, %rcx")
      emitln("popq %rax")
      emitln("cqto")
      emitln("idiv %rcx")
    // TODO: add the additional Exps
    // NOTE: the idiv op in x86 uses a fixed register for
    // the dividend: rdx:rax. This means that the rdx register
    // needs to be set before the idivq operation. After the operation,
    // %rax contains the quotient and %rdx contains the remainder.
  }
}

/*
 * We are going to go back on one of our design decision. When
 * we added the stack, we claimed that expressions can be arbitrarily
 * complex, and we would thus run out of registers. While this is
 * true in theory, in practice the expressions are not that
 * complex. Let's try to generate better code by using registers.
 *
 * Let us revisit what we are doing with the following example:
 *  1+2+4
 *
 *  1) load 1 into %rax
 *  2) push %rax (1) on the stack
 *  3) load 2 into %rax
 *  4) pop into %rbx (rbx == 1)
 *  5) add rbx to rax
 *  6) push %rax (3) on the stack
 *  7) load 4 into %rax
 *  8) pop into %rbx (rbx == 3)
 *  9) add %rbx to %rax
 * 10) DONE!!!
 *
 * It seems that there are a lot of useless operations. Can you
 * spot them and try to find a better sequence?
 *
 * ...
 * ...
 * ...
 * ...
 * ...
 * ...
 * ...
 * ...
 * ...
 * ...
 *
 * Let's examine this. We could first avoid the stack and move %rax into
 * %rbx directly:
 *
 *  1) load 1 into %rax
 *  2) move %rax (1) in %rbx
 *  3) load 2 into %rax
 *  4) add rbx to rax
 *  5) move %rax (3) in %rbx
 *  6) load 4 into %rax
 *  7) add %rbx to %rax
 *  8) DONE!!!
 *
 * Well, that is a nice improvement...but we could do better if we
 * are willing to relax our previous convention of using %rax to
 * store the result of the previous action and instead use the next
 * available register.
 *
 *  1) load 1 into %rax
 *  2) load 2 into %rbx
 *  3) add rbx to rax
 *  4) load 4 into %rbx
 *  5) add %rbx to %rax
 *  6) DONE!!!
 *
 * In order to do this, we are going to use the CPU register 'as'
 * a stack. We will define an order in which we are going to use
 * them, and we are going to keep track of the current 'stack pointer.'
 * Here a little illustration of this idea:
 *
 *  %rsi
 *  %rdi          <- sp = 2
 *  %rcx  <res2>
 *  %rbx  <res1>
 *
 * Using this idea, the generated code is going to be much more
 * efficient because there are no unnecessary moves!
 *
 * Note 1: We are still interfacing with a C function, so the final
 * result needs to be moved to %rax before return. (See emitCode).
 * Note 2: Because x86_64 uses %rax and %rdx in a very specific way
 * for idiv, for simplicity they are not part of the 'stack'.
 *
 * TODO: complete the implementation. Don't forget to add test cases.
 * You can verify that our code fails to generate code for:
 *   1+(1+(1+(1+(1+(1+(1+1))))))
 */
abstract class RegASMGenerator extends Codegen {

  val regs = Seq("%rbx", "%rcx", "%rdi", "%rsi", "%r8", "%r9")

  def emitCode(exp: Exp): Unit = {
    emitCode(exp, 0)
    emitln("movq %rbx, %rax")
  }

  /*
   * In order to understand what is the meaning of sp, let's
   * use an example:
   *  - emitCode(Lit(1), 2) => movq $1, %rdi
   *  in English: generate code that computes the value of
   *              Lit(1) and stores the result in index
   *              2 of the stack
   * sp represents the current top of the stack. Therefore
   * the function can use it to store the value of 'exp'
   */
  def emitCode(exp: Exp, sp: Int): Unit = exp match {
    case Lit(x) => emitln(s"""movq $$$x, ${regs(sp)}""")
    case Plus(x,y) =>
      emitCode(x, sp)     // result of x stored in regs(sp)
      emitCode(y, sp + 1) // result of y stored in regs(sp+1)
      emitln(s"addq ${regs(sp+1)}, ${regs(sp)}")
    case Minus(x,y) =>
      emitCode(x, sp)     // result of x stored in regs(sp)
      emitCode(y, sp + 1)
      emitln(s"subq ${regs(sp+1)}, ${regs(sp)}")
    case Times(x,y) =>
      emitCode(x, sp)     
      emitCode(y, sp + 1)
      emitln(s"imul ${regs(sp+1)}, ${regs(sp)}")
    case Div(x,y) =>
      emitCode(x, sp)     
      emitCode(y, sp + 1)
      emitln(s"mov ${regs(sp)}, %rax")
      emitln("cqto")
      emitln(s"idiv ${regs(sp+1)}")
      emitln(s"mov %rax, ${regs(sp)}")
  }
}
