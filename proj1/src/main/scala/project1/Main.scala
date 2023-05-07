package project1

import java.io._

object Runner {

  def main(args: Array[String]) = {

    val reader = new BaseReader(args(0).iterator, '\u0000')

    // Parser to test!
    val parser = new ArithParOpParser(reader)

    // Generator to test
    val generator = new RegASMGenerator {
      // Define the PrintWriter used to emit
      // the code.
      val out = new ByteArrayOutputStream
      val pOut = new PrintWriter(out, true)
      override def stream = pOut

      // Emit the code and return the String
      // representation of the code
      def code(ast: Exp) = {
        emitCode(ast)
        out.toString.stripLineEnd
      }
    }

    val ast = parser.parseCode

    println("============= AST ================")
    println(s"\t$ast")
    println("==================================\n")

    // print the AST parsed
    val code = generator.code(ast)

    val data = Map[Char,Int]()
    val runner = new ASMRunner(code, data)

    println("============ OUTPUT ==============")
    println(runner.code)
    println("==================================")

    if (runner.assemble != 0) {
      println("Compilation error!")
    } else {
      println(s"Result: ${runner.run}")
    }
  }
}
