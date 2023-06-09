package miniscala
package test.ok
import org.junit.Test

trait ConstructsOKTests extends MainHelper {
  this: AllOKTests =>

  @Test def testAppOrder =
    compileAndInterpretWithLib("""
      def intFunGen() = {
        putchar('O'.toInt); (i: Int) => i + 1
      };
      def intGen() = {
        putchar('K'.toInt); 0
      };
      intFunGen()(intGen())
      """)

  @Test def testLetrec =
    compileAndInterpretWithLib("""
      def f(x: Int): Int = if (x > 0) g(x) else x;
      def g(x: Int): Int = f(x - 1);
      putchar('O'.toInt);
      if (f(5) == 0) putchar('K'.toInt) else putchar('O'.toInt)
    """)

  @Test def testLetrec2 =
    compileAndInterpretWithLib("""
      def f(x: Int): Int = if (x > 0) g(x) else x;
      def g(x: Int): Int = f(x - 1);
      putchar('O'.toInt);
      if (f(50) == 0) putchar('K'.toInt) else putchar('O'.toInt)
    """)

}
