package miniscala.test

import ok.AllOKTests

import miniscala.test.infrastructure.CPSHighTest
import org.junit.Test

/** Whitebox testing for entire program outputs */
class CMScalaToCPS_Whitebox_NonTail extends CPSHighTest {

  // TODO: Test recursive functions

  @Test def testNonTailLiteral = {
    testCPSHighTreeEquality("3", "vall v$1 = 3; vall v$2 = 0; halt(v$2)")
  } 

  @Test def testNonTailMultiLet =
    testCPSHighTreeEquality("val x = 1; val y = 2; y",
        "vall v$1 = 1; valp v$2 = id(v$1); vall v$3 = 2; valp v$4 = id(v$3); vall v$5 = 0; halt(v$5)")
  
  @Test def testPrimNonTial1 =
    testCPSHighTreeEquality("var x = 0; val l = 4; val z =  (l << 2) + 3; z", """
      vall v$1 = 1;
      valp v$2 = block-alloc-242(v$1);
      vall v$3 = 0;
      vall v$4 = 0;
      valp v$5 = block-set(v$2, v$3, v$4);
      vall v$6 = 4;
      valp v$7 = id(v$6);
      vall v$8 = 2;
      valp v$9 = v$7 << v$8;
      vall v$10 = 3;
      valp v$11 = v$9 + v$10;
      valp v$12 = id(v$11);
      vall v$13 = 0;
      halt(v$13)
      """
    )

  @Test def testPrimNonTial2 =
    testCPSHighTreeEquality("var x = 0; val l = 4; val z =  (l << 2) + 10; z", """
      vall v$1 = 1;
      valp v$2 = block-alloc-242(v$1);
      vall v$3 = 0;
      vall v$4 = 0;
      valp v$5 = block-set(v$2, v$3, v$4);
      vall v$6 = 4;
      valp v$7 = id(v$6);
      vall v$8 = 2;
      valp v$9 = v$7 << v$8;
      vall v$10 = 10;
      valp v$11 = v$9 + v$10;
      valp v$12 = id(v$11);
      vall v$13 = 0;
      halt(v$13)
      """
    )

  // TODO add more tests

}
