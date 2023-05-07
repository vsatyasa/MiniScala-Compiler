package miniscala.test

import miniscala.test.infrastructure.CPSHighTest
import org.junit.Test

class CMScalaToCPS_Whitebox_Tail extends CPSHighTest {
  @Test def testTailUselessContinuations =
    testCPSHighTreeEquality("def f(g: () => Int) = g(); 1", """
      |deff v$1(v$2, v$3) = { v$3(v$2) };
      |vall v$4 = 1;
      |vall v$5 = 0;
      |halt(v$5)
      """.stripMargin
    )
  
  @Test def testTailWithOptimization =
    testCPSHighTreeEquality("def f(g: () => Int) = g(); def g(): Int = 10; f(g)", """
      |deff v$1(v$3, v$4) = {
      | v$4(v$3)
      | }; deff v$2(v$5) = { vall v$6 = 10; v$5(v$6) };
      | defc v$7(v$8) = { vall v$9 = 0; halt(v$9) };
      | v$1(v$7, v$2)
      """.stripMargin
    )
  
  @Test def testTailWhileOptimization =
    testCPSHighTreeEquality("var x = 0; val l = 4; while(x < l) {x = x + 1};  l + 3", """
      | vall v$1 = 1;
      | valp v$2 = block-alloc-242(v$1);
      | vall v$3 = 0;
      | vall v$4 = 0;
      | valp v$5 = block-set(v$2, v$3, v$4);
      | vall v$6 = 4;
      | valp v$7 = id(v$6);
      | defc v$8(v$9) = {
      |   defc v$10() = {
      |     vall v$11 = 3;
      |     valp v$12 = v$7 + v$11;
      |     vall v$13 = 0;
      |     halt(v$13)
      |   };
      |   defc v$14() = {
      |     vall v$15 = 0;
      |     vall v$16 = 0;
      |     valp v$17 = block-get(v$2, v$16);
      |     vall v$18 = 1;
      |     valp v$19 = v$17 + v$18;
      |     valp v$20 = block-set(v$2, v$15, v$19);
      |     valp v$21 = id(v$20);
      |     vall v$22 = ();
      |     v$8(v$22)
      |   };
      |   vall v$23 = 0;
      |   valp v$24 = block-get(v$2, v$23);
      |   if (v$24 < v$7) v$14 else v$10
      | };
      | vall v$25 = ();
      | v$8(v$25)
      """.stripMargin
    )

    
  @Test def testTailWhileOptimizationExtended1 =
    testCPSHighTreeEquality("var x = 0; val l = 4; while(x < l) {val z = if (3 > 2) 2 else 1; x = x + 1};  l + 3", """
      | vall v$1 = 1;
      | valp v$2 = block-alloc-242(v$1);
      | vall v$3 = 0;
      | vall v$4 = 0;
      | valp v$5 = block-set(v$2, v$3, v$4);
      | vall v$6 = 4;
      | valp v$7 = id(v$6);
      | defc v$8(v$9) = {
      |   defc v$10() = {
      |     vall v$11 = 3;
      |     valp v$12 = v$7 + v$11;
      |     vall v$13 = 0;
      |     halt(v$13)
      |   };
      |   defc v$14() = {
      |     defc v$15(v$16) = {
      |       valp v$17 = id(v$16);
      |       vall v$18 = 0;
      |       vall v$19 = 0;
      |       valp v$20 = block-get(v$2, v$19);
      |       vall v$21 = 1;
      |       valp v$22 = v$20 + v$21;
      |       valp v$23 = block-set(v$2, v$18, v$22);
      |       valp v$24 = id(v$23);
      |       vall v$25 = ();
      |       v$8(v$25)
      |     };
      |     defc v$26() = { vall v$27 = 2; v$15(v$27) };
      |     defc v$28() = { vall v$29 = 1; v$15(v$29) };
      |     vall v$30 = 3;
      |     vall v$31 = 2;
      |     if (v$30 > v$31) v$26 else v$28
      |   };
      |   vall v$32 = 0;
      |   valp v$33 = block-get(v$2, v$32);
      |   if (v$33 < v$7) v$14 else v$10
      | };
      | vall v$34 = ();
      | v$8(v$34)
      """.stripMargin
    )


  @Test def testTailWhileOptimizationExtended2 =
    testCPSHighTreeEquality("var x = 0; val l = 4; while(x < l) {val z = if (3 > 2) 2 else 1; x = x + 1};  l + 3", """
      | vall v$1 = 1;
      | valp v$2 = block-alloc-242(v$1);
      | vall v$3 = 0;
      | vall v$4 = 0;
      | valp v$5 = block-set(v$2, v$3, v$4);
      | vall v$6 = 4;
      | valp v$7 = id(v$6);
      | defc v$8(v$9) = {
      |   defc v$10() = {
      |     vall v$11 = 3;
      |     valp v$12 = v$7 + v$11;
      |     vall v$13 = 0;
      |     halt(v$13)
      |   };
      |   defc v$14() = {
      |     defc v$15(v$16) = {
      |       valp v$17 = id(v$16);
      |       vall v$18 = 0;
      |       vall v$19 = 0;
      |       valp v$20 = block-get(v$2, v$19);
      |       vall v$21 = 1;
      |       valp v$22 = v$20 + v$21;
      |       valp v$23 = block-set(v$2, v$18, v$22);
      |       valp v$24 = id(v$23);
      |       vall v$25 = ();
      |       v$8(v$25)
      |     };
      |     defc v$26() = { vall v$27 = 2; v$15(v$27) };
      |     defc v$28() = { vall v$29 = 1; v$15(v$29) };
      |     vall v$30 = 3;
      |     vall v$31 = 2;
      |     if (v$30 > v$31) v$26 else v$28
      |   };
      |   vall v$32 = 0;
      |   valp v$33 = block-get(v$2, v$32);
      |   if (v$33 < v$7) v$14 else v$10
      | };
      | vall v$34 = ();
      | v$8(v$34)
      """.stripMargin
    )

  // TODO add more cases
}
