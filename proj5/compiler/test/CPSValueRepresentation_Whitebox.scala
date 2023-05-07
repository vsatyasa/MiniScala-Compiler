package miniscala.test

import miniscala.test.infrastructure.CPSLowTest
import org.junit.Test

/** Whitebox testing for entire program outputs */
class CPSValueRepresentation_Whitebox extends CPSLowTest {

  // Starting this "Value representation" assignment, we will not have whitebox
  // tests anymore. We will keep the black box tests, to check your submission
  // correctness, but from now on it's up to you to find the best translation.

  // Nevertheless, here's a test, to have an example:
  @Test def testValueReprOnePlusTwo =
    testCPSLowTreeEquality("1 + 2",
      """vall v$1 = 3;
        |vall v$2 = 5;
        |valp v$3 = v$1 + v$2;
        |vall v$4 = 1;
        |valp v$5 = v$3 - v$4;
        |vall v$6 = 1;
        |vall v$7 = 1;
        |valp v$8 = v$6 >> v$7;
        |halt(v$8)""".stripMargin)


 @Test def whileLoopWhiteBox =
    testCPSLowTreeEquality("var x = 0; val l = 4; while(x < l) {x = x + 1};  l + 3", """
    |vall v$1 = 3;
    |vall v$2 = 1;
    |valp v$3 = v$1 >> v$2;
    |valp v$4 = block-alloc-242(v$3);
    |vall v$5 = 1;
    |vall v$6 = 1;
    |vall v$7 = 1;
    |valp v$8 = v$5 >> v$7;
    |valp v$9 = block-set(v$4, v$8, v$6);
    |vall v$10 = 9;
    |valp v$11 = id(v$10);
    |defc v$12(v$13) = {
    | defc v$14() = {
    |   vall v$15 = 7;
    |   valp v$16 = v$11 + v$15;
    |   vall v$17 = 1;
    |   valp v$18 = v$16 - v$17;
    |   vall v$19 = 1;
    |   vall v$20 = 1;
    |   valp v$21 = v$19 >> v$20;
    |   halt(v$21)
    | };
    | defc v$22() = {
    |   vall v$23 = 1;
    |   vall v$24 = 1;
    |   vall v$25 = 1;
    |   valp v$26 = v$24 >> v$25;
    |   valp v$27 = block-get(v$4, v$26);
    |   vall v$28 = 3;
    |   valp v$29 = v$27 + v$28;
    |   vall v$30 = 1;
    |   valp v$31 = v$29 - v$30;
    |   vall v$32 = 1;
    |   valp v$33 = v$23 >> v$32;
    |   valp v$34 = block-set(v$4, v$33, v$31);
    |   valp v$35 = id(v$31);
    |   vall v$36 = 2;
    |   v$12(v$36)
    | };
    | vall v$37 = 10;
    | vall v$38 = 1;
    | vall v$39 = 1;
    | valp v$40 = v$38 >> v$39;
    | valp v$41 = block-get(v$4, v$40);
    | if (v$41 < v$11) v$22 else v$14
    |};
    |vall v$42 = 2;
    |v$12(v$42)
    |
      """.stripMargin
    )

  @Test def testComplexWhileLoop =
    testCPSLowTreeEquality("var x = 0; val l = 4; while(x < l) {val z = if (3 > 2) 2 else 1; x = x + 1};  l + 3", """
      |vall v$1 = 3;
      |vall v$2 = 1;
      |valp v$3 = v$1 >> v$2;
      |valp v$4 = block-alloc-242(v$3);
      |vall v$5 = 1;
      |vall v$6 = 1;
      |vall v$7 = 1;
      |valp v$8 = v$5 >> v$7;
      |valp v$9 = block-set(v$4, v$8, v$6);
      |vall v$10 = 9;
      |valp v$11 = id(v$10);
      |defc v$12(v$13) = {
      |  defc v$14() = {
      |    vall v$15 = 7;
      |    valp v$16 = v$11 + v$15;
      |    vall v$17 = 1;
      |    valp v$18 = v$16 - v$17;
      |    vall v$19 = 1;
      |    vall v$20 = 1;
      |    valp v$21 = v$19 >> v$20;
      |    halt(v$21)
      |  };
      |  defc v$22() = {
      |    defc v$23(v$24) = {
      |      valp v$25 = id(v$24);
      |      vall v$26 = 1;
      |      vall v$27 = 1;
      |      vall v$28 = 1;
      |      valp v$29 = v$27 >> v$28;
      |      valp v$30 = block-get(v$4, v$29);
      |      vall v$31 = 3;
      |      valp v$32 = v$30 + v$31;
      |      vall v$33 = 1;
      |      valp v$34 = v$32 - v$33;
      |      vall v$35 = 1;
      |      valp v$36 = v$26 >> v$35;
      |      valp v$37 = block-set(v$4, v$36, v$34);
      |      valp v$38 = id(v$34);
      |      vall v$39 = 2;
      |      v$12(v$39)
      |    };
      |    defc v$40() = { vall v$41 = 5; v$23(v$41) };
      |    defc v$42() = { vall v$43 = 3; v$23(v$43) };
      |    vall v$44 = 7;
      |    vall v$45 = 5;
      |    if (v$44 > v$45) v$40 else v$42
      |  };
      |  vall v$46 = 10;
      |  vall v$47 = 1;
      |  vall v$48 = 1;
      |  valp v$49 = v$47 >> v$48;
      |  valp v$50 = block-get(v$4, v$49);
      |  if (v$50 < v$11) v$22 else v$14
      |};
      |vall v$51 = 2;
      |v$12(v$51)
      |
      """.stripMargin
    )

@Test def testFunctionAsClosure =
    testCPSLowTreeEquality("def f(x: Int): Int = x + 10; f(2)", """
      |deff v$1(v$2, v$3, v$4) = {
      |  vall v$5 = 21;
      |  valp v$6 = v$4 + v$5;
      |  vall v$7 = 1;
      |  valp v$8 = v$6 - v$7;
      |  v$2(v$8)
      |};
      |vall v$9 = 1;
      |valp v$10 = block-alloc-202(v$9);
      |vall v$11 = 0;
      |valp v$12 = block-set(v$10, v$11, v$1);
      |vall v$13 = 5;
      |defc v$14(v$15) = {
      |  vall v$16 = 1;
      |  vall v$17 = 1;
      |  valp v$18 = v$16 >> v$17;
      |  halt(v$18)
      |};
      |vall v$19 = 0;
      |valp v$20 = block-get(v$10, v$19);
      |v$20(v$14, v$10, v$13)
      """.stripMargin
    )

    @Test def testComplexClosure =
    testCPSLowTreeEquality("def f(x: Int) = (y: Int) => x + y  ; val x = f(2); x(10)", """
      |deff v$1(v$3, v$4, v$5) = {
      |  vall v$6 = 2;
      |  valp v$7 = block-alloc-202(v$6);
      |  vall v$8 = 0;
      |  valp v$9 = block-set(v$7, v$8, v$2);
      |  vall v$10 = 1;
      |  valp v$11 = block-set(v$7, v$10, v$5);
      |  v$3(v$7)
      |}; deff v$2(v$12, v$13, v$14) = {
      |  vall v$15 = 1;
      |  valp v$16 = block-get(v$13, v$15);
      |  valp v$17 = v$16 + v$14;
      |  vall v$18 = 1;
      |  valp v$19 = v$17 - v$18;
      |  v$12(v$19)
      |};
      |vall v$20 = 1;
      |valp v$21 = block-alloc-202(v$20);
      |vall v$22 = 0;
      |valp v$23 = block-set(v$21, v$22, v$1);
      |vall v$24 = 5;
      |defc v$25(v$26) = {
      |  valp v$27 = id(v$26);
      |  vall v$28 = 21;
      |  defc v$29(v$30) = {
      |    vall v$31 = 1;
      |    vall v$32 = 1;
      |    valp v$33 = v$31 >> v$32;
      |    halt(v$33)
      |  };
      |  vall v$34 = 0;
      |  valp v$35 = block-get(v$27, v$34);
      |  v$35(v$29, v$27, v$28)
      |};
      |vall v$36 = 0;
      |valp v$37 = block-get(v$21, v$36);
      |v$37(v$25, v$21, v$24)
      """.stripMargin
    )

}
