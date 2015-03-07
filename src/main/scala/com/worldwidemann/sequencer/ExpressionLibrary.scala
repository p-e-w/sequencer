/*
 * Sequencer – purely algorithmic number sequence identification
 *
 * Copyright (c) 2015 Philipp Emanuel Weidmann <pew@worldwidemann.com>
 *
 * Nemo vir est qui mundum non reddat meliorem.
 *
 * Released under the terms of the GNU General Public License, Version 3
 */

package com.worldwidemann.sequencer

// Note: Arrays and explicit minimal type bounds are used throughout
//       this class in order to maximize lookup performance
class ExpressionLibrary(configuration: Configuration) {
  private val ONE = Number("1", 1)
  private val TWO = Number("2", 2)
  private val THREE = Number("3", 3)
  private val FOUR = Number("4", 4)
  private val FIVE = Number("5", 5)
  private val SIX = Number("6", 6)
  private val SEVEN = Number("7", 7)
  private val EIGHT = Number("8", 8)
  private val NINE = Number("9", 9)
  private val TEN = Number("10", 10)
  private val integers = Array[Expression](ONE, TWO, THREE, FOUR, FIVE, SIX, SEVEN, EIGHT, NINE, TEN)

  // Common functions of the index variable are hardcoded as atomic expressions
  // in order to speed up searches for formulas that contain them
  private val N = IndexFunction("(n)", n => n)
  private val N_PLUS_ONE = IndexFunction("(n)+1", n => n + 1)
  private val N_MINUS_ONE = IndexFunction("(n)-1", n => n - 1)
  private val TWO_N = IndexFunction("2*(n)", n => 2 * n)
  private val N_POWER_TWO = IndexFunction("(n)^2", n => n * n)
  private val TWO_POWER_N = IndexFunction("2^(n)", n => math.pow(2, n))
  private val indexFunctions = Array[Expression](N, N_PLUS_ONE, N_MINUS_ONE, TWO_N, N_POWER_TWO, TWO_POWER_N)

  private val NEGATE = UnaryPrefixOperator("-", x => -x)
  private val SQRT = UnaryPrefixOperator("Sqrt", x => math.sqrt(x))
  private val ABS = UnaryPrefixOperator("Abs", x => math.abs(x))
  private val SIGN = UnaryPrefixOperator("Sign", x => math.signum(x))
  private val FLOOR = UnaryPrefixOperator("Floor", x => math.floor(x))
  private val CEILING = UnaryPrefixOperator("Ceiling", x => math.ceil(x))
  private val ROUND = UnaryPrefixOperator("Round", x => math.round(x))
  private val ADD = BinaryInfixOperator("+", (x, y) => x + y)
  private val SUBTRACT = BinaryInfixOperator("-", (x, y) => x - y)
  private val MULTIPLY = BinaryInfixOperator("*", (x, y) => x * y)
  private val DIVIDE = BinaryInfixOperator("/", (x, y) => x / y)
  private val POWER = BinaryInfixOperator("^", (x, y) => math.pow(x, y))
  private val MAX = BinaryPrefixOperator("Max", (x, y) => math.max(x, y))
  private val MIN = BinaryPrefixOperator("Min", (x, y) => math.min(x, y))
  private val standardFunctions = Array[Expression](NEGATE, SQRT, ABS, SIGN, FLOOR, CEILING, ROUND, ADD, SUBTRACT, MULTIPLY, DIVIDE, POWER, MAX, MIN)

  private val A_N_MINUS_ONE = PreviousElement(1)
  private val A_N_MINUS_TWO = PreviousElement(2)
  private val A_N_MINUS_THREE = PreviousElement(3)
  private val previousElements = Array[Expression](A_N_MINUS_ONE, A_N_MINUS_TWO, A_N_MINUS_THREE)

  private val FACTORIAL = UnaryPrefixOperator("Factorial", x =>
    if (x.isWhole && 0 <= x && x <= 20)
      Tables.factorials(x.toInt)
    else Double.NaN)
  private val DOUBLE_FACTORIAL = UnaryPrefixOperator("Factorial2", x =>
    if (x.isWhole && 0 <= x && x <= 20)
      Tables.doubleFactorials(x.toInt)
    else Double.NaN)
  private val CATALAN_NUMBER = UnaryPrefixOperator("CatalanNumber", x =>
    if (x.isWhole && 0 <= x && x <= 20)
      Tables.catalanNumbers(x.toInt)
    else Double.NaN)
  private val BINOMIAL_COEFFICIENT = BinaryPrefixOperator("Binomial", (x, y) =>
    if (x.isWhole && 0 <= x && x <= 20 && y.isWhole && 0 <= y && y <= x)
      Tables.binomialCoefficients(x.toInt)(y.toInt)
    else Double.NaN)
  private val combinatorialFunctions = Array[Expression](FACTORIAL, DOUBLE_FACTORIAL, CATALAN_NUMBER, BINOMIAL_COEFFICIENT)

  private val PRIME = UnaryPrefixOperator("Prime", x =>
    if (x.isWhole && 1 <= x && x <= 1000)
      Tables.primes(x.toInt)
    else Double.NaN)
  private val EULER_PHI = UnaryPrefixOperator("EulerPhi", x =>
    if (x.isWhole && 1 <= x && x <= 1000)
      Tables.eulerTotients(x.toInt)
    else Double.NaN)
  private val BERNOULLI_NUMBER = UnaryPrefixOperator("BernoulliB", x =>
    if (x.isWhole && 0 <= x && x <= 20)
      Tables.bernoulliNumbers(x.toInt)
    else Double.NaN)
  private val numberTheoreticFunctions = Array[Expression](PRIME, EULER_PHI, BERNOULLI_NUMBER)

  private val PI = Number("Pi", math.Pi)
  private val E = Number("E", math.E)
  private val EXP = UnaryPrefixOperator("Exp", x => math.exp(x))
  private val LOG = UnaryPrefixOperator("Log", x => math.log(x))
  private val SIN = UnaryPrefixOperator("Sin", x => math.sin(x))
  private val COS = UnaryPrefixOperator("Cos", x => math.cos(x))
  private val TAN = UnaryPrefixOperator("Tan", x => math.tan(x))
  private val transcendentalFunctions = Array[Expression](PI, E, EXP, LOG, SIN, COS, TAN)

  private val expressions =
    (integers ++ indexFunctions ++ standardFunctions ++
      (if (configuration.recurrenceRelations) previousElements else Array[Expression]()) ++
      (if (configuration.combinatorialFunctions) combinatorialFunctions else Array[Expression]()) ++
      (if (configuration.numberTheoreticFunctions) numberTheoreticFunctions else Array[Expression]()) ++
      (if (configuration.transcendentalFunctions) transcendentalFunctions else Array[Expression]()))
      // Rarely used expressions are excluded from search to improve performance
      .diff(Array[Expression](SEVEN, EIGHT, NINE, E, SQRT, SIGN, CEILING, ROUND, EXP, LOG, TAN, MAX, MIN))

  // Assign each expression a unique identifier
  expressions.zipWithIndex.foreach({ case (expression, i) => expression.id = i })

  val maxArgumentCount = expressions.map(_.getArgumentCount).max

  val maxStartIndex = expressions.map(_.getStartIndex).max

  // Grouped by number of arguments
  private val groupedExpressions = (0 to maxArgumentCount).map(argumentCount =>
    expressions.filter(_.getArgumentCount == argumentCount)).toArray

  private val nonNegativeIntegerExpressions = integers ++ indexFunctions ++
    combinatorialFunctions ++ Array[Expression](PRIME, EULER_PHI)

  private val integerExpressions = nonNegativeIntegerExpressions ++ Array[Expression](FLOOR, CEILING, ROUND)

  private val nonIntegerExpressions = transcendentalFunctions ++ Array[Expression](BERNOULLI_NUMBER)

  // Exclusion rules defining which expressions may not be used as arguments for the key expression
  private val disallowedArguments: Map[Expression, Array[Expression]] = Map(
    // -(-x) = x
    NEGATE -> Array[Expression](NEGATE),
    ABS -> (Array[Expression](NEGATE, ABS, PI, E) ++ nonNegativeIntegerExpressions),
    FLOOR -> integerExpressions,
    CEILING -> integerExpressions,
    ROUND -> integerExpressions,
    // 1*x = x*1 = x
    MULTIPLY -> Array[Expression](ONE),
    // 1^x = 1, x^1 = x
    POWER -> Array[Expression](ONE),
    FACTORIAL -> (integers ++ nonIntegerExpressions),
    DOUBLE_FACTORIAL -> (integers ++ nonIntegerExpressions),
    CATALAN_NUMBER -> (integers ++ nonIntegerExpressions),
    BINOMIAL_COEFFICIENT -> nonIntegerExpressions,
    PRIME -> (integers ++ nonIntegerExpressions),
    EULER_PHI -> (integers ++ nonIntegerExpressions),
    BERNOULLI_NUMBER -> (integers ++ nonIntegerExpressions),
    EXP -> (integerExpressions ++ previousElements ++ nonIntegerExpressions),
    LOG -> (integerExpressions ++ previousElements ++ nonIntegerExpressions),
    SIN -> (integerExpressions ++ previousElements ++ nonIntegerExpressions),
    COS -> (integerExpressions ++ previousElements ++ nonIntegerExpressions),
    TAN -> (integerExpressions ++ previousElements ++ nonIntegerExpressions))

  private def getAllowedExpressions(parentExpression: Expression, argumentCount: Int) =
    groupedExpressions(argumentCount).diff(disallowedArguments.getOrElse(parentExpression, Array[Expression]()))

  private def getLookupIndex(parentExpression: Expression, argumentCount: Int) =
    (parentExpression.id * (maxArgumentCount + 1)) + argumentCount

  // Build lookup table "parentExpression x argumentCount -> allowedExpressions"
  private val allowedExpressions = Array.ofDim[Array[Expression]](expressions.size * (maxArgumentCount + 1))
  expressions.foreach(parentExpression => (0 to maxArgumentCount).foreach(argumentCount =>
    allowedExpressions(getLookupIndex(parentExpression, argumentCount)) = getAllowedExpressions(parentExpression, argumentCount)))

  def getExpressions(node: Node): Array[Expression] = {
    val argumentCount = node.children.size
    // Using nulls instead of Options is not idiomatic Scala, but here it is raw speed that matters
    // as this method gets called from the innermost search loop
    if (node.parent == null) {
      groupedExpressions(argumentCount)
    } else {
      val parentExpression = node.parent.expression
      if (parentExpression == EmptyExpression)
        groupedExpressions(argumentCount)
      else
        allowedExpressions(getLookupIndex(parentExpression, argumentCount))
    }
  }
}
