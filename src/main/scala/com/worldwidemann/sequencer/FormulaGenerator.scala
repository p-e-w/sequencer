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

import org.apache.commons.math3.util.CombinatoricsUtils

class FormulaGenerator(configuration: Configuration) {
  // Note: Rarely used expressions are disabled to increase search speed
  private val expressions = List(
    // Atomic expressions
    List(
      Number("1", 1),
      Number("2", 2),
      Number("3", 3),
      Number("4", 4),
      Number("5", 5),
      //Number("6", 6),
      //Number("7", 7),
      //Number("8", 8),
      //Number("9", 9),
      Number("10", 10),
      // Common functions of the index variable are hardcoded as atomic expressions
      // in order to speed up searches for formulas that contain them
      IndexFunction("(n)", n => n),
      IndexFunction("(n)+1", n => n + 1),
      IndexFunction("(n)-1", n => n - 1),
      IndexFunction("2*(n)", n => 2 * n),
      IndexFunction("(n)^2", n => n * n),
      IndexFunction("2^(n)", n => math.pow(2, n))) ++
      (if (configuration.recurrenceRelations) List(
        PreviousElement(1),
        PreviousElement(2),
        PreviousElement(3))
      else List()) ++
      (if (configuration.transcendentalFunctions) List(
        Number("Pi", math.Pi) //,
        //Number("E", math.E)
        )
      else List()),

    // Unary operators
    List(
      UnaryPrefixOperator("-", x => -x),
      //UnaryPrefixOperator("Sqrt", x => math.sqrt(x)),
      UnaryPrefixOperator("Abs", x => math.abs(x)),
      //UnaryPrefixOperator("Sign", x => math.signum(x)),
      UnaryPrefixOperator("Floor", x => math.floor(x)) //,
      //UnaryPrefixOperator("Ceiling", x => math.ceil(x)),
      //UnaryPrefixOperator("Round", x => math.round(x))
      ) ++
      (if (configuration.combinatorialFunctions) List(
        UnaryPrefixOperator("Factorial", x => {
          if (x.isWhole && 0 <= x && x <= 10)
            CombinatoricsUtils.factorialDouble(x.toInt)
          else throw new IllegalArgumentException("Argument " + x + " is invalid for the Factorial function")
        }))
      else List()) ++
      (if (configuration.transcendentalFunctions) List(
        //UnaryPrefixOperator("Exp", x => math.exp(x)),
        //UnaryPrefixOperator("Log", x => math.log(x)),
        UnaryPrefixOperator("Sin", x => math.sin(x)),
        UnaryPrefixOperator("Cos", x => math.cos(x)) //,
        //UnaryPrefixOperator("Tan", x => math.tan(x))
        )
      else List()),

    // Binary operators
    List(
      BinaryInfixOperator("+", (x, y) => x + y),
      BinaryInfixOperator("-", (x, y) => x - y),
      BinaryInfixOperator("*", (x, y) => x * y),
      BinaryInfixOperator("/", (x, y) => x / y),
      BinaryInfixOperator("^", (x, y) => math.pow(x, y)) //,
      //BinaryPrefixOperator("Max", (x, y) => math.max(x, y)),
      //BinaryPrefixOperator("Min", (x, y) => math.min(x, y))
      ) ++
      (if (configuration.combinatorialFunctions) List(
        BinaryPrefixOperator("Binomial", (x, y) => {
          if (x.isWhole && 0 <= x && x <= 10 && y.isWhole && 0 <= y && y <= x)
            CombinatoricsUtils.binomialCoefficientDouble(x.toInt, y.toInt)
          else throw new IllegalArgumentException("Arguments " + x + ", " + y + " are invalid for the Binomial function")
        }))
      else List()))

  private def rollNodeExpression(node: Node) = {
    val allowedExpressions = expressions(node.children.size)
    node.expressionIndex = if (node.expressionIndex + 1 >= allowedExpressions.size) 0 else node.expressionIndex + 1
    node.expression = allowedExpressions(node.expressionIndex)
    // Indicate whether we rolled over or not
    node.expressionIndex == 0
  }

  // The callback pattern is much more efficient than returning a collection
  // because the number of formulas grows extremely fast with the number of nodes
  def getFormulas(nodes: Int, formulaCallback: Node => Unit, progressCallback: Double => Unit) = {
    val trees = new TreeGenerator(expressions.size - 1).getTrees(nodes)

    for ((tree, i) <- trees.view.zipWithIndex) {
      val nodes = tree.getTreeNodes
      // Set initial expressions
      nodes.foreach(node => rollNodeExpression(node))

      var lastRolled = false
      while (!lastRolled) {
        // Pass the formula in its current form.
        // The object is mutable, so any processing has to happen in the callback
        formulaCallback(tree)

        var j = 0
        var rolled = true
        while (j < nodes.size && rolled) {
          rolled = rollNodeExpression(nodes(j))
          j += 1
          if (j == nodes.size && rolled)
            lastRolled = true
        }
      }

      progressCallback((i + 1).toDouble / trees.size)
    }
  }
}