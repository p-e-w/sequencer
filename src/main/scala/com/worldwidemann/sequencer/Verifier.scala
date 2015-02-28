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

object Verifier {
  private val testThreshold = math.pow(10, -10)

  // Verifies formula numerically
  def testFormula(formula: Node, startIndex: Int, sequence: Seq[Double]): Boolean = {
    for (index <- startIndex to sequence.size) {
      try {
        val value = formula.evaluate(index, sequence)
        if (!Utilities.isNumerical(value) || math.abs(value - sequence(index - 1)) > testThreshold)
          return false
      } catch {
        // Arithmetic exceptions etc. indicate that the formula is invalid
        case e: Exception => return false
      }
    }

    true
  }

  // Verifies formula symbolically
  def verifyFormula(formula: Node, startIndex: Int, sequence: Seq[String]): Boolean = {
    for (index <- startIndex to sequence.size) {
      var equation = formula.toString.replace("(n)", "(" + index + ")")
      // Substitute values of previous elements
      for (offset <- 1 to startIndex - 1)
        equation = equation.replace("(a" + offset + ")", "(" + sequence(index - offset - 1) + ")")
      equation += "==" + sequence(index - 1)

      try {
        if (Utilities.evaluateSymja(equation) != "True")
          return false
      } catch {
        // Arithmetic exceptions etc. indicate that the formula is invalid
        case e: Exception => return false
      }
    }

    true
  }
}