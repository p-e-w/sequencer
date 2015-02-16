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
  // Verifies formula numerically
  def testFormula(formula: Node, sequence: Seq[Double]): Boolean = {
    for (index <- Utilities.getStartIndex(formula) to sequence.size) {
      try {
        if (formula.evaluate(index, sequence) != sequence(index - 1))
          return false
      } catch {
        // Arithmetic exceptions etc. indicate that the formula is invalid
        case e: Exception => return false
      }
    }

    true
  }

  // Verifies formula symbolically
  def verifyFormula(formula: Node, sequence: Seq[Double]): Boolean = {
    val startIndex = Utilities.getStartIndex(formula)

    for (index <- startIndex to sequence.size) {
      var equation = formula.toString.replace("(n)", "(" + index + ")")
      // Substitute values of previous elements
      for (offset <- 1 to startIndex - 1)
        equation = equation.replace("(a" + offset + ")", Utilities.getSymbolicForm(sequence(index - offset - 1)))
      equation += "==" + Utilities.getSymbolicForm(sequence(index - 1))

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