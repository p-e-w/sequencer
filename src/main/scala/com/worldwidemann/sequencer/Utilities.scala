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

import org.matheclipse.core.eval.EvalUtilities

object Utilities {
  private val evaluator = new EvalUtilities(false, true)

  def evaluateSymja(expression: String) = evaluator.evaluate(expression).toString

  def isNumerical(value: Double) = !value.isNaN && !value.isInfinite

  def getNumericalValue(expression: String) = evaluateSymja("(" + expression + ") + 0.0").toDouble

  def isNumber(expression: String) = try {
    isNumerical(getNumericalValue(expression))
  } catch {
    case e: Exception => false
  }

  // Retrieves the index from which on the generic part of the formula applies
  // (1 for non-recurrence formulas)
  def getStartIndex(formula: Node) = {
    val offsets = formula.getTreeNodes
      .map(_.expression)
      .filter(_.isInstanceOf[PreviousElement])
      .map(_.asInstanceOf[PreviousElement].offset)
    if (offsets.isEmpty) 1 else offsets.max + 1
  }
}