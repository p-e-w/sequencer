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

import java.text.DecimalFormat

import org.matheclipse.core.eval.EvalUtilities

object Utilities {
  private val evaluator = new EvalUtilities(false, true)

  def evaluateSymja(expression: String) = evaluator.evaluate(expression).toString

  def isDouble(string: String): Boolean = {
    try {
      string.toDouble
      return true
    } catch {
      case e: Exception => return false
    }
  }

  private val format = new DecimalFormat("#.#")

  def formatNumber(number: Double) = format.format(number)

  // Rewrites number as a fraction of integers to prevent Symja from entering numerical mode
  def getSymbolicForm(number: Double): String = {
    val stringForm = formatNumber(number).split("\\.")
    if (stringForm.size == 1)
      return "(" + stringForm(0) + ")"
    "(" + stringForm(0) + stringForm(1) + "/" + math.pow(10, stringForm(1).length).round + ")"
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