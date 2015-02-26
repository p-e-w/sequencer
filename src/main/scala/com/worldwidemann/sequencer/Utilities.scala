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

import java.io.StringWriter

import org.matheclipse.core.eval.EvalEngine
import org.matheclipse.core.eval.EvalUtilities
import org.matheclipse.core.eval.TeXUtilities

object Utilities {
  // Symja isn't thread safe, so the EvalUtilities object can not be shared between calls
  def evaluateSymja(expression: String) = new EvalUtilities(false, true).evaluate(expression).toString

  def isNumerical(value: Double) = !value.isNaN && !value.isInfinite

  def getNumericalValue(expression: String) = evaluateSymja("(" + expression + ") + 0.0").toDouble

  def isNumber(expression: String) = try {
    isNumerical(getNumericalValue(expression))
  } catch {
    case e: Exception => false
  }

  def getLaTeX(expression: String) = {
    val writer = new StringWriter
    new TeXUtilities(new EvalEngine(true), true).toTeX(expression, writer)
    writer.toString
  }

  def formatSequence(sequence: Seq[String], latex: Boolean) = {
    if (latex)
      (sequence :+ "\\ldots").mkString(",\\; ")
    else
      (sequence :+ "...").mkString(", ")
  }

  // Retrieves the index from which on the general part of the formula applies
  // (1 for non-recurrence formulas)
  def getStartIndex(formula: Node) = {
    val offsets = formula.getTreeNodes
      .map(_.expression)
      .filter(_.isInstanceOf[PreviousElement])
      .map(_.asInstanceOf[PreviousElement].offset)
    if (offsets.isEmpty) 1 else offsets.max + 1
  }
}