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

import scala.collection.mutable.ListBuffer

object Predictor {
  def predict(formula: Node, sequence: Seq[String], elements: Int) = {
    val sequenceNew = new ListBuffer[String]
    sequenceNew ++= sequence
    val startIndex = Utilities.getStartIndex(formula)

    (sequence.size + 1 to sequence.size + elements).map(index => {
      var formulaNew = formula.toString.replace("(n)", "(" + index + ")")
      // Substitute values of previous elements
      for (offset <- 1 to startIndex - 1)
        formulaNew = formulaNew.replace("(a" + offset + ")", "(" + sequenceNew(index - offset - 1) + ")")

      val newElement = Simplifier.simplify(formulaNew)
      if (!Utilities.isNumber(newElement))
        throw new RuntimeException("Unable to predict sequence for formula '" + formulaNew.toString + "'")
      sequenceNew += newElement
      newElement
    })
  }
}