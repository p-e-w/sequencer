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

case class Configuration(maximumComplexity: Int, maximumIdentifications: Int, predictionLength: Int,
                         recurrenceRelations: Boolean, combinatorialFunctions: Boolean, transcendentalFunctions: Boolean,
                         numericalTest: Boolean, printProgress: Boolean)

case class SequenceIdentification(formula: String, continuation: Seq[String])

class Sequencer(configuration: Configuration) {
  def identifySequence(sequence: Seq[String]): Seq[SequenceIdentification] = {
    val sequenceNumerical = sequence.map(Utilities.getNumericalValue)

    val identifications = new ListBuffer[SequenceIdentification]

    for (nodes <- 1 to configuration.maximumComplexity) {
      new FormulaGenerator(configuration).getFormulas(nodes, formula => {
        if (!configuration.numericalTest || Verifier.testFormula(formula, sequenceNumerical)) {
          // Sequence matched numerically (or test skipped) => verify symbolically
          if (Verifier.verifyFormula(formula, sequence)) {
            try {
              identifications += SequenceIdentification(getFullFormula(formula, sequence),
                Predictor.predict(formula, sequence, configuration.predictionLength))
              if (configuration.maximumIdentifications > 0 && identifications.distinct.size >= configuration.maximumIdentifications)
                return identifications.distinct.sortBy(_.formula.length)
            } catch {
              // Occasionally, simplification or prediction throw an exception although
              // symbolic verification did not. This indicates a bug in Symja
              // and is simply ignored
              case e: Exception => {}
            }
          }
        }
      }, progress => {
        if (configuration.printProgress)
          print("\rTrying formulas with complexity " + nodes + "... " + "%3d".format((progress * 100).round) + " %")
      })

      if (configuration.printProgress)
        println
    }

    identifications.distinct.sortBy(_.formula.length)
  }

  // Returns the full descriptive string form of the formula,
  // including seed values for recurrence relations
  private def getFullFormula(formula: Node, sequence: Seq[String]) = {
    val builder = new StringBuilder
    var generalTerm = formula.toString
    val startIndex = Utilities.getStartIndex(formula)
    for (index <- 1 to startIndex - 1) {
      builder.append("a(").append(index).append(") = ").append(Simplifier.simplify(sequence(index - 1))).append("\n")
      generalTerm = generalTerm.replace("(a" + index + ")", "(a((n)-" + index + "))")
    }
    builder.append("a(n) = ").append(Simplifier.simplify(generalTerm)).append("   for n >= ").append(startIndex).toString
  }
}