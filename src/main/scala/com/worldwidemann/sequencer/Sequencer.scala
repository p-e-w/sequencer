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
                         numericalTest: Boolean, printProgress: Boolean, outputLaTeX: Boolean)

case class SequenceIdentification(formula: String, continuation: Seq[String])

class Sequencer(configuration: Configuration) {
  def identifySequence(sequence: Seq[String]): Seq[SequenceIdentification] = {
    val sequenceSimplified = sequence.map(Simplifier.simplify)
    val sequenceNumerical = sequenceSimplified.map(Utilities.getNumericalValue)

    val identifications = new ListBuffer[SequenceIdentification]

    for (nodes <- 1 to configuration.maximumComplexity) {
      new FormulaGenerator(configuration).getFormulas(nodes, formula => {
        // Consider recurrence relations only if they predict at least one element
        // of the sequence without referencing a seed value
        if (sequence.size > 2 * (Utilities.getStartIndex(formula) - 1)) {
          if (!configuration.numericalTest || Verifier.testFormula(formula, sequenceNumerical)) {
            // Sequence matched numerically (or test skipped) => verify symbolically
            if (Verifier.verifyFormula(formula, sequenceSimplified)) {
              try {
                identifications += SequenceIdentification(getFullFormula(formula, sequenceSimplified),
                  Predictor.predict(formula, sequenceSimplified, configuration.predictionLength))
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

    var generalPart = Simplifier.simplify(formula.toString)
    if (configuration.outputLaTeX) {
      builder.append("\\begin{align}\n")
      generalPart = Utilities.getLaTeX(generalPart)
    }

    val startIndex = Utilities.getStartIndex(formula)
    for (index <- 1 to startIndex - 1) {
      if (configuration.outputLaTeX) {
        builder.append("a_").append(index).append(" &= ").append(Utilities.getLaTeX(sequence(index - 1))).append(" \\\\")
        // Note that we can not rely on "a1" etc. being surrounded by brackets anymore
        // as the formula has already been simplified
        generalPart = generalPart.replace("a" + index, "a_{n-" + index + "}")
      } else {
        builder.append("a(").append(index).append(") = ").append(sequence(index - 1))
        generalPart = generalPart.replace("a" + index, "a(n-" + index + ")")
      }
      builder.append("\n")
    }

    if (configuration.outputLaTeX)
      builder.append("a_n &= ")
    else
      builder.append("a(n) = ")
    builder.append(generalPart)

    if (startIndex > 1) {
      if (configuration.outputLaTeX) {
        builder.append("\\quad \\text{for } n\\geq ").append(startIndex)
      } else {
        builder.append("   for n >= ").append(startIndex)
      }
    }

    if (configuration.outputLaTeX)
      builder.append("\n\\end{align}")

    builder.toString
  }
}