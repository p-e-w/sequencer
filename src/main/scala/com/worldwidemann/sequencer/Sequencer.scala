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

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable.SynchronizedQueue
import scala.collection.generic.Growable

case class Configuration(maximumComplexity: Int, maximumIdentifications: Int, predictionLength: Int,
                         recurrenceRelations: Boolean, combinatorialFunctions: Boolean, numberTheoreticFunctions: Boolean, transcendentalFunctions: Boolean,
                         parallelSearch: Boolean, numericalTest: Boolean, printProgress: Boolean, outputLaTeX: Boolean)

case class SequenceIdentification(formula: String, continuation: Seq[String])

class Sequencer(configuration: Configuration) {
  // Note that neither of the two classes keeps a state,
  // so sharing these objects is thread safe
  private val formulaGenerator = new FormulaGenerator(configuration)
  private val treeGenerator = new TreeGenerator(formulaGenerator.getMaxChildren)

  def identifySequence(sequence: Seq[String]): Seq[SequenceIdentification] = {
    val sequenceSimplified = sequence.map(Simplifier.simplify)
    val sequenceNumerical = sequenceSimplified.map(Utilities.getNumericalValue)

    val identifications = new SynchronizedQueue[SequenceIdentification]

    for (nodes <- 1 to configuration.maximumComplexity) {
      val trees = treeGenerator.getTrees(nodes)
      val progress = new AtomicInteger(0)

      // TreeGenerator generates tree objects that are completely independent of each other,
      // so searching for formulas based on them can be parallelized
      (if (configuration.parallelSearch) trees.par else trees).foreach(tree => {
        identifySequenceTask(tree, sequenceSimplified, sequenceNumerical, identifications)

        if (configuration.printProgress && !maximumReached(identifications))
          // TODO: Concurrency issues with interleaving print statements?
          print("\rTrying formulas with complexity " + nodes + "... " +
            "%3d".format(((progress.incrementAndGet.toDouble / trees.size) * 100).round) + " %")
      })

      if (maximumReached(identifications))
        return processIdentifications(identifications)

      if (configuration.printProgress)
        println
    }

    processIdentifications(identifications)
  }

  // Parallel execution unit.
  // Finds formulas generating the sequence based on the specified expression tree skeleton
  private def identifySequenceTask(tree: Node, sequence: Seq[String], sequenceNumerical: Seq[Double],
                                   identifications: Seq[SequenceIdentification] with Growable[SequenceIdentification]) {
    formulaGenerator.getFormulas(tree, (formula, startIndex) => {
      // Consider recurrence relations only if they predict at least one element
      // of the sequence without referencing a seed value
      if (sequence.size > 2 * (startIndex - 1)) {
        if (!configuration.numericalTest || Verifier.testFormula(formula, startIndex, sequenceNumerical)) {
          // Sequence matched numerically (or test skipped) => verify symbolically
          if (Verifier.verifyFormula(formula, startIndex, sequence)) {
            try {
              val continuation = Predictor.predict(formula, startIndex, sequence, configuration.predictionLength)
                .map(element => if (configuration.outputLaTeX) Utilities.getLaTeX(element) else element)
              identifications += SequenceIdentification(getFullFormula(formula, startIndex, sequence), continuation)
            } catch {
              // Occasionally, simplification or prediction throw an exception although
              // symbolic verification did not. This indicates a bug in Symja
              // and is simply ignored
              case e: Exception => {}
            }
          }
        }
      }

      if (maximumReached(identifications))
        return
    })
  }

  private def maximumReached(identifications: Seq[SequenceIdentification]) =
    // Check number of elements before checking number of distinct elements (performance)
    configuration.maximumIdentifications > 0 && identifications.size >= configuration.maximumIdentifications &&
      identifications.distinct.size >= configuration.maximumIdentifications

  private def processIdentifications(identifications: Seq[SequenceIdentification]) = {
    // Sort alphabetically before sorting by length to get deterministic ordering independent of search order
    val sortedIdentifications = identifications.distinct.sortBy(_.formula).sortBy(_.formula.length)
    if (configuration.maximumIdentifications > 0)
      // Necessary because the tasks might find additional formulas
      // before they determine that the maximum has been reached and return
      sortedIdentifications.take(configuration.maximumIdentifications)
    else
      sortedIdentifications
  }

  // Returns the full descriptive string form of the formula,
  // including seed values for recurrence relations
  private def getFullFormula(formula: Node, startIndex: Int, sequence: Seq[String]) = {
    val builder = new StringBuilder

    var generalPart = Simplifier.simplify(formula.toString)
    if (configuration.outputLaTeX) {
      builder.append("\\begin{align}\n")
      generalPart = Utilities.getLaTeX(generalPart)
    }

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