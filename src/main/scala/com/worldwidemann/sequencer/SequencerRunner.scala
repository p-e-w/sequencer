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

import java.io.OutputStream
import java.io.PrintStream

import scala.collection.mutable.ListBuffer

import scopt.OptionParser

object SequencerRunner {
  def main(args: Array[String]) {
    println("Sequencer 1.5.0 (https://github.com/p-e-w/sequencer)\n")

    // Suppress annoying Symja console output (idea from http://stackoverflow.com/a/8363580).
    // This is a very brittle solution. In particular, if we do not use the Console stream
    // at least once before redirecting System.out, println calls will be swallowed too
    val dummyStream = new PrintStream(new OutputStream {
      override def write(b: Int) = {}
    })
    System.setOut(dummyStream)
    System.setErr(dummyStream)

    val sequence = new ListBuffer[String]

    val parser = new OptionParser[Configuration]("Sequencer") {
      opt[Int]('d', "depth") action { (x, c) =>
        c.copy(maximumComplexity = x)
      } text ("search depth (maximum number of nodes in expression tree) [default: 6]")
      opt[Int]('r', "results") action { (x, c) =>
        c.copy(maximumIdentifications = x)
      } text ("maximum number of formulas to return, 0 for unlimited [default: 5]. " +
        "Note that for parallel searches, the order in which formulas are found is not deterministic " +
        "so limiting the number of search results can lead to unreproducible searches " +
        "unless the --sequential option is also used.")
      opt[Int]('p', "predict") action { (x, c) =>
        c.copy(predictionLength = x)
      } text ("number of elements to predict in sequence continuation [default: 5]")
      opt[Int]('u', "recurrence-depth") action { (x, c) =>
        c.copy(recurrenceDepth = x)
      } text ("maximum number of previous elements to consider for recurrence relations [default: 3]")
      opt[Unit]('c', "no-combinatorics") action { (x, c) =>
        c.copy(combinatorialFunctions = false)
      } text ("do not search for combinatorial functions (speeds up search)")
      opt[Unit]('n', "no-number-theory") action { (x, c) =>
        c.copy(numberTheoreticFunctions = false)
      } text ("do not search for number theoretic functions (speeds up search)")
      opt[Unit]('t', "no-transcendentals") action { (x, c) =>
        c.copy(transcendentalFunctions = false)
      } text ("do not search for transcendental functions (speeds up search)")
      opt[Unit]('q', "sequential") action { (x, c) =>
        c.copy(parallelSearch = false)
      } text ("disable search parallelization (single-threaded search)")
      opt[Unit]('s', "symbolic") action { (x, c) =>
        c.copy(numericalTest = false)
      } text ("skip numerical test (symbolic verification only; slows down search)")
      opt[Unit]('o', "hide-progress") action { (x, c) =>
        c.copy(printProgress = false)
      } text ("do not output progress while searching")
      opt[Unit]('l', "latex") action { (x, c) =>
        c.copy(outputLaTeX = true)
      } text ("output LaTeX instead of plain text")
      arg[String]("a_1, a_2, ...") unbounded () action { (x, c) =>
        sequence += x
        c.copy()
      } validate { x =>
        if (Utilities.isNumber(x)) success else failure("'" + x + "' can not be evaluated numerically")
      } text ("list of numbers to search for (symbolic expressions allowed)")
    }

    parser.parse(args, Configuration(6, 5, 5, 3, true, true, true, true, true, true, false)) match {
      case Some(configuration) => {
        println("Searching for formulas for sequence " +
          (if (configuration.outputLaTeX)
            "$(a_n)_{n\\geq 1} = " + Utilities.formatSequence(sequence.map(Utilities.getLaTeX), true) + "$"
          else
            "(a(n)) = " + Utilities.formatSequence(sequence, false)))

        val time = System.currentTimeMillis

        val identifications = new Sequencer(configuration).identifySequence(sequence)
        identifications.foreach(identification => {
          println("\n" + (if (configuration.outputLaTeX) "$$" else "") + identification.formula +
            (if (configuration.outputLaTeX) "$$" else ""))
          println("Continuation: " + (if (configuration.outputLaTeX) "$" else "") +
            Utilities.formatSequence(identification.continuation, configuration.outputLaTeX) +
            (if (configuration.outputLaTeX) "$" else ""))
        })

        if (identifications.isEmpty)
          println("\nSearch space exhausted. No formula generating the sequence was found.")

        println("\nTime: " + (System.currentTimeMillis - time) + " ms")
      }
      // Invalid command line
      case None => {}
    }
  }
}