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
  def main(args: Array[String]): Unit = {
    println("Sequencer 1.0.0 (https://github.com/p-e-w/sequencer)\n")

    // Suppress annoying Symja console output (idea from http://stackoverflow.com/a/8363580).
    // This is a very brittle solution. In particular, if we do not use the Console stream
    // at least once before redirecting System.out, println calls will be swallowed too
    System.setOut(new PrintStream(new OutputStream {
      override def write(b: Int) = {}
    }))

    val sequence = new ListBuffer[Double]

    val parser = new OptionParser[Configuration]("Sequencer") {
      opt[Int]('d', "depth") action { (x, c) =>
        c.copy(maximumComplexity = x)
      } text ("search depth (maximum number of nodes in expression tree) [default: 6]")
      opt[Int]('r', "results") action { (x, c) =>
        c.copy(maximumIdentifications = x)
      } text ("maximum number of formulas to return, 0 for unbounded [default: 5]")
      opt[Int]('p', "predict") action { (x, c) =>
        c.copy(predictionLength = x)
      } text ("number of elements to predict in sequence continuation [default: 5]")
      opt[Unit]('u', "no-recurrences") action { (x, c) =>
        c.copy(recurrenceRelations = false)
      } text ("do not search for recurrence relations (speeds up search)")
      opt[Unit]('c', "no-combinatorics") action { (x, c) =>
        c.copy(combinatorialFunctions = false)
      } text ("do not search for combinatorial functions (speeds up search)")
      opt[Unit]('t', "no-transcendentals") action { (x, c) =>
        c.copy(transcendentalFunctions = false)
      } text ("do not search for transcendental functions (speeds up search)")
      opt[Unit]('s', "symbolic") action { (x, c) =>
        c.copy(numericalTest = false)
      } text ("skip numerical test (symbolic verification only; slows down search)")
      arg[Double]("a_1, a_2, ...") unbounded () action { (x, c) =>
        sequence += x
        c.copy()
      } text ("list of numbers to search for")
    }

    parser.parse(args, Configuration(6, 5, 5, true, true, true, true, true)) match {
      case Some(configuration) => {
        println("Searching for formulas for sequence (a(n)) = " + sequence.map(Utilities.formatNumber).mkString(", ") + ", ...")

        val time = System.currentTimeMillis

        val identifications = new Sequencer(configuration).identifySequence(sequence)
        identifications.foreach(identification => {
          println("\n" + identification.formula)
          println("Continuation: " + identification.continuation.map(Utilities.formatNumber).mkString(", ") + ", ...")
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