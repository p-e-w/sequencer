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

trait Expression {
  def evaluate(arguments: Seq[Double], index: Int, sequence: Seq[Double]): Double
  def render(arguments: Seq[String]): String
  // Returns the index from which on the general part of the formula applies
  def getStartIndex = 1
}

case object EmptyExpression extends Expression {
  def evaluate(arguments: Seq[Double], index: Int, sequence: Seq[Double]) = 0
  def render(arguments: Seq[String]) = if (arguments.isEmpty) "O" else "O(" + arguments.mkString(",") + ")"
}

case class IndexFunction(symbol: String, f: Int => Double) extends Expression {
  def evaluate(arguments: Seq[Double], index: Int, sequence: Seq[Double]) = f(index)
  def render(arguments: Seq[String]) = "(" + symbol + ")"
}

case class PreviousElement(offset: Int) extends Expression {
  def evaluate(arguments: Seq[Double], index: Int, sequence: Seq[Double]) =
    if (index > offset)
      sequence(index - offset - 1)
    else Double.NaN
  def render(arguments: Seq[String]) = "(a" + offset + ")"
  override def getStartIndex = offset + 1
}

case class Number(symbol: String, value: Double) extends Expression {
  def evaluate(arguments: Seq[Double], index: Int, sequence: Seq[Double]) = value
  def render(arguments: Seq[String]) = "(" + symbol + ")"
}

case class UnaryPrefixOperator(symbol: String, f: Double => Double) extends Expression {
  def evaluate(arguments: Seq[Double], index: Int, sequence: Seq[Double]) = f(arguments(0))
  def render(arguments: Seq[String]) = symbol + "(" + arguments(0) + ")"
}

case class BinaryInfixOperator(symbol: String, f: (Double, Double) => Double) extends Expression {
  def evaluate(arguments: Seq[Double], index: Int, sequence: Seq[Double]) = f(arguments(0), arguments(1))
  def render(arguments: Seq[String]) = "(" + arguments(0) + ")" + symbol + "(" + arguments(1) + ")"
}

case class BinaryPrefixOperator(symbol: String, f: (Double, Double) => Double) extends Expression {
  def evaluate(arguments: Seq[Double], index: Int, sequence: Seq[Double]) = f(arguments(0), arguments(1))
  def render(arguments: Seq[String]) = symbol + "(" + arguments(0) + "," + arguments(1) + ")"
}