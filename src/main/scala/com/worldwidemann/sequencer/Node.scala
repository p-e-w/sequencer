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

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer

class Node(val parent: Node) {
  var expression: Expression = EmptyExpression
  // Ensures that the initial roll selects the first valid expression (see FormulaGenerator)
  var expressionIndex = -1

  val children = new ListBuffer[Node]

  def getCopy(parent: Node): Node = {
    val copy = new Node(parent)
    copy.expression = expression
    copy.expressionIndex = expressionIndex
    copy.children ++= children.map(_.getCopy(copy))
    copy
  }

  // Returns all nodes in the tree
  def getTreeNodes: Seq[Node] = {
    // ArrayBuffer rather than ListBuffer because fast random access is needed
    val treeNodes = new ArrayBuffer[Node]
    children.foreach(child => treeNodes ++= child.getTreeNodes)
    // Post-order traversal (see FormulaGenerator for why this is important)
    treeNodes += this
    treeNodes
  }

  def evaluate(index: Int, sequence: Seq[Double]): Double =
    expression.evaluate(children.map(_.evaluate(index, sequence)), index, sequence)

  override def toString = expression.render(children.map(_.toString))

  override def equals(that: Any): Boolean = {
    that match {
      // Consider two nodes equal if their associated trees are structurally equal
      case that: Node => that.isInstanceOf[Node] && that.toString == toString
      case _          => false
    }
  }

  override def hashCode: Int = toString.hashCode
}