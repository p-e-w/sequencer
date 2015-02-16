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

class Node {
  var expression: Expression = EmptyExpression
  // Ensures that the initial roll selects the first valid expression (see FormulaGenerator)
  var expressionIndex = -1

  val children = new ListBuffer[Node]

  def getCopy: Node = {
    val copy = new Node
    copy.expression = expression
    copy.expressionIndex = expressionIndex
    children.foreach(child => {
      copy.children += child.getCopy
    })
    copy
  }

  // Returns all nodes in the tree
  def getTreeNodes: Seq[Node] = {
    val treeNodes = new ListBuffer[Node]
    treeNodes += this
    children.foreach(child => {
      treeNodes ++= child.getTreeNodes
    })
    treeNodes
  }

  def evaluate(index: Int, sequence: Seq[Double]): Double =
    expression.evaluate(children.map(child => child.evaluate(index, sequence)), index, sequence)

  override def toString = expression.render(children.map(child => child.toString))

  override def equals(that: Any): Boolean = {
    that match {
      // Consider two nodes equal if their associated trees are structurally equal
      case that: Node => that.isInstanceOf[Node] && that.toString == toString
      case _          => false
    }
  }

  override def hashCode: Int = toString.hashCode
}