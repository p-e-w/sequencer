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

class FormulaGenerator(expressionLibrary: ExpressionLibrary) {
  private def rollNodeExpression(node: Node) = {
    val allowedExpressions = expressionLibrary.getExpressions(node)
    val size = allowedExpressions.size
    node.expressionIndex = if (size == 0) 0 else ((node.expressionIndex + 1) % size)
    node.expression = if (size == 0) EmptyExpression else allowedExpressions(node.expressionIndex)
    // Indicate whether we rolled over or not
    node.expressionIndex == 0
  }

  // Generates all formulas based on the specified expression tree skeleton.
  // The callback pattern is much more efficient than returning a collection
  // because the number of formulas grows extremely fast with the number of nodes
  def getFormulas(tree: Node, formulaCallback: (Node, Int) => Unit) {
    // Retrieves the nodes by post-order traversal, meaning that a parent node
    // always succeeds all of its children.
    // This is important because it ensures that the loop below rolls a node's expression
    // only if all of the node's children have just rolled over to 0. If it were otherwise,
    // the values of the child nodes' expressionIndex fields would be meaningless as the
    // allowed expressions for a node depend on the expression in the node's parent
    val nodes = tree.getTreeNodes
    val nodesSize = nodes.size

    // Set initial expressions
    nodes.foreach(rollNodeExpression)

    // For each start index, counts how often it occurs in the formula
    val startIndexCount = Array.ofDim[Int](expressionLibrary.maxStartIndex + 1)
    nodes.foreach(node => startIndexCount(node.expression.getStartIndex) += 1)

    var startIndex = nodes.map(_.expression.getStartIndex).max

    var lastRolled = false
    while (!lastRolled) {
      // Pass the formula in its current form.
      // The object is mutable, so any processing has to happen in the callback
      formulaCallback(tree, startIndex)

      var i = 0
      var rolled = true
      while (i < nodesSize && rolled) {
        val node = nodes(i)
        val oldStartIndex = node.expression.getStartIndex
        rolled = rollNodeExpression(node)
        val newStartIndex = node.expression.getStartIndex

        startIndexCount(oldStartIndex) -= 1
        startIndexCount(newStartIndex) += 1

        // The start index is computed here because doing so while already iterating
        // is faster than having to iterate again over the entire collection later
        if (newStartIndex > startIndex) {
          // The formula's start index is the maximum of the constituent expressions' start indices
          startIndex = newStartIndex
        } else if (oldStartIndex == startIndex && startIndexCount(oldStartIndex) == 0) {
          // An expression that had the single maximum start index before
          // now fell below the maximum, so we need to recompute
          for (j <- 1 until oldStartIndex) {
            if (startIndexCount(j) > 0)
              startIndex = j
          }
        }

        i += 1
        if (i == nodesSize && rolled)
          lastRolled = true
      }
    }
  }
}