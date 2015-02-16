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

object Simplifier {
  // Simplifications that Symja cannot do because it does not know that n is a positive integer
  private val simplificationRules = List(
    ("Abs(n)", "(n)"),
    ("Floor(n)", "(n)"),
    ("Ceiling(n)", "(n)"),
    ("Round(n)", "(n)"),
    ("Log(E^n)", "(n)"),
    ("Log(E^(-n))", "(-n)"),
    ("Sign(n)", "(1)"),
    ("Sign(E^n)", "(1)"),
    ("Abs(Log(n))", "(Log(n))"),
    ("Abs(E^n)", "(E^n)"),
    ("Abs(n!)", "(n!)"),
    ("Floor(n!)", "(n!)"),
    ("Ceiling(n!)", "(n!)"),
    ("Round(n!)", "(n!)"))

  private def simplifySymja(expression: String) = Utilities.evaluateSymja("FullSimplify(" + expression + ")")

  def simplify(formula: String) = {
    var oldFormula = formula
    var newFormula = formula

    do {
      newFormula = simplifySymja(newFormula)
      oldFormula = newFormula
      simplificationRules.foreach(rule => {
        newFormula = newFormula.replace(rule._1, rule._2)
      })
    } while (newFormula != oldFormula)

    simplifySymja(newFormula)
  }
}