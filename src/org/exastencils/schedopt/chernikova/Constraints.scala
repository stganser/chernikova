package org.exastencils.schedopt.chernikova

import Chernikova.V

import isl.Conversions._

class Constraints(eqs: Set[V], ineqs: Set[V]) {
  val equalities:   Set[V] = eqs
  val inequalities: Set[V] = ineqs
}

object Constraints {
  private def lcm(a:BigInt, b:BigInt): BigInt = (a/a.gcd(b))*b

  def fromBasicSet(bset: isl.BasicSet): Constraints = {
    var eqs:   Set[V] = Set.empty
    var ineqs: Set[V] = Set.empty

    def putConstraint(c: isl.Constraint): Unit = {
      val aff: isl.Aff = c.getAff
      val nI = aff.getSpace.dim(isl.DimType.In)
      val nP = aff.getSpace.dim(isl.DimType.Param)
      val coeffs: Vector[isl.Val] = (0 to nI-1).to[Vector].map((i:Int) => aff.getCoefficientVal(isl.DimType.In, i)) ++
        (0 to nP-1).to[Vector].map((i:Int) => aff.getCoefficientVal(isl.DimType.Param, i)) :+ aff.getConstantVal

      val d: BigInt = coeffs.map((x:isl.Val) => new BigInt(x.getDen)).reduceLeft((a,b) => lcm(a,b))
      val icoeffs: V = coeffs.map(c => new BigInt(c.getNum)*(d/new BigInt(c.getDen)))
      if (c.isEquality)
        eqs += icoeffs
      else
        ineqs += icoeffs
    }

    bset.foreachConstraint( putConstraint(_: isl.Constraint) )

    new Constraints(eqs, ineqs)
  }
}
