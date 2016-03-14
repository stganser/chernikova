package org.exastencils.schedopt.chernikova

import isl.Conversions._

object Chernikova {
  type V = Vector[BigInt]

  def generatorsToConstraints(ctx: isl.Ctx, nVars: Int, nParams: Int, gens: Set[Generators]): isl.Set = {
    val univ: isl.Set = isl.BasicSet.universe(isl.Space.setAlloc(ctx, nParams, nVars))
    val sets: Set[isl.Set] = gens.map((g: Generators) =>
      isl.Set.fromBasicSet(generatorsToConstraints(ctx, nVars, nParams, g)))
    val res: isl.Set = sets.foldLeft(univ)(_.union(_))
    res
  }

  def generatorsToConstraints(ctx: isl.Ctx, nVars: Int, nParams: Int, gens: Generators): isl.BasicSet = {
    val lines: Set[V] = gens.homogeneousLines
    val rays: Set[V] = gens.homogeneousRays

    val sp: isl.Space = isl.Space.setAlloc(ctx, nParams, nVars)

    if (lines.isEmpty && rays.isEmpty)
      return isl.BasicSet.empty(sp)

    assert(!(lines ++ rays).exists(_.size != nVars + nParams + 1))

    val (l: Set[V], r: Set[V]) = chernikova(lines ++ lines.map(_.map(-_)) ++ rays)
    val r1: Set[V] = r filter (_.init.exists(_ != 0))

    val ls: isl.LocalSpace = isl.LocalSpace.fromSpace(sp)
    val zero: isl.Aff = isl.Aff.zeroOnDomain(ls)

    def vecToAff(v: V): isl.Aff = {
      def setCoeff(a: isl.Aff, dt: isl.DimType, i: Int, j: Int): isl.Aff =
        a.setCoefficientVal(dt, i, isl.Val.fromBigInteger(ctx, v(j).bigInteger))
      val vs: isl.Aff = (0 to nVars - 1).foldLeft(zero)((aff, i) => setCoeff(aff, isl.DimType.In, i, i))
      val ps: isl.Aff = (0 to nParams - 1).foldLeft(vs)((aff, i) => setCoeff(aff, isl.DimType.Param, i, nParams + i))
      ps.setConstantVal(isl.Val.fromBigInteger(ctx, v(nVars + nParams).bigInteger))
    }

    val eqs: Set[isl.BasicSet] = l.map(vecToAff(_).zeroBasicSet)
    val ineqs: Set[isl.BasicSet] = r1.map(vecToAff(_).geBasicSet(zero))
    val univ: isl.BasicSet = isl.BasicSet.universe(sp)
    val res: isl.BasicSet = (eqs ++ ineqs).fold(univ)(_.intersect(_))
    res
  }

  def constraintsToGenerators(s: isl.Set): Set[Generators] = {
    var bsets: Set[isl.BasicSet] = Set.empty
    s.foreachBasicSet((bs: isl.BasicSet) => bsets += bs)
    bsets map constraintsToGenerators collect { case Some(g) => g }
  }

  def constraintsToGenerators(bset: isl.BasicSet): Option[Generators] = {
    val constrs: Constraints = Constraints.fromBasicSet(bset)
    constraintsToGenerators(constrs.equalities, constrs.inequalities)
  }

  def constraintsToGenerators(eqs: Set[V], ineqs: Set[V]): Option[Generators] = {
    var ineqsE: Set[V] = ineqs ++ eqs
    if (eqs.size > 0) {
      // add negated sum of equalities to constraints
      val teqs: Vector[V] = eqs.to[Vector].transpose
      ineqsE += teqs.map((v: V) => v.reduceLeft(_ + _)).map(-_)
    }
    val n: Int = ineqsE.head.length
    val pos1: V = Vector.fill(n - 1)(0: BigInt).to[Vector] :+ (1: BigInt)
    val (lines, rays) = chernikova(ineqsE + pos1)
    if (rays.exists(_.last > 0))
      Some(new Generators(lines, rays))
    else
      None
  }

  private def chernikova(constraints: Set[V]): (Set[V], Set[V]) = {
    val dim: Int = constraints.head.length
    val maxIdx: Int = constraints.size + dim - 1

    def unitV(i: Int): V = Vector.fill(dim)(0: BigInt) updated (i, 1: BigInt)
    val initLines: Set[V] =
      ((0 to dim - 1).to[Vector].map(unitV(_)) ++ constraints.to[Vector]).transpose.to[Set]

    def step(lr: (Set[V], Set[V]), c: Int): (Set[V], Set[V]) = chernikovaStep(dim, lr._1, lr._2, c)
    val (lines, rays) = (maxIdx to (dim, -1)).foldLeft((initLines, Set.empty: Set[V]))(step)

    def deleteConstrs(c: V): V = c.slice(0, dim)
    (lines.map(deleteConstrs(_)), rays.map(deleteConstrs(_)))
  }

  private def chernikovaStep(dim: Int, lines: Set[V], rays: Set[V], c: Int): (Set[V], Set[V]) = {
    if (Thread.interrupted())
      throw new InterruptedException()
    def cancel(c: V): V = {
      val f: BigInt = c.reduce((x, y) => x.gcd(y)).abs
      c map ((_: BigInt) / f)
    }

    findGtLine(lines, c) match {
      case Some((gtLine, otherLines)) =>
        val c_gtLine: BigInt = gtLine(c)
        // Note that c_gtLine>0 holds.
        def project(y: V): V =
          cancel((y.map(_ * c_gtLine), gtLine.map(_ * y(c))).zipped map (_ - _))

        (otherLines map project, (rays map project) + gtLine)
      case None =>
        val (neg, zero, pos) = partitionBySign(rays, c)

        def deleteC(v: V): V = v.slice(0, c) ++ v.drop(c + 1)
        if (neg.size == 0)
          (lines map deleteC, (pos ++ zero) map deleteC)
        else {
          val maxIdx: Int = (lines ++ rays).head.length - 1
          def s(y: V): Vector[Int] = (c + 1 to maxIdx).to[Vector].filter(y(_) == 0)

          val satRows: Map[V, Vector[Int]] = rays.map((r: V) => (r, s(r))).toMap

          // Form a positive combination. Note that y1(c)>0 and y2(c)<0 holds,
          // so y2*y1(c) - y1*y2(c) is a positive combination.
          def combine(y1: V, y2: V): V =
            cancel((y2.map(_ * y1(c)), y1.map(_ * y2(c))).zipped map (_ - _))
          val combs: Set[(V, V, V)] = for (y1 <- pos; y2 <- neg) yield (y1, y2, combine(y1, y2))

          val t: Int = lines.size
          def adjacent(arg: (V, V, V)): Boolean = {
            val y1: V = arg._1
            val y2: V = arg._2
            val y: V = arg._3
            val sy: Vector[Int] = s(arg._3)
            if (sy.length <= dim - t - 3)
              false
            else {
              val sat: Set[Vector[Int]] = (satRows - y1 - y2).values.to[Set]
              !sat.exists((sa: Vector[Int]) => sy.diff(sa).isEmpty)
            }
          }
          val qbar1 = combs.filter(adjacent(_))
          val qbar: Set[V] = qbar1 map { case (_, _, y) => y }
          (lines, pos ++ zero ++ qbar)
        }
    }
  }

  private def findGtLine(lines: Set[V], c: Int): Option[(V, Set[V])] = {
    lines.find(_(c) != 0) match {
      case Some(l) => if (l(c) > 0) Some(l, lines - l) else Some(l.map(_ * (-1)), lines - l)
      case None => None
    }
  }

  private def partitionBySign(rays: Set[V], c: Int): (Set[V], Set[V], Set[V]) = {
    val (ge, lt) = rays.partition(_(c) >= 0)
    val (gt, eq) = ge.partition(_(c) > 0)
    (lt, eq, gt)
  }

  def testWith(bs: isl.BasicSet): Boolean = {
    val optG: Option[Generators] = constraintsToGenerators(bs)
    // bs is empty iff !(optG is None)
    if (bs.isEmpty && !optG.isDefined)
      return true
    if (!optG.isDefined)
      return false

    val g: Generators = optG.get
    val nV: Int = bs.dim(isl.DimType.Set)
    val nP: Int = bs.dim(isl.DimType.Param)
    val bs2: isl.BasicSet = generatorsToConstraints(bs.getCtx, nV, nP, g)
    if (false && !bs2.isEqual(bs)) {
      println("Error: dual of dual is not primal")
      return false
    }

    var constrs: Constraints = Constraints.fromBasicSet(bs)
    val eqs: Set[V] = constrs.equalities
    val ineqs: Set[V] = constrs.inequalities

    def dot(a: V, b: V): BigInt = (a, b).zipped.map(_ * _).reduceLeft(_ + _)

    for (e <- eqs) {
      for (l <- g.homogeneousLines) {
        if (dot(l, e) != 0) {
          println("Equality " + e + " violated by line " + l)
          return false
        }
      }
      for (r <- g.homogeneousRays) {
        if (dot(r, e) != 0) {
          println("Equality " + e + " violated by ray " + r)
          return false
        }
      }
    }

    for (i <- ineqs) {
      for (l <- g.homogeneousLines) {
        if (dot(l, i) != 0) {
          println("Inquality " + i + " violated by line " + l)
          return false
        }
      }
    }

    for (r <- g.homogeneousRays) {
      var hasNonZero: Boolean = false
      for (i <- ineqs) {
        val d: BigInt = dot(r, i)
        if (d < 0) {
          println("Inquality " + i + " violated by ray " + r)
          return false
        } else if (d > 0) {
          hasNonZero = true
        }
      }
      // For each ray apart from (0..0,1), there must be an inequality which
      // the ray verifies but does not saturate (i.e., d > 0 above).
      // Otherwise, the ray would have to be a line.
      if (!hasNonZero && r.init.exists(_ > 0)) {
        println("Ray " + r + " saturates all constraints")
        return false
      }
    }
    true
  }
}
