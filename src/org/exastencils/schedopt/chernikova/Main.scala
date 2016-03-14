package org.exastencils.schedopt.chernikova

import Chernikova.V

import isl.Conversions._
import isl.Isl

object Main {
  def main(args: Array[String]): Unit = {
    val ctx: isl.Ctx = Isl.ctx
    val s: isl.Set =
      if (args.length > 0) isl.Set.readFromStr(ctx, args(0))
      else isl.Set.readFromStr(ctx, "[n] -> { [i] : 5 <= i <= n and i <= 50; [i] : 100 <= i <= 200 }")
    var bsets: Set[isl.BasicSet] = Set.empty
    s.foreachBasicSet((bs: isl.BasicSet) => bsets += bs)
    println(bsets)
    val res:Set[Generators] = Chernikova.constraintsToGenerators(s)

    res map ((g:Generators) => { println("==="); println(g.toString()) })

    val nV: Int = s.dim(isl.DimType.Set)
    val nP: Int = s.dim(isl.DimType.Param)
    val ineqs: Set[isl.BasicSet] = res.map(Chernikova.generatorsToConstraints(ctx, nV, nP, _))
    println(ineqs)

    val ineqs2: isl.BasicSet =
      Chernikova.generatorsToConstraints(ctx, 1, 1, new Generators(Set(), Set(Vector(0,1)), Set(Vector(5,5), Vector(50,50))) )
    println(ineqs)
    println("-------")

    val t: isl.BasicSet = isl.BasicSet.readFromStr(ctx, "{ [i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i6, i11, i8, i13, i14] : i11 <= i7 and i9 <= i0 - i3 + i5 and i9 <= i0 + i1 - i3 - i4 + i5 and i9 <= i1 - i4 + i5 and i9 <= i5 and i14 <= i5 + i7 - i9 - i11 + i13 and i2 >= 0 }")
    println(t)
    print("Test: " + Chernikova.testWith(t))
  }
}
