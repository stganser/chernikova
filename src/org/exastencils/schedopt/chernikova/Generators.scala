package org.exastencils.schedopt.chernikova

import Chernikova.V

case class Generators(hLines: Set[V], hRays: Set[V]) {

  // Initialize from generators in non-homogeneous coordinates,
  // i.e., lines, rays and vertices.
  def this(nhLines: Set[V], nhRays: Set[V], nhVertices: Set[V]) =
    this(nhLines.map(_ :+ (0:BigInt)),
         nhRays.map(_ :+ (0:BigInt)) ++ nhVertices.map(_ :+ (1:BigInt))
        )

  // Lines in homogeneous coordinates
  val homogeneousLines: Set[V] = hLines

  // Rays in homogeneous coordinates
  val homogeneousRays: Set[V] = hRays

  private def dropLast(v: V): V = v.slice(0, v.length - 1)

  // Lines in ordinary coordinates
  val lines: Set[V] = homogeneousLines map dropLast

  // Rays in ordinary coordinates
  val rays: Set[V] = homogeneousRays filter (_.last == 0) map dropLast

  // Vertices in ordinary coordinates given as
  // (numerators of coordinates, common denominator) pairs
  val vertices: Set[(V, BigInt)] =
    homogeneousRays filter (_.last != 0) map ((r: V) => (dropLast(r), r.last))

  override def toString(): String = {
    var sb: StringBuilder = new StringBuilder
    sb =
      sb.append("Lines = ")
        .append(lines.map(_.mkString("(", ",", ")")).mkString("{ ", "; ", " }"))
        .append('\n')
        .append("Rays = ")
        .append(rays.map(_.mkString("(", ",", ")")).mkString("{ ", "; ", " }"))
        .append('\n')

    def mkCoeff(n: BigInt, d: BigInt): String = {
      if (d == 1)
        n.toString
      else
        n.toString + "/" + d.toString
    }
    sb = sb.append("Vertices = ")
      .append(vertices.map((vd: (V, BigInt)) => vd._1.map(mkCoeff(_, vd._2)))
        .map(_.mkString("(", ",", ")")).mkString("{ ", "; ", " }"))
    sb.toString()
  }
}
