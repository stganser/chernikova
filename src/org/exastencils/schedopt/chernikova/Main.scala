package org.exastencils.schedopt.chernikova

import java.net.URL

import isl.Conversions._

object Main {

  private var loaded: Boolean = false
  def load() : Unit = {

    if (loaded)
      return

    val system: String =
      System.getProperty("os.name") match {
        case x if (x.startsWith("Windows")) => "win32"
        case x if (x.startsWith("Linux"))   => "linux"
        case x if (x.startsWith("Mac"))     => "darwin"
        case x if (x.startsWith("Darwin"))  => "darwin"
        case x =>
          throw new Exception("unknown operating system (" + x + "), cannot load native library isl")
      }
    val arch: String =
      System.getProperty("os.arch") match {
        case "amd64"     => "x86_64"
        case "x86_64"    => "x86_64"
        case "i386"      => "x86"
        case "powerpc64" => "ppc64"
        case x =>
          throw new Exception("unknown system architecture (" + x + "), cannot load native library isl")
      }

    val ldir: String = system + '-' + arch
    val lname: String = System.mapLibraryName("isl_jni")
    val lurl: URL = ClassLoader.getSystemResource(ldir + '/' + lname)
    if (lurl == null)
      throw new Error("unable to locate native library " + ldir + '/' + lname)

    lurl.getProtocol() match {
      case "file" =>
        val lfile: String = lurl.getPath()
        val lpath: String = lfile.substring(0, lfile.lastIndexOf('/'))
        isl.Init.loadNative(lpath)

      case "jar" =>
        throw new Error("extracting native library from a jar file is not supported for this test")
    }

    loaded = true
  }

  final lazy val ctx = {
    this.load()
    isl.Ctx.alloc()
  }

  def main(args: Array[String]): Unit = {
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
