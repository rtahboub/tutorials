package scala.lms.tutorial

import _root_.java.awt.geom.Point2D
import _root_.java.awt.geom.Rectangle2D

import scala.lms.common._



class GridTest2 extends TutorialFunSuite {
  val under = "grid_test_"


  test("grid1") {

    object Engine { //extends SimpleQueryProcessor with SQLParser { // SQL support not needed

      trait QueryCompiler extends Dsl with ScannerLowerBase {

        class Scanner(name: Rep[String]) {
          val fd = open(name)
          val fl = filelen(fd)
          val data = mmap[Char](fd, fl)
          var pos = 0

          def next(d: Rep[Char]) = {
            val start = pos: Rep[Int] // force read
            while (data(pos) != d) pos += 1
            val len = pos - start
            pos += 1
            RString(stringFromCharArray(data, start, len), len)
          }

          def nextInt(d: Rep[Char]) = {
            val start = pos: Rep[Int] // force read
            var num = 0
            while (data(pos) != d) {
              num = num * 10 + (data(pos) - '0').toInt
              pos += 1
            }
            pos += 1
            num: Rep[Int] //RInt(num)
          }

          def nextDouble(d: Rep[Char]) = {
            println("TODO: implement nextDouble")
            // adapt from here:
            // https://github.com/TiarkRompf/legobase-micro/blob/master/minidb/src/main/scala/Scanner.scala#L49
            val first = nextInt('.')
            val second = nextInt(d)
            first.asInstanceOf[Rep[Double]]
          }


          def hasNext = pos < fl

          def done = close(fd)
        }

        abstract class RField {
          def print()

          def compare(o: RField): Rep[Boolean]

          def hash: Rep[Long]
        }


        case class RString(data: Rep[String], len: Rep[Int]) extends RField {
          def print() = prints(data)

          def compare(o: RField) = o match {
            case RString(data2, len2) => if (len != len2) false
            else {
              // TODO: we may or may not want to inline this (code bloat and icache considerations).
              var i = 0
              while (i < len && data.charAt(i) == data2.charAt(i)) {
                i += 1
              }
              i == len
            }
          }

          def hash = data.HashCode(len)
        }

        // not used at the moment
        case class RInt(value: Rep[Int]) extends RField {
          def print() = printf("%d", value)

          def compare(o: RField) = o match {
            case RInt(v2) => value == v2
          }

          def hash = value.asInstanceOf[Rep[Long]]
        }

        case class Point(key: Rep[Int], x: Rep[Double], y: Rep[Double])
        case class Rectangle(x: Rep[Double], y: Rep[Double], width: Rep[Double], height: Rep[Double]) {
          def isEmpty() = width <= 0.0 || height <= 0.0
          def contains(x: Rep[Double], y: Rep[Double]): Rep[Boolean] = {
            if (isEmpty()) {
              false
            } else {
              val x1 = this.x;
              val y1 = this.y;
              val x2 = x1 + this.width;
              val y2 = y1 + this.height;
              x1 <= x && x < x2 && y1 <= y && y < y2
            }
          }
          def contains(p: Point): Rep[Boolean] = contains(p.x, p.y)
          def contains(r: Rectangle): Rep[Boolean] = { println("TODO: implement contains"); false }
          def intersects(r: Rectangle): Rep[Boolean] = { println("TODO: implement intersects"); false }
          /* TODO: translate from java, like above 
          see here for Rectangle2D source code: https://android.googlesource.com/platform/frameworks/native/+/edbf3b6/awt/java/awt/geom/Rectangle2D.java
          public boolean intersects(double x, double y, double width, double height) {
              if (isEmpty() || width <= 0.0 || height <= 0.0) {
                  return false;
              }
              double x1 = getX();
              double y1 = getY();
              double x2 = x1 + getWidth();
              double y2 = y1 + getHeight();
              return x + width > x1 && x < x2 && y + height > y1 && y < y2;
          }
          public boolean contains(double x, double y, double width, double height) {
              if (isEmpty() || width <= 0.0 || height <= 0.0) {
                  return false;
              }
              double x1 = getX();
              double y1 = getY();
              double x2 = x1 + getWidth();
              double y2 = y1 + getHeight();
              return x1 <= x && x + width <= x2 && y1 <= y && y + height <= y2;
          }
          */
        }

        trait GridAttributes{
          val spaceSide = 22361
          val numAgents = 50000
          val numCells = (numAgents / 296).toInt
          val cellsPerSide = (math.sqrt(numCells)).toInt
          val cellSize = (spaceSide / cellsPerSide).toInt
          val maxPoints = (1 << 20)
          val gridSize1D = cellsPerSide * cellsPerSide
        }

        class Grid extends GridAttributes {
          val keys:  Rep[Array[Int]]    = NewArray[Int](maxPoints)
          val xs:    Rep[Array[Double]] = NewArray[Double](maxPoints)
          val ys:    Rep[Array[Double]] = NewArray[Double](maxPoints)
          val nexts: Rep[Array[Int]]    = NewArray[Int](maxPoints)
          var numPoints = 0

          val grid:  Rep[Array[Int]]    = NewArray[Int](gridSize1D)
          for (i <- 0 until gridSize1D)
            grid(i) = -1

          def getPoint(n: Rep[Int]) = Point(keys(n), xs(n), ys(n))
          def getPointList(n0: Rep[Int]) = new {
            def foreach(f: Point => Rep[Unit]) = {
              var n = n0
              while (n >= 0) {
                f(getPoint(n))
                n = nexts(n)
              }
            }
          }

          def accessGrid(x: Rep[Int],y: Rep[Int]) = {
            val gridPos = y * cellsPerSide + x
            getPointList(grid(gridPos))
          }

          def addPoint(p: Point) = {
            if (numPoints >= maxPoints) println("ERROR: maxPoints exceeded")
            val xbin = (p.x / cellSize).toInt
            val ybin = (p.y / cellSize).toInt
            val gridPos = ybin * cellsPerSide + xbin
            val next = grid(gridPos)
            grid(gridPos) = numPoints
            keys(numPoints) = p.key
            xs(numPoints) = p.x
            ys(numPoints) = p.y
            nexts(numPoints) = next
            numPoints += 1
          }

          // note: signature changed to join/lookup only 1 point (called in a loop from method snippet)
          def nestedIndexWindowJoin(p1: Point, xDel: Double, yDel: Double)(f: Point => Rep[Unit]) = {
            val window = Rectangle(p1.x - xDel, p1.y - yDel, 2 * xDel, 2 * yDel)
            //     println(t1(i).x, t1(i).y, window)

            // TODO: be smarter about iteration space
            for (a <- 0 until cellsPerSide){
              for (b <- 0 until cellsPerSide){
                val gridcell = Rectangle(cellSize * a, cellSize * b, cellSize, cellSize)
                if (window.contains(gridcell)){
                  //           println("contains", a, b)
                  //report all points
                  accessGrid(a,b).foreach(f)
                }
                else if(window.intersects(gridcell)){
                  //           println("intersects", a, b)
                  for(c <- accessGrid(a,b))
                    if(window.contains(c)) {
                      f(c)
                      //               println(t1(i).x, t1(i).y, c)
                      //               println(result)
                    }

                }
              }
            }
          }
        }

        def processCSV(filename: String)(f: Point => Rep[Unit]) = {
          val in = new Scanner(filename)
          while (in.hasNext) {
            val id   = in.nextInt(',')
            val x   = in.nextDouble(',')
            val y  = in.nextDouble('\n')
            f(Point(id,x,y))
          }
          in.done
        }

        // main function - code generated from everything inside gets compiled
        def snippet(x: Rep[String]): Rep[Unit] = {
          val grid2 = new Grid()
          processCSV("src/data/test10_t2.csv") { p2 => 
            grid2.addPoint(p2) }
          processCSV("src/data/test10_t1.csv") { p1 =>
            grid2.nestedIndexWindowJoin(p1,11.26, 11.26) { p3 => 
              printf("id: %d, x: %f, y: %f",p3.key,p3.x,p3.y) }}
        }
      }

      def run = {
        val snippet = new LMS_Driver[String,Unit] with QueryCompiler
        println(snippet.code)
        println("--- now running: ---")
        //   snippet.precompile
        utils.time {
          snippet.eval("empty")
        }

      }

    }

    exec("out", utils.captureOut(Engine.run), suffix="txt")
  }


  /*trait SimpleQueryProcessor extends PlainQueryProcessor with SQLParser {
    def dynamicFilePath(table: String): Table = table
    def execQuery(q: Operator): Unit = ???
    def version: String = "simple"
  }*/


  abstract class LMS_Driver[A:Manifest,B:Manifest] extends DslDriverC[A,B] with  ScannerLowerExp{ q =>
    override val codegen = new DslGenC with CGenScannerLower {
      val IR: q.type = q
    }
  }

}