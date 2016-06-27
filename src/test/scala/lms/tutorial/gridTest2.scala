package scala.lms.tutorial

import _root_.java.awt.geom.Point2D
import _root_.java.awt.geom.Rectangle2D

import _root_.Engine.QueryCompiler.PointList

import scala.lms.common._



class QueryLiveStepsTest3 extends TutorialFunSuite {
  val under = "query_live_"



  test("demo9") {

    object Engine extends SimpleQueryProcessor with SQLParser {

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
            RInt(num)
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

        case class RInt(value: Rep[Int]) extends RField {
          def print() = printf("%d", value)

          def compare(o: RField) = o match {
            case RInt(v2) => value == v2
          }

          def hash = value.asInstanceOf[Rep[Long]]
        }

        // class Record(fields: Vector[Rep[String]], schema: Vector[String]) {
        case class Record(fields: Vector[RField], schema: Vector[String]) {
          def apply(name: String): RField = fields(schema indexOf name)

          def apply(names: Schema): Vector[RField] = names map (this apply _)
        }

        def processCSV(file: Rep[String], schema: Vector[String])(yld: Record => Unit): Unit = {
          val in = new Scanner(file)
          in.next('\n')
          while (in.hasNext) {
            val fields = schema map (n => in.next(if (n == schema.last) '\n' else ','))
            yld(Record(fields, schema))
          }
        }

        def evalRef(p: Ref)(rec: Record): RField = p match {
          case Value(x) => RString(x.toString,x.toString.length)
          case Field(name) => rec(name)
        }

        def evalPred(p: Predicate)(rec: Record): Rep[Boolean] = p match {
          case Eq(a, b) => evalRef(a)(rec) == evalRef(b)(rec)
        }

        def execOp(op: Operator)(yld: Record => Unit): Unit = op match {
          case Scan(file, schema, _, _) => processCSV(file, schema)(yld)
          case PrintCSV(parent) => execOp(parent) { rec => printFields(rec.fields) }
          case Project(outSchema, inSchema, parent) =>
            execOp(parent) { rec => yld(Record(rec(inSchema), outSchema)) }
          case Filter(pred, parent) =>
            execOp(parent) { rec => if (evalPred(pred)(rec)) yld(rec) }
        }

        //      def printFields(fields: Vector[RField]) = printf(fields.map(_ => "%s").mkString("", ",", "\n"), fields: _*)
        def printFields(fields: Vector[RField]) = {
          if (fields.nonEmpty) {
            fields.head.print
            fields.tail.foreach { x  => printf(defaultFieldDelimiter.toString); x.print }
          }
          println("")
        }


        import java.awt.geom.Rectangle2D
        import java.awt.geom.Point2D

        case class Point(id: Rep[Int], x: Rep[Double], y: Rep[Double])

        trait GridAttributes{
          val spaceSide = 22361
          val numAgents = 50000
          val numCells = (numAgents / 296).toInt
          val cellsPerSide = (math.sqrt(numCells)).toInt
          val cellSize = (spaceSide / cellsPerSide).toInt
          val globalSize = cellsPerSide * cellsPerSide
        }

        class Grid extends GridAttributes {

          val ids:   Rep[Array[Int]]    = NewArray[Int](globalSize)
          val xs:    Rep[Array[Double]] = NewArray[Double](globalSize)
          val ys:    Rep[Array[Double]] = NewArray[Double](globalSize)
          val nexts: Rep[Array[Int]]    = NewArray[Int](globalSize)

          val grid:  Rep[Array[Int]]    = NewArray[Int](globalSize)

          case class PointList(head: Point, next: Int) {
            def tail: Option[PointList] = getPointList(next)
          }

          def getPoint(n: Int) = Point(ids(n), xs(n), ys(n))
          def getPointList(n: Int) = if (n < 0) None else Some(PointList(getPoint(n), nexts(n)))

          def accessGrid(x: Int, y: Int): Option[PointList] = getPointList(grid(y * cellsPerSide + x))
          def updateGrid(x: Int, y: Int)(p: Point) = {
            val n = y * cellsPerSide + x
            val pl = accessGrid(x,y)
            pl match {
              case None =>
                nexts(n) = -1
              case Some(tail) =>
                nexts(n) = -1
            }
            ids(n) = p.id
            xs(n) = p.x
            ys(n) = p.y
          }


          def addPoint(p: Point): Array[Array[List[A]]] = {
            val g = Array.fill(cellsPerSide, cellsPerSide)(Nil: List[A])

              val xbin = (p.x / cellSize).toInt
              val ybin = (p.y / cellSize).toInt
              updateGrid(xbin,ybin)(p)
          }

          def printGrid(): Unit = {
            for (i <- 0 to cellsPerSide - 1) {
              print(i, " ")
              for (j <- 0 to cellsPerSide - 1)
                print(grid(i)(j), " ")
              println()
            }
          }

          def nestedIndexWindowJoin(t1: List[Point], xDel: Double, yDel: Double): List[Point] = {
            var result = List[Point]()
            for (i <- 0 to t1.length - 1) {
              val window = new Rectangle2D.Double(t1(i).x - xDel, t1(i).y - yDel, 2 * xDel, 2 * yDel )
              //     println(t1(i).x, t1(i).y, window)

              for (a <- 0 to cellsPerSide - 1){
                for (b <- 0 to cellsPerSide - 1){
                  val gridcell = new Rectangle2D.Double(cellSize * a, cellSize * b, cellSize, cellSize)
                  if (window.contains(gridcell)){
                    //           println("contains", a, b)
                    //report all points
                    result :::= grid(a)(b)
                  }
                  else if(window.intersects(gridcell)){
                    //           println("intersects", a, b)
                    for(c <- grid(a)(b))
                      if(window.contains(new Point2D.Double(c.x,c.y))) {
                        result ::= c
                        //               println(t1(i).x, t1(i).y, c)
                        //               println(result)
                      }

                  }
                }
              }
            }
            result
          }
        }

        object driver{
          def processCSV(filename: String): List[Point] = {
            var result = List[Point]()
            val in = new Scanner(filename)
            while (in.hasNext) {
              val id   = in.next(',').toInt
              val x   = in.next(',').toDouble
              val y  = in.next('\n').toDouble
              result ::= Point(id,x,y)
            }
            in.close

            result
          }

          def main(args: Array[String]): Unit = {
            val t1 = processCSV("src/data/test10_t1.csv")
            val t2 = processCSV("src/data/test10_t2.csv")
            val x = new Grid(t2)
            val r = x.nestedIndexWindowJoin(t1,11.26, 11.26)
            println(r)
          }
        }




      }

      def run = {
        val snippet = new LMS_Driver[String,Unit] with QueryCompiler {

          def snippet(x: Rep[String]): Rep[Unit] = {
            val ops = parseSql("select time,room,title from src/data/talks.csv where time = '09:00 AM'")
            execOp(PrintCSV(ops)) { _ => }
          }
        }

        println(snippet.code)
        println("--- now running: ---")
        //   snippet.precompile
        utils.time {
          snippet.eval("")
        }

      }

    }

    exec("data", utils.captureOut(Engine.run), suffix="csv")
  }


  trait SimpleQueryProcessor extends PlainQueryProcessor with SQLParser {
    def dynamicFilePath(table: String): Table = table
    def execQuery(q: Operator): Unit = ???
    def version: String = "simple"
  }


  abstract class LMS_Driver[A:Manifest,B:Manifest] extends DslDriverC[A,B] with  ScannerLowerExp{ q =>
    override val codegen = new DslGenC with CGenScannerLower {
      val IR: q.type = q
    }
  }

  /*abstract class LMS_Driver[A:Typ,B:Typ] extends DslDriver[A,B] with ScannerExp
    with StagedEngine with MainEngine with query_staged.QueryCompiler { q =>
      override val codegen = new DslGen with ScalaGenScanner {
        val IR: q.type = q
      }
  }*/


}

//def c_engine =
//new DslDriverC[String,Unit] with ScannerLowerExp
//with StagedEngine with MainEngine with query_optc.QueryCompiler { q =>
//  override val codegen = new DslGenC with CGenScannerLower {
//  val IR: q.type = q
//}
//  override def snippet(fn: Table): Rep[Unit] = run
//  override def prepare: Unit = {}
//  override def eval: Unit = eval(filename)
//}
