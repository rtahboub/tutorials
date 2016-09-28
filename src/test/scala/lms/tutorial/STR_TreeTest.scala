package scala.lms.tutorial

import _root_.java.awt.geom.Point2D
import _root_.java.awt.geom.Rectangle2D

import scala.lms.common._

class STR_TreeTest extends TutorialFunSuite {
  val under = "str_tree_test_"

  test("STR_Tree") {

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

            //println("TODO: implement nextDouble")
            // adapt from here:
            // https://github.com/TiarkRompf/legobase-micro/blob/master/minidb/src/main/scala/Scanner.scala#L49
            val start = pos: Rep[Double] // force read
            val first = nextInt('.')
            val second = nextInt(d)
            var invariant =  second
            var denom = 1

            first.asInstanceOf[Rep[Double]]
            second.asInstanceOf[Rep[Double]]

            while (invariant != 0 ){
              invariant = invariant /10
              denom = denom * 10
            }
            val res = if (denom > 0) first.doubleValue + second.doubleValue /denom else first.doubleValue
            res: Rep[Double]
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
              val x1 = this.x
              val y1 = this.y
              val x2 = x1 + this.width
              val y2 = y1 + this.height
              x1 <= x && x < x2 && y1 <= y && y < y2
            }
          }
          def contains(p: Point): Rep[Boolean] = contains(p.x, p.y)
          def contains(r: Rectangle): Rep[Boolean] = {
            if (isEmpty() || r.width <= 0.0 || r.height <= 0.0) {
              false
            } else {
              val x1 = this.x
              val y1 = this.y
              val x2 = x1 + this.width
              val y2 = y1 + this.height
              x1 <= r.x && r.x + r.width <= x2 && y1 <= r.y && r.y + r.height <= y2
            }
          }

          def intersects(r: Rectangle): Rep[Boolean] = {
            if (isEmpty() || r.width <= 0.0 || r.height <= 0.0) {
              false
            } else {
              val x1 = this.x
              val y1 = this.y
              val x2 = x1 + this.width
              val y2 = y1 + this.height
              r.x + r.width > x1 && r.x < x2 && r.y + r.height > y1 && r.y < y2
            }
          }
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

        trait STR_TreeAttributes{
          val fillFactor = 1
          val maxEntries = 12
          val numInLeaf = fillFactor * maxEntries
        }

        class STR_Tree(val size: Int) extends STR_TreeAttributes{
          val keys:  Rep[Array[Int]]    = NewArray[Int](size)
          val xs:    Rep[Array[Double]] = NewArray[Double](size)
          val ys:    Rep[Array[Double]] = NewArray[Double](size)

          var numPoints = 0

          var numOfLevels = 0
          var s = size
          while(s!=0){
            s = s / numInLeaf
            numOfLevels += 1
          }

          //helper arrays to assist in finding the start index and end index of each level
          val startLevel: Rep[Array[Int]] = NewArray[Int](numOfLevels)
          val levelActualEnd: Rep[Array[Int]] = NewArray[Int](numOfLevels)

          startLevel(0) = 0

          var v = 1 : Rep[Int]
          var inode_size = 0
          for (i <- 0 until numOfLevels+1){
            startLevel(i) = inode_size
            inode_size = inode_size + v
            v = v * numInLeaf
          }
         // startLevel(numOfLevels-1) = inode_size

          var min0:    Rep[Array[Double]] = NewArray[Double](inode_size)
          var max0:    Rep[Array[Double]] = NewArray[Double](inode_size)
          var min1:    Rep[Array[Double]] = NewArray[Double](inode_size)
          var max1:    Rep[Array[Double]] = NewArray[Double](inode_size)

          def addPoint(p: Point) = {
            keys(numPoints) = p.key
            xs(numPoints) = p.x
            ys(numPoints) = p.y
            numPoints += 1
          }

          def buildIndex()={

            //the leaf-level case
            //var currentLevel = numOfLevels - 1
            //var currentSartIndex = inode_size - power(currentLevel)

            var currentSartIndex = startLevel(numOfLevels - 1)

            var k = 0 //the starting index of xs and ys
            while(k<size-numInLeaf){
              var min0Val = xs(k): Rep[Double]
              var max0Val = xs(k): Rep[Double]
              var min1Val = ys(k): Rep[Double]
              var max1Val = ys(k): Rep[Double]

              for(i <- 1 + k until numInLeaf+k){
                if(xs(i) < min0Val)
                  min0Val = xs(i)
                if(xs(i) > max0Val)
                  max0Val = xs(i)
                if(ys(i)<min1Val)
                  min1Val = ys(i)
                if(ys(i) > max1Val)
                  max1Val = ys(i)
              }

              min0(currentSartIndex) = min0Val
              max0(currentSartIndex) = max0Val
              min1(currentSartIndex) = min1Val
              max1(currentSartIndex) = max1Val

              currentSartIndex = currentSartIndex + 1

              k = k + numInLeaf

            }

            if(k < size){
              //last leaf is incomplete
              val remaining = size-k

              var min0Val = xs(k): Rep[Double]
              var max0Val = xs(k): Rep[Double]
              var min1Val = ys(k): Rep[Double]
              var max1Val = ys(k): Rep[Double]

              for(i<- 1 + k until remaining +k){
                if(xs(i) < min0Val)
                  min0Val = xs(i)
                if(xs(i) > max0Val)
                  max0Val = xs(i)
                if(ys(i)<min1Val)
                  min1Val = ys(i)
                if(ys(i) > max1Val)
                  max1Val = ys(i)
              }
              min0(currentSartIndex) = min0Val
              max0(currentSartIndex) = max0Val
              min1(currentSartIndex) = min1Val
              max1(currentSartIndex) = max1Val
            }
            else
              currentSartIndex = currentSartIndex - 1

            levelActualEnd(numOfLevels-1) = currentSartIndex


            //println(currentSartIndex)

            var j = numOfLevels-2


            while(j>=0) {
              currentSartIndex = startLevel(j)
              k = startLevel(j+1) //the start index of the level we are going to process
            println(currentSartIndex)
            println(k)
            println(levelActualEnd(j+1))

              while(k < levelActualEnd(j+1) - numInLeaf){

                var min0Val = min0(k): Rep[Double]
                var max0Val = max0(k): Rep[Double]
                var min1Val = min1(k): Rep[Double]
                var max1Val = max1(k): Rep[Double]

                for(i <- 1 + k until numInLeaf+k){
                  if(min0(i) < min0Val)
                    min0Val = min0(i)
                  if(max0(i) > max0Val)
                    max0Val = max0(i)
                  if(min1(i)<min1Val)
                    min1Val = min1(i)
                  if(max1(i) > max1Val)
                    max1Val = max1(i)
                }

                min0(currentSartIndex) = min0Val
                max0(currentSartIndex) = max0Val
                min1(currentSartIndex) = min1Val
                max1(currentSartIndex) = max1Val

                currentSartIndex = currentSartIndex + 1

                k = k + numInLeaf

              }

            println(k)
            if(k < levelActualEnd(j+1)){ //last node is incomplete
              val remaining = levelActualEnd(j+1) - k
              println(remaining)

              var min0Val = min0(k): Rep[Double]
              var max0Val = max0(k): Rep[Double]
              var min1Val = min1(k): Rep[Double]
              var max1Val = max1(k): Rep[Double]

              for(i<- 1 + k until remaining +k){
                if(min0(i) < min0Val)
                  min0Val = min0(i)
                if(max0(i) > max0Val)
                  max0Val = max0(i)
                if(min1(i) < min1Val)
                  min1Val = min1(i)
                if(max1(i) > max1Val)
                  max1Val = max1(i)
              }

              min0(currentSartIndex) = min0Val
              max0(currentSartIndex) = max0Val
              min1(currentSartIndex) = min1Val
              max1(currentSartIndex) = max1Val

            }
            else
              currentSartIndex = currentSartIndex - 1

            println(currentSartIndex)
            levelActualEnd(j) = currentSartIndex

              println(j)
              j = j-1
            }

            println(min0(0))
            println(max0(0))
            println(min1(0))
            println(max1(0))







          }

          def power(v: Rep[Int]): Rep[Int]={
            var out = 1: Rep[Int]
            for(i <- 0 until v)
              out = out * numInLeaf

            out

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
          println("start building")
          val strTree = new STR_Tree(20000)
          processCSV("/Users/postgresuser/Research/tutorials/src/data/t2_20000.csv") { p2 =>
            strTree.addPoint(p2)
          }
          strTree.buildIndex
          println("end building")
//          println("start query")
//          processCSV("/Users/postgresuser/Research/tutorials/src/data/t1_10000.csv") { p1 =>
//            grid2.nestedIndexWindowJoin_efficient(p1,11.26, 11.26)
//            { p3 =>
//              //println(p3.key)
//              //printf("id: %d, x: %f, y: %f",p3.key,p3.x,p3.y)
//            }
//          }
//          println("end query")
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


  abstract class LMS_Driver[A:Manifest,B:Manifest] extends DslDriverC[A,B] with  ScannerLowerExp{ q =>
    override val codegen = new DslGenC with CGenScannerLower {
      val IR: q.type = q
    }
  }
}