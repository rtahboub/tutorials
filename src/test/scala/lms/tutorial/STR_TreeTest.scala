package scala.lms.tutorial

import _root_.java.awt.geom.Point2D
import _root_.java.awt.geom.Rectangle2D

import scala.lms.common._
import scala.virtualization.lms.common
import scala.virtualization.lms.internal._
import scala.virtualization.lms.util._
import java.io.{PrintWriter,StringWriter,FileOutputStream}
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

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

          def nextDouble(d: Rep[Char] = '|'): Rep[Double] = {
            val start = pos: Rep[Double] // force read
            var num = 0
            var denom = 1
            while (boolean_and(data(pos) != '.', data(pos) != d) && data(pos) != '\n') {
              num = num * 10 + (data(pos) - '0').toInt
              pos += 1
            }
            if (data(pos) == '.') {
              pos += 1
              while (boolean_and(data(pos) != d, data(pos) != '\n')) {
                num = num * 10 + (data(pos) - '0').toInt
                denom = denom * 10
                pos += 1
              }
            }
            pos += 1
            val res = num.doubleValue / denom.doubleValue
            //printf("nextDouble %d/%d=%.2f\n",num,denom,res)
            res
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

        case class Point(key: Rep[Int], x: Rep[Int], y: Rep[Int])

        trait STR_TreeAttributes{
          val fillFactor = 1
          val maxEntries = 12
          val numInLeaf = fillFactor * maxEntries


          def array_quicksort[T:Manifest](xs: Rep[Array[T]], size: Rep[Long], comp: Function2[Rep[T],Rep[T],Rep[Int]]): Rep[Unit] = {
            val f = uninlinedFunc2((a:Rep[MyPointer[T]], b:Rep[MyPointer[T]]) => comp(deref(a), deref(b)))
            if (manifest[T] == manifest[Int])
              cg"qsort($xs,$size,sizeof(int),(__compar_fn_t)$f)".as[Unit]
            else if (manifest[T] == manifest[Long])
              cg"qsort($xs,$size,sizeof(long),(__compar_fn_t)$f)".as[Unit]
            else if (manifest[T] == manifest[Double])
              cg"qsort($xs,$size,sizeof(double),(__compar_fn_t)$f)".as[Unit]
            else if (manifest[T] == manifest[Char])
              cg"qsort($xs,$size,sizeof(char),(__compar_fn_t)$f)".as[Unit]
            else if (manifest[T] == manifest[String])
              cg"qsort($xs,$size,sizeof(char*),(__compar_fn_t)$f)".as[Unit]
            else if (manifest[T] <:< manifest[Record]) {
              val name = structName(manifest[T])
              rq(new StringContext("qsort((void *)", ",", s",sizeof(struct $name), (__compar_fn_t)", ")")).cg(xs, size, f).as[Unit]
            } else {
              cg"qsort((void *)$xs,$size,sizeof(void*),(__compar_fn_t)$f)".as[Unit]
            }
          }
        }

        case class Rectangle(low0: Rep[Double], high0: Rep[Double], low1: Rep[Double], high1: Rep[Double]) {
          def intersects(otherLow0: Double, otherHigh0: Double, otherLow1: Double, otherHigh1:Double): Rep[Boolean] = {

            ((low0 <= otherHigh0)
              && (high0 >= otherLow0)
              && (low1 <= otherHigh1)
              && (high1 >= otherLow1))
          }
        }

        class STR_Tree(val size: Int) extends STR_TreeAttributes{
          val keys:  Rep[Array[Int]]    = NewArray[Int](size)
          val xs:    Rep[Array[Int]] = NewArray[Int](size)
          val ys:    Rep[Array[Int]] = NewArray[Int](size)

          var numPoints = 0

          var numOfLevels = 0
          var s = size
          while(s!=0){
            s = s / numInLeaf
            numOfLevels += 1
          }
          numOfLevels = numOfLevels -1
          //println(numOfLevels)

          //helper arrays to assist in finding the start index and end index of each level
          val startLevel: Rep[Array[Int]] = NewArray[Int](numOfLevels)
          val levelActualEnd: Rep[Array[Int]] = NewArray[Int](numOfLevels)

          var v = numInLeaf
          var inode_size = 0
          for (i <- 0 until numOfLevels){
            startLevel(i) = inode_size
            inode_size = inode_size + v
            v = v * numInLeaf
          }

          val nexts_size = startLevel(numOfLevels-1) + (size/numInLeaf) + 1
          println(nexts_size)
          var nexts: Rep[Array[Int]]    = NewArray[Int](nexts_size)
          var counts: Rep[Array[Int]]    = NewArray[Int](nexts_size)
          val stack_size = nexts_size
          var stack: Rep[Array[Int]]    = NewArray[Int](stack_size)
          var stack_ptr = 0


          var min0:    Rep[Array[Int]] = NewArray[Int](inode_size)
          var max0:    Rep[Array[Int]] = NewArray[Int](inode_size)
          var min1:    Rep[Array[Int]] = NewArray[Int](inode_size)
          var max1:    Rep[Array[Int]] = NewArray[Int](inode_size)

          def addPoint(p: Point) = {
            keys(numPoints) = p.key
            xs(numPoints) = p.x
            ys(numPoints) = p.y

            numPoints += 1
          }

          def buildIndex()={


            var currentSartIndex = startLevel(numOfLevels-1)

            var k = 0 //the starting index of xs and ys
            while(k<size-numInLeaf){
              var min0Val = xs(k): Rep[Int]
              var max0Val = xs(k): Rep[Int]
              var min1Val = ys(k): Rep[Int]
              var max1Val = ys(k): Rep[Int]

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

              nexts(currentSartIndex) = k
              counts(currentSartIndex) = numInLeaf

              currentSartIndex = currentSartIndex + 1

              k = k + numInLeaf

            }

            if(k < size){
              var c = 0
              //last leaf is incomplete
              val remaining = size-k

              var min0Val = xs(k): Rep[Int]
              var max0Val = xs(k): Rep[Int]
              var min1Val = ys(k): Rep[Int]
              var max1Val = ys(k): Rep[Int]

              for(i<- 1 + k until remaining +k){
                if(xs(i) < min0Val)
                  min0Val = xs(i)
                if(xs(i) > max0Val)
                  max0Val = xs(i)
                if(ys(i)<min1Val)
                  min1Val = ys(i)
                if(ys(i) > max1Val)
                  max1Val = ys(i)

                c = c+1
              }
              min0(currentSartIndex) = min0Val
              max0(currentSartIndex) = max0Val
              min1(currentSartIndex) = min1Val
              max1(currentSartIndex) = max1Val

              nexts(currentSartIndex) = k
              counts(currentSartIndex) = c

            }
            else
              currentSartIndex = currentSartIndex - 1

            levelActualEnd(numOfLevels-1) = currentSartIndex


            //println(currentSartIndex)

            var j = numOfLevels-1

            while(j>0) {
              currentSartIndex = startLevel(j-1)
              k = startLevel(j) //the start index of the level we are going to process

              while(k < levelActualEnd(j) - numInLeaf){

                var min0Val = min0(k): Rep[Int]
                var max0Val = max0(k): Rep[Int]
                var min1Val = min1(k): Rep[Int]
                var max1Val = max1(k): Rep[Int]

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

                nexts(currentSartIndex) = k
                counts(currentSartIndex) = numInLeaf

                currentSartIndex = currentSartIndex + 1

                k = k + numInLeaf

              }

            if(k < levelActualEnd(j)){ //last node is incomplete
              var c2 = 0
              val remaining = levelActualEnd(j) - k
              //println(remaining)

              var min0Val = min0(k): Rep[Int]
              var max0Val = max0(k): Rep[Int]
              var min1Val = min1(k): Rep[Int]
              var max1Val = max1(k): Rep[Int]

              for(i<- 1 + k until remaining +k){

                c2 = c2 + 1

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

              nexts(currentSartIndex) = k
              counts(currentSartIndex) = c2

            }
            else{
              currentSartIndex = currentSartIndex - 1
            }

            levelActualEnd(j-1) = currentSartIndex

              j = j-1
            }

          }

          def intersects(low0: Rep[Int], high0: Rep[Int], low1: Rep[Int], high1:Rep[Int],
                         otherLow0: Rep[Int], otherHigh0: Rep[Int], otherLow1: Rep[Int], otherHigh1:Rep[Int]): Rep[Boolean] = {

            ((low0 <= otherHigh0) && (high0 >= otherLow0) && (low1 <= otherHigh1) && (high1 >= otherLow1))
          }

          def containsPoint(low0: Rep[Int], high0: Rep[Int], low1: Rep[Int], high1:Rep[Int], x: Rep[Int], y: Rep[Int]) :Rep[Boolean]={
             ((low0 <= x) && (high0 >= x) && (low1 <= y) && (high1 >=y))
          }


          def nestedIndexWindowJoin(p1: Point, xDel: Int, yDel: Int)(f: Point => Rep[Unit]) = {
            //reset stack
            stack_ptr = 0

            var k = counts(0) - 1

            for (a <-0 until k) {
              stack(stack_ptr) = a
              stack_ptr = stack_ptr + 1

              var x1 = (p1.x - xDel) : Rep[Int]
              var y1 = (p1.y - yDel) : Rep[Int]
              var x2 = (p1.x + xDel) : Rep[Int]
              var y2 = (p1.y + yDel) : Rep[Int]

              while (stack_ptr != 0) {
                var top = stack(stack_ptr - 1): Rep[Int]
                stack_ptr = stack_ptr - 1

                if (intersects(min0(top), max0(top), min1(top), max1(top), x1, x2, y1, y2)) {

                  //update intersection area
                  var low0 = min0(top): Rep[Int]
                  var low1 = max0(top): Rep[Int]
                  var high0 = min1(top): Rep[Int]
                  var high1 = max1(top): Rep[Int]


                  if (min0(top) < x1) {
                      low0 = x1;
                  } else {
                      low0 = min0(top);
                  }

                  if (max0(top) < x2) {
                      high0 = max0(top);
                  } else {
                      high0 = x2;
                  }

                  if (min1(top) < y1) {
                      low1 = y1;
                  } else {
                      low1 = min1(top);
                  }

                  if (max1(top) < y2) {
                      high1 = max1(top);
                  } else {
                      high1 = y2;
                  }

                  x1 = low0
                  x2 = high0
                  y1 = low1
                  y2 = high1
                  var n = nexts(top)
                  if (n == 0) {}
                  else if (n < startLevel(numOfLevels-1)) {
                    var j = n + counts(n) - 1
                    while (j >= n) {
                      if (stack_ptr < stack_size) {
                        stack(stack_ptr) = j
                        stack_ptr = stack_ptr + 1
                      }
                      j = j - 1
                     }
                    }
                   else {
                      var leafStart = nexts(n)
                      for (i <- 0 until counts(n) * numInLeaf  ) {
                        if (containsPoint(x1, x2, y1, y2, xs(leafStart + i), ys(leafStart + i))) {
                          f(Point(keys(leafStart + i), xs(leafStart + i),ys(leafStart + i)))
                        }
                      }
                  }
                }
              } //end while
            }//end for
          }


          def printIndex(key_id: Rep[Int]) : Rep[Unit] = {
            //push the root into the stack
            var k = counts(key_id) - 1
            while (k>=0){
              stack(stack_ptr) = k
              stack_ptr = stack_ptr + 1
              k = k - 1
            }

            while(stack_ptr != 0 ){
              var top = stack(stack_ptr-1)
              stack_ptr = stack_ptr - 1
              print(top)
              var n = nexts(top)
              print("----")
              println(n)
              if(n == 0){}

              else if (n < startLevel(numOfLevels-1)){

                var j = n + counts(n) -1
                while(j>=n){
                  if(stack_ptr < stack_size) {
                    stack(stack_ptr) = j
                    stack_ptr = stack_ptr + 1
                  }
                  j = j-1
                }

              }
              else {
                print("leaf ")
                println(n)
                var leafStart = nexts(n)
                println(leafStart)
                for(i<-0 until counts(n) * numInLeaf){
                  print(keys(leafStart+i))
                  print(" ")
                  print(xs(leafStart+i))
                  print(" ")
                  println(ys(leafStart+i))

                  if (i%numInLeaf == 0)
                    println(" ")
                }
              }
            }
          }

          def printStack(): Rep[Unit]={
            for (i<- 0 until stack_ptr)
              println(stack(i))
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
            val x   = in.nextInt(',')
            val y  = in.nextInt('\n')
            f(Point(id,x,y))
          }
          in.done
        }

        // main function - code generated from everything inside gets compiled
        def snippet(x: Rep[String]): Rep[Unit] = {
          println("start building")
          val strTree = new STR_Tree(20004)
          processCSV("/Users/postgresuser/Research/tutorials/src/data/t2_20004_correct.csv") { p2 =>
            strTree.addPoint(p2)
          }
          strTree.buildIndex
          println("end building")
          println("start query")
          processCSV("/Users/postgresuser/Research/tutorials/src/data/t1_10000_int_correct.csv") { p1 =>
            strTree.nestedIndexWindowJoin(p1,11, 11)
            { p3 =>
              println(p3.key)
              //println(p3.key)
            }
          }
          println("end query")
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
//sbt -J-Xmx3G -J-Xms1G -J-XX:MaxPermSize=1G
