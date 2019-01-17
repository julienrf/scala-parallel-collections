package scala.collection

import org.junit.Test
import org.junit.Assert._

import scala.collection.mutable.Buffer
import scala.collection.parallel.immutable.ParVector
import scala.collection.parallel.mutable.ParArray
import scala.reflect.ClassTag

// based on run/collections-conversion.scala partest
class CollectionConversionsTest {
  val out = new StringBuilder

  val testVector = Vector(1,2,3)
  val testBuffer = Buffer(1,2,3)
  val testSeq = scala.Seq(1,2,3)
  val testLazyList = LazyList(1,2,3)
  val testArray = Array(1,2,3)
  val testParVector = ParVector(1,2,3)
  val testParSeq = parallel.ParSeq(1,2,3)
  val testParArray = ParArray(1,2,3)

  @Test def testAll: Unit = {
    testConversionIterator("iterator", (1 to 3).iterator)
    testConversion("Vector", Vector(1,2,3))
    testConversion("List", List(1,2,3))
    testConversion("Buffer", Buffer(1,2,3))
    testConversionParIterable("ParVector", ParVector(1,2,3))
    testConversionParIterable("ParArray", ParArray(1,2,3))
    testConversion("Set", Set(1,2,3))
    testConversion("SetView", Set(1,2,3).view)
    testConversion("BufferView", Buffer(1,2,3).view)
  }

  def printResult[A,B](msg: String, obj: A, expected: B)(implicit tag: ClassTag[A], tag2: ClassTag[B]): Boolean = {
    out ++= ("  :" + msg +": ")
    val isArray = obj match {
      case x: Array[Int] => true
      case _ => false
    }
    val expectedEquals =
      if(isArray) obj.asInstanceOf[Array[Int]].toSeq == expected.asInstanceOf[Array[Int]].toSeq
      else obj == expected
    val tagEquals = tag == tag2
    val ok = expectedEquals && tagEquals
    if(ok) out ++= "OK"
    else out ++= "FAILED"
    if(!expectedEquals) out ++= (", " + obj + " != " + expected)
    if(!tagEquals)     out ++= (", " + tag + " != " + tag2)
    out += '\n'
    ok
  }

  def testConversion[A: ClassTag](name: String, col: => Iterable[A]): Unit = {
    out ++= ("-- Testing " + name + " ---\n")
    if(!(
      printResult("[Direct] Vector   ", col.toVector, testVector) &&
      printResult("[Copy]   Vector   ", col.to(Vector), testVector) &&
      printResult("[Direct] Buffer   ", col.toBuffer, testBuffer) &&
      printResult("[Copy]   Buffer   ", col.to(Buffer), testBuffer) &&
      printResult("[Direct] Seq      ", col.toSeq, testSeq) &&
      printResult("[Copy]   Seq      ", col.to(scala.Seq), testSeq) &&
      printResult("[Copy]   Stream   ", col.to(LazyList), testLazyList) &&
      printResult("[Direct] Array    ", col.toArray, testArray) &&
      printResult("[Copy]   Array    ", col.to(Array), testArray) &&
      printResult("[Copy]   ParVector", col.to(ParVector), testParVector) &&
      printResult("[Copy]   ParArray ", col.to(ParArray), testParArray)
    )) {
      print(out)
      fail("Not all tests successful")
    }
  }

  def testConversionIterator[A: ClassTag](name: String, col: => Iterator[A]): Unit = {
    out ++= ("-- Testing " + name + " ---\n")
    if(!(
      printResult("[Direct] Vector   ", col.toVector, testVector) &&
      printResult("[Copy]   Vector   ", col.to(Vector), testVector) &&
      printResult("[Direct] Buffer   ", col.toBuffer, testBuffer) &&
      printResult("[Copy]   Buffer   ", col.to(Buffer), testBuffer) &&
      printResult("[Direct] Seq      ", col.toSeq, testSeq) &&
      printResult("[Copy]   Seq      ", col.to(scala.Seq), testSeq) &&
      printResult("[Copy]   Stream   ", col.to(LazyList), testLazyList) &&
      printResult("[Direct] Array    ", col.toArray, testArray) &&
      printResult("[Copy]   Array    ", col.to(Array), testArray) &&
      printResult("[Copy]   ParVector", col.to(ParVector), testParVector) &&
      printResult("[Copy]   ParArray ", col.to(ParArray), testParArray)
    )) {
      print(out)
      fail("Not all tests successful")
    }
  }

  def testConversionParIterable[A: ClassTag](name: String, col: => parallel.ParIterable[A]): Unit = {
    out ++= ("-- Testing " + name + " ---\n")
    if(!(
      printResult("[Direct] Vector   ", col.toVector, testVector) &&
      printResult("[Copy]   Vector   ", col.to(Vector), testVector) &&
      printResult("[Direct] Buffer   ", col.toBuffer, testBuffer) &&
      printResult("[Copy]   Buffer   ", col.to(Buffer), testBuffer) &&
      printResult("[Direct] ParSeq   ", col.toSeq, testParSeq) &&
      printResult("[Copy]   Seq      ", col.to(scala.Seq), testSeq) &&
      printResult("[Copy]   Stream   ", col.to(LazyList), testLazyList) &&
      printResult("[Direct] Array    ", col.toArray, testArray) &&
      printResult("[Copy]   Array    ", col.to(Array), testArray) &&
      printResult("[Copy]   ParVector", col.to(ParVector), testParVector) &&
      printResult("[Copy]   ParArray ", col.to(ParArray), testParArray)
    )) {
      print(out)
      fail("Not all tests successful")
    }
  }

  // Tests checking that implicit conversions are correctly triggered for various types of collections
  def testImplicitConverters(): Unit = {
    import scala.{collection => sc}
    import scala.collection.{mutable => scm, immutable => sci}

    import scala.collection.parallel.CollectionConverters._

    val xs1 = sc.Iterable(1, 2, 3).par
    val xs1T: sc.parallel.ParIterable[Int] = xs1
    val xs2 = sc.Seq(1, 2, 3).par
    val xs2T: sc.parallel.ParSeq[Int] = xs2
    val xs3 = scala.Seq(1, 2, 3).par
    val xs3T: sc.parallel.immutable.ParSeq[Int] = xs3
    val xs4 = sci.Seq(1, 2, 3).par
    val xs4T: sc.parallel.immutable.ParSeq[Int] = xs4
    val xs5 = List(1, 2, 3).par
    val xs5T: sc.parallel.immutable.ParSeq[Int] = xs5
    val xs6 = Vector(1, 2, 3).par
    val xs6T: sc.parallel.immutable.ParVector[Int] = xs6
    val xs7 = scm.Seq(1, 2, 3).par
    val xs7T: sc.parallel.mutable.ParSeq[Int] = xs7
    val xs8 = scm.ArrayBuffer(1, 2, 3).par
    val xs8T: sc.parallel.mutable.ParArray[Int] = xs8
    val xs9 = Array(1, 2, 3).par
    val xs9T: sc.parallel.mutable.ParArray[Int] = xs9
  }

}
