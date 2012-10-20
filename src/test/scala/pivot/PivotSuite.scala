package pivot 

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PivotSuite extends FunSuite {
  trait PivotTest {
    val numberRows = (List("one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten") +: 
      Range(1, 10, 1).map(x => {Range(x*1, x*1+10, 1).map(_.toString).toList})).toList
    val p1 = Pivot(numberRows)
    
    val marriedP = Pivot(
      List(
        List("Name", "Age", "Married"),
        List("Bill", "42", "TRUE"),
        List("Heloise", "47", "TRUE"),
        List("Thelma", "34", "FALSE"),
        List("Bridget", "47", "TRUE"),
        List("Robert", "42", "FALSE"),
        List("Eddie", "42", "TRUE")

      )
    )
  }

  test("headers should have same size as rows") {
    new PivotTest {
      p1.body.foreach(b => {
        assert(p1.headers.size === b.size)}
      )
    }
  }

  test("calculated columns should be appended to the row set") {
    new PivotTest {
      def createSum(row: List[String]) = {
        row.map(_.toInt).foldRight(0)(_ + _).toString 
      }
      val p2 = p1.addCalcCol(createSum)("sum")
    }
  }

  test("pivot should transpose") {
    new PivotTest {
      def accum(pointVal: String) = {
        "1"
      }
      marriedP.doPivot(accum)("Age", "Married")
      //println(p1.doPivot)
    }
  }

  test("subset should reduce rows") {
    new PivotTest {
      println(p1.subset(numberRows(0).zipWithIndex.filter(_._2 % 2 == 0).map(_._1)))
    }
  }
}
