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
      println(p1.doPivot)
    }
  }

  test("subset should reduce rows") {
    new PivotTest {
      println(p1.subset(numberRows(0).zipWithIndex.filter(_._2 % 2 == 0).map(_._1)))
    }
  }
}
