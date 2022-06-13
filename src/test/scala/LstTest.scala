package Lst
import munit.FunSuite
import Lst.*
import Lst._

class LstTest extends FunSuite {
  test("getLeft on some") {
    val expected = Lst(1, 2, 3)
    val actual = Lst(1, 2, 3, 4, 5, 6, 7).getLeft
    assertEquals(actual, expected)
  }
  test("getLeft on empty") {
    val expected = Lst()
    val actual = Lst().getLeft
    assertEquals(actual, expected)
  }
  test("getRight on some") {
    val expected = Lst(4, 5, 6, 7)
    val actual = Lst(1, 2, 3, 4, 5, 6, 7).getRight
    assertEquals(actual, expected)
  }
  test("getRight on empty") {
    val expected = Lst()
    val actual = Lst().getRight
    assertEquals(actual, expected)
  }
  test("len on some") {
    val expected = 7
    val actual = Lst(1, 2, 3, 4, 5, 6, 7).len
    assertEquals(actual, expected)
  }
  test("len on empty") {
    val expected = 0
    val actual = Lst().len
    assertEquals(actual, expected)
  }
  test("reverse on some") {
    val expected = Lst(5, 4, 3, 2, 1)
    val actual = Lst(1, 2, 3, 4, 5).reverse
    assertEquals(actual, expected)
  }
  test("reverse on empty") {
    val expected = Lst()
    val actual = Lst().reverse
    assertEquals(actual, expected)
  }
  test("getHead on some") {
    val expected = 1
    val actual = Lst(1, 2, 3).getHead
    assertEquals(actual, expected)
  }
  test("getHead on empty") {
    val expected = "NO HEAD!"
    val actual = intercept[RuntimeException] {
      Lst().getHead
    }
    assertEquals(expected, actual.getMessage)
  }
  test("getInits on some") {
    val expected = Lst(Lst(1), Lst(1, 2), Lst(1, 2, 3))
    val actual = Lst(1, 2, 3).getInits
    assertEquals(expected, actual)
  }
  test("getInits on empty") {
    val expected = Lst(Lst())
    val actual = Lst().getInits
    assertEquals(expected, actual)
  }
  test("exists on some") {
    val expected = false
    val actual = Lst(1, 2, 3, 4, 5, 6, 7).exists(
      { (el: Int, add: Int, eq: Int) =>
        if el + add == eq then true else false
      },
      1,
      9
    )
    assertEquals(expected, actual)
  }
  test("exists on some") {
    val expected = true
    val actual = Lst(1, 2, 3, 4, 5, 6, 7).exists(
      { (el: Int, add: Int, eq: Int) =>
        if el + add == eq then true else false
      },
      1,
      8
    )
    assertEquals(expected, actual)
  }
  test("exists on empty") {
    val expected = false
    val actual = Lst(1, 2, 3, 4, 5, 6, 7).exists(
      { (el: Int, add: Int, eq: Int) =>
        if el + add == eq then true else false
      },
      1,
      9
    )
    assertEquals(expected, actual)
  }
  test("mergeSort on some") {
    val expected = Lst(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,
      17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34,
      35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52,
      53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70,
      71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88,
      89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99)
    val actual = Lst(42, 78, 74, 8, 19, 45, 77, 95, 52, 98, 99, 62, 36, 85, 3,
      54, 90, 80, 46, 9, 97, 86, 12, 68, 56, 44, 66, 71, 89, 43, 39, 25, 76, 47,
      24, 23, 58, 70, 15, 31, 16, 91, 64, 73, 83, 72, 22, 84, 33, 40, 53, 63,
      21, 96, 41, 82, 34, 26, 67, 55, 94, 32, 6, 29, 2, 18, 5, 87, 51, 75, 7,
      11, 60, 79, 69, 1, 14, 10, 20, 92, 50, 57, 0, 88, 13, 35, 59, 28, 38, 17,
      93, 37, 48, 27, 4, 65, 49, 61, 81, 30).mergeSort({ (a: Int, b: Int) =>
      if a > b then Compared("Gt")
      else if a < b then Compared("Lt")
      else Compared("Eq")
    })
    assertEquals(expected, actual)
  }
  test("mergeSort on empty") {
    val expected = Lst()
    val actual = Lst().mergeSort({ (a: Int, b: Int) =>
      if a > b then Compared("Gt")
      else if a < b then Compared("Lt")
      else Compared("Eq")
    })
    assertEquals(expected, actual)
  }

}
