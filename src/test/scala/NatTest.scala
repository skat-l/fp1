package com.tkroman.kpi.y2022.l1

import munit.FunSuite

class NatTest extends FunSuite {

  test("multiply") {
    val expected = 10
    val actual = {
      toInt(
        mul(fromInt(2), fromInt(5))
      )
    }
    assertEquals(actual, expected)
  }

  test ("pow"){
    val expected = 81
    val actual = {
      toInt(
        pow(fromInt(3), fromInt(4))
      )
    }
    assertEquals(actual, expected)
  }

  test ("toInt"){
    val expected = 5
    val actual = {
      toInt(
        Succ(Succ(Succ(Succ(Succ(Zero)))))
      )
    }
    assertEquals(actual, expected)
  }

  test ("fromInt"){
    val expected = Succ(Succ(Succ(Succ(Succ(Zero)))))
    val actual = fromInt(5)
    assertEquals(actual, expected)
  }

  test ("modPow"){
    val expected = 4
    val actual = {
      toInt(
        modPow(fromInt(3), fromInt(4), fromInt(7))
      )
    }
    assertEquals(actual, expected)
  }
}
