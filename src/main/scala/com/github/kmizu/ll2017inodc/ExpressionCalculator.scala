package com.github.kmizu.ll2017inodc

import Parsers._

object ExpressionCalculator {
  def expression: Parser[Int] = additive
  def additive: Parser[Int] = P((multitive ~
    ($("+") ~ multitive | $("-") ~ multitive).*).map {
    case (l, rs) => rs.foldLeft(l) { case (e, ("+", r)) => e + r; case (e, ("-", r)) => e - r }
  })
  def multitive: Parser[Int] = P((primary ~
    ($("*") ~ primary | $("/") ~ primary).*).map {
      case (l, rs) => rs.foldLeft(l) { case (e, ("*", r)) => e * r; case (e, ("/", r)) => e / r }
    }
  )
  def primary: Parser[Int] = P(
    ($("(") ~> expression <~ $(")")) | number
  )
  def number: Parser[Int] = oneOf('0'to'9').*.map {
    digits => digits.mkString.toInt
  }

  def calculate(input: String): Int = {
    expression(input) match {
      case Some((v, n)) => v
      case None => sys.error("cannot reach here")
    }
  }

  def main(args: Array[String]): Unit = {
    assert(3 == calculate("1+2"))
    assert(-1 == calculate("1-2"))
    assert(2 == calculate("1*2"))
    assert(0 == calculate("1/2"))
    assert(2 == calculate("1+2*3/4"))
    assert(2 == calculate("(1+2)*3/4"))
    assert(0 == calculate("(1+2)*(3/4)"))
  }
}
