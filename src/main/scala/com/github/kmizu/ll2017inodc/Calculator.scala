package com.github.kmizu.ll2017inodc

object Calculator extends Parsers {
  def expression: Parser[Int] = additive
  def additive: Parser[Int] = (multitive ~ (string("+") ~ multitive | string("-") ~ multitive).*).map{
    case (l, rs) => rs.foldLeft(l) { case (e, ("+", r)) => e + r; case (e, ("-", r)) => e - r }
  }
  def multitive: Parser[Int] = (primary ~ (string("*") ~ primary | string("/") ~ primary).*).map{
    case (l, rs) => rs.foldLeft(l) { case (e, ("*", r)) => e * r; case (e, ("/", r)) => e / r }
  }
  def primary: Parser[Int] = {
    (string("(") ~> expression <~ string(")")) | number
  }
  def number: Parser[Int] = oneOf('0'to'9').*.map{digits => digits.mkString.toInt}

  def main(args: Array[String]): Unit = {
    println(expression("1+2*3"))
    println(expression("1+5*3/4"))
    println(expression("(1+5)*3/2"))
  }
}
