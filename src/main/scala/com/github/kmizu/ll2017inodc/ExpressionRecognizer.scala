package com.github.kmizu.ll2017inodc

import com.github.kmizu.ll2017inodc.Parsers._

object ExpressionRecognizer {
  def expression: Parser[Any] = additive
  def additive: Parser[Any] = P((multitive ~
    ($("+") ~ multitive | $("-") ~ multitive).*)
  )
  def multitive: Parser[Any] = P((primary ~
    ($("*") ~ primary | $("/") ~ primary).*)
  )
  def primary: Parser[Any] = P(
    ($("(") ~> expression <~ $(")")) | number
  )
  def number: Parser[Any] = oneOf('0'to'9')
}
