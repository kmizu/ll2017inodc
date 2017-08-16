package com.github.kmizu.ll2017inodc

abstract class Parsers {
  sealed class ParseResult[+T]
  case class ParseSuccess[+T](value: T, next: String) extends ParseResult[T]
  case object ParseFaiure extends ParseResult[Nothing]

  type Parser[+T] = String => ParseResult[T]

  def oneOf(seqs: Seq[Char]*): Parser[String] = input => {
    if(input.length == 0 || !seqs.exists(seq => seq.exists(ch => ch == input.charAt(0)))) ParseFaiure
    else ParseSuccess(input.substring(0, 1), input.substring(1))
  }

  def string(literal: String): Parser[String] = input => {
    if(input.startsWith(literal)) ParseSuccess(literal, input.substring(literal.length)) else ParseFaiure
  }

  implicit class RichParser[T](val self: Parser[T]) {
    def * : Parser[List[T]] = input => {
      def repeat(input: String): (List[T], String) = self(input) match {
        case ParseSuccess(value, next1) =>
          val (result, next2) = repeat(next1)
          (value::result, next2)
        case ParseFaiure =>
          (Nil, input)
      }
      val (result, next) = repeat(input)
      ParseSuccess(result, next)
    }

    def ~[U](right: Parser[U]) : Parser[(T, U)] = input => {
      self(input) match {
        case ParseSuccess(value1, next1) =>
          right(next1) match {
            case ParseSuccess(value2, next2) =>
              ParseSuccess((value1, value2), next2)
            case ParseFaiure =>
              ParseFaiure
          }
        case ParseFaiure =>
          ParseFaiure
      }
    }

    def ~>[U](that: Parser[U]): Parser[U] = (self ~ that).map{ case (l, r) => r}

    def <~[U](that: Parser[U]): Parser[T] = (self ~ that).map{ case (l, r) => l}

    def |(right: Parser[T]): Parser[T] = input => {
      self(input) match {
        case success@ParseSuccess(_, _) => success
        case ParseFaiure => right(input)
      }
    }

    def map[U](function: T => U): Parser[U] = input => {
      self(input) match {
        case ParseSuccess(value, next) => ParseSuccess(function(value), next)
        case ParseFaiure => ParseFaiure
      }
    }
}
