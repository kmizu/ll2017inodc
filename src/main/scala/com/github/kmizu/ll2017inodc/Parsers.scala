package com.github.kmizu.ll2017inodc

object Parsers {
  type Result[+A] = Option[(A, String)]
  type Parser[+A] = String => Result[A]

  def oneOf(seq: Seq[Char]): Parser[String] = input => {
    if(input.length == 0 || !seq.exists(_ == input.charAt(0))) None
    else Some(input.substring(0, 1) -> input.substring(1))
  }

  def $(literal: String): Parser[String] = input => {
    if(input.startsWith(literal)) Some(literal -> input.substring(literal.length)) else None
  }

  def P[A](parser: => Parser[A]): Parser[A] = input => parser(input)

  implicit class RichParser[T](val self: Parser[T]) {
    def * : Parser[List[T]] = input => {
      def repeat(input: String): (List[T], String) = self(input) match {
        case Some((value, next1)) =>
          val (result, next2) = repeat(next1)
          (value :: result, next2)
        case None =>
          (Nil, input)
      }

      val (result, next) = repeat(input)
      Some(result -> next)
    }

    def ~[U](right: Parser[U]): Parser[(T, U)] = input => {
      self(input) match {
        case Some((value1, next1)) =>
          right(next1) match {
            case Some((value2, next2)) =>
              Some((value1, value2), next2)
            case None => None
          }
        case None => None
      }
    }

    def ~>[U](that: Parser[U]): Parser[U] = (self ~ that).map { case (l, r) => r }

    def <~[U](that: Parser[U]): Parser[T] = (self ~ that).map { case (l, r) => l }

    def |(right: Parser[T]): Parser[T] = input => {
      self(input) match {
        case success@Some((_, _)) => success
        case None => right(input)
      }
    }

    def map[U](function: T => U): Parser[U] = input => {
      self(input) match {
        case Some((value, next)) => Some(function(value) -> next)
        case None => None
      }
    }
  }
}
