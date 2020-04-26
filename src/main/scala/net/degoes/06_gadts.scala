package net.degoes

/*
 * INTRODUCTION
 *
 * In Functional Design, type safety of data models can be greatly improved by
 * using so-called generalized algebraic data types.
 *
 * In this section, you'll review GADTs with a focus on functional models.
 *
 */

/**
 * EXPRESSIONS - EXERCISE SET 1
 *
 * Consider an application (such as the spreadsheet example) that needs to
 * calculate values in a user-defined way.
 */
object expr {
  sealed trait Expr[+A]
  object Expr {
    final case class Integer(value: Int) extends Expr[Int]
    final case class Str(value: String)  extends Expr[String]

    /**
     * EXERCISE 1
     *
     * Add an operator that adds two integer expressions, yielding an integer
     * expression.
     *
     * NOTE: Be sure to modify the `calculate` method below, so that it can
     * handle the new operation.
     */
    final case class Add()

    /**
     * EXERCISE 2
     *
     * Add an operator that subtracts an integer from another integer expression,
     * yielding an integer expression.
     *
     * NOTE: Be sure to modify the `calculate` method below, so that it can
     * handle the new operation.
     */
    final case class Subtract()

    /**
     * EXERCISE 3
     *
     * Add an operator that multiplies two integer expressions, yielding an
     * integer expression.
     *
     * NOTE: Be sure to modify the `calculate` method below, so that it can
     * handle the new operation.
     */
    final case class Multiply()

    /**
     * EXERCISE 4
     *
     * Add an operator that concatenates two strings, yielding a string
     * expression.
     *
     * NOTE: Be sure to modify the `calculate` method below, so that it can
     * handle the new operation.
     */
    final case class Concat()

    /**
     * EXERCISE 5
     *
     * Add an operator that determines if a string starts with a specified
     * prefix, yielding a boolean expression.
     *
     * NOTE: Be sure to modify the `calculate` method below, so that it can
     * handle the new operation.
     */
    final case class StartsWith()
  }

  import Expr._

  def calculate[A](expr: Expr[A]): A =
    expr match {
      case Integer(v) => v
      case Str(v)     => v
    }
}

/**
 * PARSERS - EXERCISE SET 2
 */
object parser {
  sealed trait Parser[+A]
  object Parser {
    final case object OneChar extends Parser[Char]

    /**
     * EXERCISE 1
     *
     * Add an operator that can repeat a parser between some lower range
     * (optional) and some upper range (optional).
     *
     * NOTE: Be sure to modify the `parse` method below, so that it can
     * handle the new operation.
     */
    final case class Repeat()

    /**
     * EXERCISE 2
     *
     * Add a constructor that models the production of the specified value (of
     * any type at all), without consuming any input.
     *
     * NOTE: Be sure to modify the `parse` method below, so that it can
     * handle the new operation.
     */
    final case class Succeed()

    /**
     * EXERCISE 3
     *
     * Add a constructor that models failure with a string error message.
     *
     * NOTE: Be sure to modify the `parse` method below, so that it can
     * handle the new operation.
     */
    final case class Fail()

    /**
     * EXERCISE 4
     *
     * Add an operator that can try one parser, but if that fails, try
     * another parser.
     *
     * NOTE: Be sure to modify the `parse` method below, so that it can
     * handle the new operation.
     */
    final case class OrElse()

    /**
     * EXERCISE 5
     *
     * Add an operator that parses one thing, and then parses another one,
     * in sequence, producing a tuple of their results.
     *
     * NOTE: Be sure to modify the `parse` method below, so that it can
     * handle the new operation.
     */
    final case class Sequence()
  }

  import Parser._

  def parse[A](parser: Parser[A], input: String): Option[A] =
    parser match {
      case OneChar => input.headOption
    }
}
