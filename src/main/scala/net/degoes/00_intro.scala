package net.degoes

import zio.*

/*
 * INTRODUCTION
 *
 * _Functional Design_ is a technique for modeling solutions to problems in some
 * domain using _immutable values_ (_models_), with a small but rich set of
 * primitives for constructing, transforming, and composing solutions in order
 * to solve even the most complex problems easily, with high testability and
 * reasonability.
 *
 * Functional Design embraces the composability, immutability, and static
 * type safety afforded by functional programming, and applies it to solve
 * real world problems, some of which may be close to business domains, all
 * without the esoteric type classes, category theory jargon, confusing and
 * undiscoverable implicits, monad transformers, and other constructs common
 * in the early days of functional Scala.
 *
 * Functional Design has been used for a number of open source Scala libraries,
 * including ZIO. The techniques provide very powerful, concise, and type-safe
 * solutions to many complex problems within given domains.
 *
 * In this section, you will get a taste for the kind of expressive power,
 * compositionality, declarativity, and type-safety that Functional Design
 * can provide.
 */

object tour:
  object effect:

    /** The shard function creates N workers reading from a Queue, if one of them fails, then wait
      * for the other ones to process their current item, but terminate all the workers.
      *
      * Returns the first error, or never return, if there is no error.
      */
    def shard[A](
      queue: Queue[A],
      count: Int,
      worker: A => Task[Unit]
    ): UIO[Throwable] =
      val qworker =
        ZIO.uninterruptible {
          for
            a <- ZIO.interruptible(queue.take)
            _ <- worker(a).onError(_ => queue.offer(a))
          yield ()
        }.forever

      val qworkers = List.fill(count)(qworker)

      (for
        fiber <- ZIO.forkAll(qworkers)
        list  <- fiber.join
      yield list.head).flip
    end shard
  end effect

  object schedule:
    type Response

    /** Constructs a schedule that will use exponential falloff, starting from 10 milliseconds,
      * until the delay becomes 60 seconds, when it will switch to using a fixed spacing of 60
      * seconds between recurrences, until it reaches 100 recurrences at that spacing, at which
      * point the schedule will no longer recur.
      */
    val schedule =
      (Schedule.exponential(10.millis).whileOutput(_ < 60.seconds) andThen
        (Schedule.fixed(60.seconds) && Schedule.recurs(100))).jittered

    def flakyRequest(url: String): Task[Response] = ???

    lazy val example = flakyRequest("https://google.com").retry(schedule)

  object optics:
    final case class User(name: String, address: Address)
    object User:
      val name: Lens[User, String]     = Lens(_.name, (u, n) => u.copy(name = n))
      val address: Lens[User, Address] = Lens(_.address, (u, a) => u.copy(address = a))
    final case class Address(street: String)
    object Address:
      val street: Lens[Address, String] = Lens(_.street, (u, s) => u.copy(street = s))

    val sherlock = User("Sherlock Holmes", Address("Baker Street"))

    /** Composes a lens for 'address' in 'User' together with a lens for 'street' in 'Address',
      * generating a lens that accesses '.address.street' inside a `User` object. Then uses the lens
      * to make the street name lower case inside the `sherlock` user.
      */
    (User.address >>> Address.street).update(_.toLowerCase)(sherlock)

    final case class Lens[S, A](get: S => A, set: (S, A) => S):
      self =>
      def >>>[B](that: Lens[A, B]): Lens[S, B] =
        Lens((s: S) => that.get(self.get(s)), (s: S, b: B) => self.set(s, that.set(self.get(s), b)))

      def update(f: A => A): S => S = (s: S) => self.set(s, f(self.get(s)))
  end optics

  object parsers:
    type Input = String

    /** Through composition and transformation, parsers an email from simpler pieces.
      */
    lazy val email = username + Parser.char('@') + server

    lazy val letterOrDigit =
      Parser.anyChar.suchThat(c => c.isLetter || c.isDigit, "Expected a letter or digit")

    lazy val username = letterOrDigit.repeatedly

    lazy val tld = Parser.string("com") | Parser.string("net")

    lazy val server = letterOrDigit.repeatedly + Parser.char('.').string + tld

    final case class Parser[+E, +A](parse: Input => Either[E, (Input, A)]):
      self =>
      def flatMap[E1 >: E, B](f: A => Parser[E1, B]): Parser[E1, B] =
        Parser { input =>
          parse(input).flatMap:
            case (input, a) => f(a).parse(input)
        }

      def map[B](f: A => B): Parser[E, B] = self.flatMap(a => Parser.succeed(f(a)))

      def +[E1 >: E, B](
        that: => Parser[E1, B]
      )(implicit ev1: StringLike[A], ev2: StringLike[B]): Parser[E1, String] =
        self.string.zipWith(that.string)(_ + _)

      def ~[E1 >: E, B](that: => Parser[E1, B]): Parser[E1, (A, B)] = self.zip(that)

      def zip[E1 >: E, B](that: => Parser[E1, B]): Parser[E1, (A, B)] =
        for
          a <- self
          b <- that
        yield (a, b)

      def zipWith[E1 >: E, B, C](that: => Parser[E1, B])(f: (A, B) => C): Parser[E1, C] =
        (self zip that).map(f.tupled)

      def *>[E1 >: E, B](that: => Parser[E1, B]): Parser[E1, B] = (self zip that).map(_._2)

      def <*[E1 >: E](that: => Parser[E1, Any]): Parser[E1, A] = (self zip that).map(_._1)

      def |[E1 >: E, A1 >: A](that: => Parser[E1, A1]): Parser[E1, A1] = self.orElse(that)

      def orElse[E1 >: E, A1 >: A](that: => Parser[E1, A1]): Parser[E1, A1] =
        Parser { input =>
          self.parse(input) match
            case Left(_) => that.parse(input)
            case other   => other
        }

      def repeatedly: Parser[E, Vector[A]] =
        self.map(Vector(_)).zipWith(repeatedly)(_ ++ _).orElse(Parser.succeed(Vector()))

      def suchThat[E1 >: E](f: A => Boolean, e: E1): Parser[E1, A] =
        for
          a <- self
          _ <- if f(a) then Parser.succeed(()) else Parser.fail(e)
        yield a

      def string(implicit ev: StringLike[A]): Parser[E, String] =
        self.map(ev).map(_.toString)
    end Parser
    object Parser:
      def succeed[A](a: => A): Parser[Nothing, A] = Parser(input => Right(input -> a))

      def fail[E](e: => E): Parser[E, Nothing] = Parser(_ => Left(e))

      def char(c: Char): Parser[String, Char] =
        for
          char <- anyChar
          _    <- if char == c then Parser.succeed(()) else Parser.fail("Expected " + c)
        yield char

      val anyChar: Parser[String, Char] =
        Parser { input =>
          if input.length == 0 then Left("Need More input!")
          else Right(input.drop(1) -> input.charAt(0))
        }

      def string(s: String): Parser[String, String] =
        Parser { input =>
          if input.startsWith(s) then Right(input.drop(s.length) -> s)
          else Left(s"Expected ${s} but found " + input.take(s.length) + "...")
        }
    end Parser

    trait StringLike[-A] extends (A => String):
      final def apply(a: A): String = makeString(a)
      def makeString(a: A): String
    object StringLike:
      implicit val stringLikeChar: StringLike[Char] = _.toString

      implicit def stringLikeVector[A](implicit sl: StringLike[A]): StringLike[Vector[A]] =
        (vector: Vector[A]) => vector.map(sl.makeString(_)).mkString("")

      implicit val stringLikeString: StringLike[String] = identity[String](_)
  end parsers
end tour
