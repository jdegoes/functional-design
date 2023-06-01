package net.degoes

import scala.annotation.tailrec

/*
 * INTRODUCTION
 *
 * In Functional Design, composable operators allow building infinitely many
 * solutions from a few operators and domain constructors.
 *
 * Operators and constructors are either primitive, meaning they cannot be
 * expressed in terms of others, or they are derived, meaning they can be
 * expressed in terms of other operators or constructors.
 *
 * The choice of primitives determine how powerful and expressive a domain
 * model is. Some choices lead to weaker models, and others, to more powerful
 * models. Power is not always a good thing: constraining the power of a model
 * allows more efficient and more feature-full execution.
 *
 * Derived operators and constructors bridge the gap from the domain, to common
 * problems that a user of the domain has to solve, improving productivity.
 *
 * In many domains, there exist many potential choices for the set of primitive
 * operators and constructors. But not all choices are equally good.
 *
 * The best primitives are:
 *
 * * Composable, to permit a lot of power in a small, reasonable package
 * * Expressive, to solve the full range of problems in the domain
 * * Orthogonal, such that no primitive provides the capabilities of any other
 *
 * Orthogonality also implies minimalism, which means the primitives are the
 * smallest set of orthogonal primitives that exist.
 *
 */

/** ORTHOGONALITY - EXERCISE SET 1
  */
object email_filter3:
  final case class Address(emailAddress: String)
  final case class Email(sender: Address, to: List[Address], subject: String, body: String)

  /** EXERCISE 1
    *
    * In the following model, which describes an email filter, there are many primitives with
    * overlapping responsibilities. Find the smallest possible set of primitive operators and
    * constructors, without deleting any constructors or operators (you may implement them in terms
    * of primitives).
    *
    * NOTE: You may *not* use a final encoding, which would allow you to collapse everything down to
    * one primitive.
    */
  enum EmailFilter:
    case Const(value: Boolean)
    case And(left: EmailFilter, right: EmailFilter)
    case Not(value: EmailFilter)
    case SenderEquals(target: Address)
    case RecipientEquals(target: Address)
    case BodyContains(phrase: String)
    case SubjectContains(phrase: String)

    def self = this

    def &&(that: EmailFilter): EmailFilter = EmailFilter.And(self, that)

    def ||(that: EmailFilter): EmailFilter = !(!self && !that)

    def ^^(that: EmailFilter): EmailFilter = (self && !that) || (!self && that)

    def unary_! : EmailFilter = EmailFilter.Not(self)
  end EmailFilter
  object EmailFilter:
    object Or:
      //  !(!self && !that)
      def unapply(arg: EmailFilter): Option[(EmailFilter, EmailFilter)] = arg match
        case Not(And(Not(left), Not(right))) => Some((left, right))
        case _ => None

    val always: EmailFilter = Const(true)

    val never: EmailFilter = Const(false)

    def senderIs(sender: Address): EmailFilter = SenderEquals(sender)

    def senderIsNot(sender: Address): EmailFilter = !senderIs(sender)

    def recipientIs(recipient: Address): EmailFilter = RecipientEquals(recipient)

    def recipientIsNot(recipient: Address): EmailFilter = !recipientIs(recipient)

    def senderIn(senders: Set[Address]): EmailFilter = 
      senders.foldLeft(never)(_ || senderIs(_))

    def recipientIn(recipients: Set[Address]): EmailFilter = 
      recipients.foldLeft(never)(_ || recipientIs(_))

    def bodyContains(phrase: String): EmailFilter = BodyContains(phrase)

    def bodyDoesNotContain(phrase: String): EmailFilter = !bodyContains(phrase)

    def subjectContains(phrase: String): EmailFilter = SubjectContains(phrase)

    def subjectDoesNotContain(phrase: String): EmailFilter = !subjectContains(phrase)
  end EmailFilter


  enum MatchTodo:
    case And(right: EmailFilter) 
    case Not 

  extension (filter: EmailFilter) def matches(email: Email): Boolean = 
    @tailrec
    def loop(filter: EmailFilter, todo: List[MatchTodo]): Boolean = 
      inline def continue(inline bool: Boolean) = loop(EmailFilter.Const(bool), todo)

      filter match
        case EmailFilter.Const(value) => 
          todo match 
            case Nil => value
            case MatchTodo.Not :: todo => loop(EmailFilter.Const(!value), todo)

            case MatchTodo.And(right) :: tail => 
              if value then loop(right, tail)
              else loop(EmailFilter.Const(false), tail)

        case EmailFilter.And(left, right) => 
          loop(left, MatchTodo.And(right) :: todo)

        case EmailFilter.Not(value) => 
          loop(value, MatchTodo.Not :: todo)

        case EmailFilter.SenderEquals(target) => 
          continue(email.sender == target)

        case EmailFilter.RecipientEquals(target) => 
          continue(email.to.contains(target))

        case EmailFilter.BodyContains(phrase) => 
          continue(email.body.contains(phrase))

        case EmailFilter.SubjectContains(phrase) => 
          continue(email.subject.contains(phrase))

    loop(filter, Nil)

  lazy val emailFilter1 = 
    EmailFilter.subjectContains("discount") && 
    EmailFilter.bodyContains("N95") && 
    !EmailFilter.recipientIs(Address("john@doe.com"))

  @main
  def test3 = 
    println(emailFilter1.matches(Email(Address("john@doe.com"), Address("john@doe.com") :: Nil, "discount", "N95")))

end email_filter3

/** COMPOSABILITY - EXERCISE SET 2
  */
object ui_components:
  trait Turtle:
    self =>
    def turnLeft(degrees: Int): Unit

    def turnRight(degrees: Int): Unit

    def goForward(): Unit

    def goBackward(): Unit

    def draw(): Unit

  abstract class BitmapTurtle(width: Int, height: Int) extends Turtle:
    def bitmap: Array[Array[Boolean]] = ???

  def createTurtle(width: Int, height: Int): BitmapTurtle = ??? 

  object executable:
    /** EXERCISE 1
      *
      * The following API is not composable—there is no domain. Introduce a domain with elements,
      * constructors, and composable operators. Use an executable model.
      */
    final case class TurtleTodo(draw: Turtle => Unit):
      self =>
      def + (that: TurtleTodo): TurtleTodo = 
        TurtleTodo: turtle => 
          self.draw(turtle)
          that.draw(turtle)

      def turnRight(degrees: Int): TurtleTodo = self + TurtleTodo.turnRight(degrees)

      def turnLeft(degrees: Int): TurtleTodo = self + TurtleTodo.turnLeft(degrees)

      def goForward: TurtleTodo = self + TurtleTodo.goForward

      def goBackward: TurtleTodo = self + TurtleTodo.goBackward

    object TurtleTodo: 
      val empty: TurtleTodo = TurtleTodo(_ => ())

      def turnRight(degrees: Int): TurtleTodo = TurtleTodo(_.turnRight(degrees))

      def turnLeft(degrees: Int): TurtleTodo = TurtleTodo(_.turnLeft(degrees))

      def goForward: TurtleTodo = TurtleTodo(_.goForward())

      def goBackward: TurtleTodo = TurtleTodo(_.goBackward())

      def draw: TurtleTodo = TurtleTodo(_.draw())

  object declarative:
    /*
    enum TurtleStep:
      case TurnRight
      case GoForward
      case Draw
      case Sequence(left: TurtleStep, right: TurtleStep)

    def empty = AndThen(GoForward, GoBackward)
    */

    /** EXERCISE 2
      *
      * The following API is not composable—there is no domain. Introduce a domain with elements,
      * constructors, and composable operators. Use a declarative model.
      */
    enum TurtleStep:
      case TurnRight
      case GoForward
      case Draw

    final case class TurtleTodo(steps: Vector[TurtleStep]):
      self =>
      def + (that: TurtleTodo): TurtleTodo = TurtleTodo(self.steps ++ that.steps)

      def turnRight(degrees: Int): TurtleTodo = self + TurtleTodo.turnRight(degrees)

      def turnLeft(degrees: Int): TurtleTodo = self + TurtleTodo.turnLeft(degrees)

      def goForward: TurtleTodo = self + TurtleTodo.goForward

      def goBackward: TurtleTodo = self + TurtleTodo.goBackward

      def draw: TurtleTodo = self + TurtleTodo.draw
    object TurtleTodo:
      def empty = TurtleTodo(Vector.empty)

      def turnRight(degrees: Int): TurtleTodo = 
        (0 until degrees).foldLeft(empty)((acc, _) => acc + TurtleTodo(Vector(TurtleStep.TurnRight)))

      def turnLeft(degrees: Int): TurtleTodo = 
        turnRight(360 - degrees)

      def goForward: TurtleTodo = TurtleTodo(Vector(TurtleStep.GoForward))

      def goBackward: TurtleTodo = 
        turnLeft(180) + goForward + turnLeft(180)

      def draw: TurtleTodo = TurtleTodo(Vector(TurtleStep.Draw))


end ui_components
