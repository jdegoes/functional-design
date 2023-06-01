package net.degoes

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
        
    val always: EmailFilter = !never

    val never: EmailFilter = bodyContains("") && !bodyContains("")

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
end email_filter3

/** COMPOSABILITY - EXERCISE SET 2
  */
object ui_components:

  object executable:
    /** EXERCISE 1
      *
      * The following API is not composable—there is no domain. Introduce a domain with elements,
      * constructors, and composable operators. Use an executable model.
      */
    trait Turtle:
      self =>
      def turnLeft(degrees: Int): Unit

      def turnRight(degrees: Int): Unit

      def goForward(): Unit

      def goBackward(): Unit

      def draw(): Unit

  object declarative:
    /** EXERCISE 2
      *
      * The following API is not composable—there is no domain. Introduce a domain with elements,
      * constructors, and composable operators. Use a declarative model.
      */
    trait Turtle:
      self =>
      def turnLeft(degrees: Int): Unit

      def turnRight(degrees: Int): Unit

      def goForward(): Unit

      def goBackward(): Unit

      def draw(): Unit
end ui_components
