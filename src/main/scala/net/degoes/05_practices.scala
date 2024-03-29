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
    case Always
    case Never
    case And(left: EmailFilter, right: EmailFilter)
    case InclusiveOr(left: EmailFilter, right: EmailFilter)
    case ExclusiveOr(left: EmailFilter, right: EmailFilter)
    case SenderEquals(target: Address)
    case SenderNotEquals(target: Address)
    case RecipientEquals(target: Address)
    case RecipientNotEquals(target: Address)
    case SenderIn(targets: Set[Address])
    case RecipientIn(targets: Set[Address])
    case BodyContains(phrase: String)
    case BodyNotContains(phrase: String)
    case SubjectContains(phrase: String)
    case SubjectNotContains(phrase: String)

    def self = this

    def &&(that: EmailFilter): EmailFilter = EmailFilter.And(self, that)

    def ||(that: EmailFilter): EmailFilter = EmailFilter.InclusiveOr(self, that)

    def ^^(that: EmailFilter): EmailFilter = EmailFilter.ExclusiveOr(self, that)
  end EmailFilter
  object EmailFilter:
    val always: EmailFilter = Always

    val never: EmailFilter = Never

    def senderIs(sender: Address): EmailFilter = SenderEquals(sender)

    def senderIsNot(sender: Address): EmailFilter = SenderNotEquals(sender)

    def recipientIs(recipient: Address): EmailFilter = RecipientEquals(recipient)

    def recipientIsNot(recipient: Address): EmailFilter = RecipientNotEquals(recipient)

    def senderIn(senders: Set[Address]): EmailFilter = SenderIn(senders)

    def recipientIn(recipients: Set[Address]): EmailFilter = RecipientIn(recipients)

    def bodyContains(phrase: String): EmailFilter = BodyContains(phrase)

    def bodyDoesNotContain(phrase: String): EmailFilter = BodyNotContains(phrase)

    def subjectContains(phrase: String): EmailFilter = SubjectContains(phrase)

    def subjectDoesNotContain(phrase: String): EmailFilter = SubjectNotContains(phrase)
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
