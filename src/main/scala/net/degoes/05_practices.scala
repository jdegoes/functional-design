package net.degoes

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
 * models. Power is not always a good thing; constraining the power of a model
 * allows more efficient and more feature-full execution.
 *
 * Derived operators and constructors bridge the gap from the domain, to common
 * problems that a user of the domain has to solve, improving productivity.
 *
 * In many domains, there exist many potential choices for the set of primitive
 * operators and constructors. Not all choices are equally good.
 *
 * The best primitives are:
 *
 * * Composable, to permit a lot of power in a small, reasonable package
 * * Minimal, for the desired level of power
 * * Orthogonal, such that no primitive provides the capabilities of any other
 *
 */

/**
 * ORTHOGONALITY - EXERCISE SET 1
 */
object email_filter3 {
  final case class Address(emailAddress: String)
  final case class Email(sender: Address, to: List[Address], subject: String, body: String)

  /**
   * EXERCISE 1
   *
   * In the following model, which describes an email filter, there are many
   * primitives with overlapping responsibilities. Find the smallest possible
   * set of primitive operators and constructors, without deleting any
   * constructors or operators (you may implement them in terms of primitives).
   *
   * NOTE: You may *not* use a final encoding, which would allow you to
   * collapse everything down to one primitive.
   */
  sealed trait EmailFilter { self =>
    def &&(that: EmailFilter): EmailFilter = EmailFilter.And(self, that)

    def ||(that: EmailFilter): EmailFilter = EmailFilter.InclusiveOr(self, that)

    def ^^(that: EmailFilter): EmailFilter = EmailFilter.ExclusiveOr(self, that)
  }
  object EmailFilter {
    final case object Always                                            extends EmailFilter
    final case object Never                                             extends EmailFilter
    final case class And(left: EmailFilter, right: EmailFilter)         extends EmailFilter
    final case class InclusiveOr(left: EmailFilter, right: EmailFilter) extends EmailFilter
    final case class ExclusiveOr(left: EmailFilter, right: EmailFilter) extends EmailFilter
    final case class SenderEquals(target: Address)                      extends EmailFilter
    final case class SenderNotEquals(target: Address)                   extends EmailFilter
    final case class RecipientEquals(target: Address)                   extends EmailFilter
    final case class RecipientNotEquals(target: Address)                extends EmailFilter
    final case class SenderIn(targets: Set[Address])                    extends EmailFilter
    final case class RecipientIn(targets: Set[Address])                 extends EmailFilter
    final case class BodyContains(phrase: String)                       extends EmailFilter
    final case class BodyNotContains(phrase: String)                    extends EmailFilter

    val always: EmailFilter = Always

    val never: EmailFilter = Always

    def senderIs(sender: Address): EmailFilter = SenderEquals(sender)

    def senderIsNot(sender: Address): EmailFilter = SenderNotEquals(sender)

    def recipientIs(recipient: Address): EmailFilter = RecipientEquals(recipient)

    def recipientIsNot(recipient: Address): EmailFilter = RecipientNotEquals(recipient)

    def senderIn(senders: Set[Address]): EmailFilter = SenderIn(senders)

    def recipientIn(recipients: Set[Address]): EmailFilter = RecipientIn(recipients)

    def bodyContains(phrase: String): EmailFilter = BodyContains(phrase)

    def bodyDoesNotContain(phrase: String): EmailFilter = BodyNotContains(phrase)
  }
}

/**
 * COMPOSABILITY - EXERCISE SET 2
 */
object ui_components {

  /**
   * EXERCISE 1
   *
   * The following API is not composable—there is no domain. Introduce a
   * domain with elements, constructors, and composable operators.
   */
  trait Turtle {
    def turnLeft(): Unit

    def turnRight(): Unit

    def goForward(): Unit

    def goBackward(): Unit

    def draw(): Unit
  }

  /**
   * EXERCISE 2
   *
   * The following API is not composable—there is no domain. Introduce a
   * domain with elements, constructors, and composable operators.
   */
  trait WidgetFramework {
    type Widget[+State]

    def createContainer: Widget[Any]

    def createTextField(name: String, chars: Int): Widget[String]

    def getState[A](widget: Widget[A]): A

    def installChild(container: Widget[Any], child: Widget[Any]): Unit

    def createButton(label: String, onClick: Widget[Unit] => Unit): Widget[Unit]

    def displayWidget(widget: Widget[Any]): Unit

    def destroyWidget(widget: Widget[Any]): Unit
  }
}
