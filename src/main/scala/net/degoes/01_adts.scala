package net.degoes

import java.time.{Instant, LocalDate}
import java.util.{Currency}

/*
 * INTRODUCTION
 *
 * Functional Design depends heavily on functional data modeling. Functional
 * data modeling is the task of creating precise, type-safe models of a given
 * domain using algebraic data types and generalized algebraic data types.
 *
 * In this section, you'll review basic functional domain modeling.
 */

/**
 * E-COMMERCE - EXERCISE SET 1
 *
 * Consider an e-commerce application that allows users to purchase products.
 */
object credit_card {

  /**
   * EXERCISE 1
   *
   * Using only sealed traits and case classes, create an immutable data model
   * of a credit card, which must have:
   *
   *  * Number
   *  * Name
   *  * Expiration date
   *  * Security code
   */

  // smart constructor pattern Name("foo") is illegal. This is encouraged for production code even though it
  // does not fit the ADT idiom directly.
  sealed abstract case class Name private (value: String)
  object Name {
    def fromString(s: String): Option[Name] =
      if (s.trim.isEmpty) None
      else Some(new Name(s.trim){})
  }

  case class CreditCard(number: Long, name: String, expiration: LocalDate, securityCode: Int) // LocalDate is too big since we need Month and Year actually
  // likewise Int is too big because 3 or 4 digits should suffice, Long may not be enough, possibly BigInt required.
  /**
   * EXERCISE 2
   *
   *
   * Using only sealed traits and case classes, create an immutable data model
   * of a product, which could be a physical product, such as a gallon of milk,
   * or a digital product, such as a book or movie, or access to an event, such
   * as a music concert or film showing.
   */
  sealed trait Product { // self =>
    def price: BigDecimal // avoid val for Heaven's sake!
    //possible
    // def price = self match { // price names in case class clearly don't matter in this instance! And this is definitely not OO as
    // we have exhaustive pattern match here, so including price in here is still FP modelling.
    // case PhysicalGood(p) => p
    // case DigitalGood(p) => p
    // case Event(p) => p
    // } It's the pattern matching here that forces us to ensure all terms of the trait implement the price
    // as opposed to a concept of OO inheritance. Nonetheless the OO encoding is far more convenient as
    // the pattern match is verbose and represents more syntax! But only if it's just a member/field/property
    // If you DO have business logic to compute it than the common sealed trait should do the work and do pattern matching.
    // def isDigitalGood matching against self! Better to centralize is there as you see it one place than have each term do it.
  }

  object Product {
    // Interestingly he suggests putting the functions in the trait, not in the object companion!
    // This way you have the function directly available on the object just like normal OO!
    // He even says Haskell developers wish they had that!
    case class PhysicalGood(name: String, price: BigDecimal) extends Product
    case class DigitalGood(name: String, price: BigDecimal) extends Product
    case class Event(name: String, price: BigDecimal, time: LocalDate) extends Product
  }

  /**
   * EXERCISE 3
   *
   * Using only sealed traits and case classes, create an immutable data model
   * of a product price, which could be one-time purchase fee, or a recurring
   * fee on some regular interval.
   */
  //type ProductPrice PricingScheme, below is from Pierre
  sealed trait Interval
  object Interval {
    case object Monthly extends Interval
    case object Yearly  extends Interval
  }

  sealed trait ProductPrice
  object ProductPrice {
    case class OneTimeFee(fee: BigDecimal)                       extends ProductPrice
    case class RecurringFee(fee: BigDecimal, interval: Interval) extends ProductPrice
  }
}

/**
 * EVENT PROCESSING - EXERCISE SET 3
 *
 * Consider an event processing application, which processes events from both
 * devices, as well as users.
 */
object events {

  /**
   * EXERCISE
   *
   * Refactor the object-oriented data model in this section to a more
   * functional one, which uses only sealed traits and case classes.
   */
  //abstract class Event(val id: Int) {
  //
  //  def time: Instant
  //}
  //
  //// Events are either UserEvent (produced by a user) or DeviceEvent (produced by a device),
  //// please don't extend both it will break code!!!
  //trait UserEvent extends Event {
  //  def userName: String
  //}
  //
  //// Events are either UserEvent (produced by a user) or DeviceEvent (produced by a device),
  //// please don't extend both it will break code!!!
  //trait DeviceEvent extends Event {
  //  def deviceId: Int
  //}
  //
  //class SensorUpdated(id: Int, val deviceId: Int, val time: Instant, val reading: Option[Double])
  //    extends Event(id)
  //    with DeviceEvent
  //
  //class DeviceActivated(id: Int, val deviceId: Int, val time: Instant) extends Event(id) with DeviceEvent
  //
  //class UserPurchase(id: Int, val item: String, val price: Double, val time: Instant, val userName: String)
  //    extends Event(id)
  //    with UserEvent
  //
  //class UserAccountCreated(id: Int, val userName: String, val time: Instant) extends Event(id) with UserEvent
  //
  // from Adam Fraser and Pierre Ricadat and John
  sealed trait Event {
    def id: Int
    def time: Instant
  }

  object Event {

    // Option 1
    //sealed trait DeviceEvent extends Event { // sealed is redundant but robust to refactoring
    //  def deviceId: Int
    //}
    //
    //object DeviceEvent {
    //
    //  final case class SensorUpdated(id: Int, deviceId: Int, time: Instant, reading: Option[Double])
    //    extends DeviceEvent
    //
    //  final case class DeviceActivated(id: Int, deviceId: Int, time: Instant) extends DeviceEvent
    //
    //}

    // prefer the sums to be pushed down not at top level because they are more cumbersome to work with. YES!
    final case class DeviceEventPayload(id: Int, time: Instant, deviceId: Int)
    sealed trait DeviceEventSum
    object DeviceEventSum {
      final case class SensorUpdated(reading: Option[Double]) extends DeviceEventSum
      final case object DeviceActivated extends DeviceEventSum
    }
    final case class DeviceEvent(payload: DeviceEventPayload, custom: DeviceEventSum) extends Event {
      def id: Int = payload.id
      def time: Instant  = payload.time
    }

    sealed trait UserEvent extends Event { // note continue to make it sealed if not in object but in object not needed and use def
      def userName: String
    }

    object UserEvent {

      // note that with the scoping we can just say Purchase!
      //final case class UserPurchase(id: Int, item: String, price: Double, time: Instant, userName: String)
      final case class Purchase(id: Int, item: String, price: Double, time: Instant, userName: String)
        extends UserEvent

      // not UserAccountCreated
      final case class AccountCreated(id: Int, userName: String, time: Instant) extends UserEvent

    }

  }
}

/**
 * DOCUMENT EDITING - EXERCISE SET 4
 *
 * Consider a web application that allows users to edit and store documents
 * of some type (which is not relevant for these exercises).
 */
object documents {
  final case class UserId(identifier: String)
  final case class DocId(identifier: String)
  final case class DocContent(body: String)

  /**
   * EXERCISE 1
   *
   * Using only sealed traits and case classes, create a simplified but somewhat
   * realistic model of a Document.
   */
  //type Document
  final case class Document(userId: UserId, id: DocId, content: DocContent)

  /**
   * EXERCISE 2
   *
   * Using only sealed traits and case classes, create a model of the access
   * type that a given user might have with respect to a document. For example,
   * some users might have read-only permission on a document.
   */
   //type AccessType
  sealed trait AccessType
  object AccessType {
    case object NoAccess extends AccessType
    case object Read extends AccessType
    case object Write extends AccessType
    case object Share extends AccessType
  }
  /**
   * EXERCISE 3
   *
   * Using only sealed traits and case classes, create a model of the
   * permissions that a user has on a set of documents they have access to.
   * Do not store the document contents themselves in this model.
   */
  //type DocPermissions, interesting problem as we don't encode everything, we encode what we need only.
  // Adam
  final case class DocPermissions(user: UserId, permissions: Map[DocId, AccessType]) // this could be a set of access types, it depends.
}

/**
 * BANKING - EXERCISE SET 5
 *
 * Consider a banking application that allows users to hold and transfer money.
 */
object bank {

  /**
   * EXERCISE 1
   *
   * Using only sealed traits and case classes, develop a model of a customer at a bank.
   */
  //type Customer

  //Adam Fraser @adamgfraser Apr 27 06:41
  final case class Customer(name: String, customerID: Long)
  /**
   * EXERCISE 2
   *
   * Using only sealed traits and case classes, develop a model of an account
   * type. For example, one account type allows the user to write checks
   * against a given currency. Another account type allows the user to earn
   * interest at a given rate for the holdings in a given currency.
   */
  //type AccountType
  sealed trait AccountType
  object AccountType {
    // sealed trait is preferred over sealed abstract class which may be needed for Scala 2.11
    final case class InterestChecking(currency: Currency, id: BigInt, interestRate: BigDecimal ) extends AccountType
    final case class InterestSaving(currency: Currency, id: BigInt, interestRate: BigDecimal)  extends AccountType
    final case class MMA(currency: Currency, id: BigInt, interestRate: BigDecimal)  extends AccountType
    case object PersonalChecking extends AccountType
    case object BusinessChecking extends AccountType
  }

  /**
   * EXERCISE 3
   *
   * Using only sealed traits and case classes, develop a model of a bank
   * account, including details on the type of bank account, holdings, customer
   * who owns the bank account, and customers who have access to the bank account.
   */
  //type Account, permissions are definitely lacking in this approximation.
  final case class Account(customer: Customer, accountType: AccountType, holdings: BigDecimal, access: Set[Customer])
}

/**
 * STOCK PORTFOLIO - GRADUATION PROJECT
 *
 * Consider a web application that allows users to manage their portfolio of investments.
 */
object portfolio {

  /**
   * EXERCISE 1
   *
   * Using only sealed traits and case classes, develop a model of a stock
   * exchange. Ensure there exist values for NASDAQ and NYSE.
   */
  //type Exchange

  /**
   * EXERCISE 2
   *
   * Using only sealed traits and case classes, develop a model of a currency
   * type.
   */
  //type CurrencyType

  /**
   * EXERCISE 3
   *
   * Using only sealed traits and case classes, develop a model of a stock
   * symbol. Ensure there exists a value for Apple's stock (APPL).
   */
  //type StockSymbol

  /**
   * EXERCISE 4
   *
   * Using only sealed traits and case classes, develop a model of a portfolio
   * held by a user of the web application.
   */
  //type Portfolio

  /**
   * EXERCISE 5
   *
   * Using only sealed traits and case classes, develop a model of a user of
   * the web application.
   */
  //type User

  /**
   * EXERCISE 6
   *
   * Using only sealed traits and case classes, develop a model of a trade type.
   * Example trade types might include Buy and Sell.
   */
  //type TradeType

  /**
   * EXERCISE 7
   *
   * Using only sealed traits and case classes, develop a model of a trade,
   * which involves a particular trade type of a specific stock symbol at
   * specific prices.
   */
  //type Trade
  // Adrian Filip
  sealed trait Exchange
  object Exchange {
    case object NASDAQ
    case object NYSE
  }

  /**
   * EXERCISE 2
   *
   * Using only sealed traits and case classes, develop a model of a currency
   * type.
   */
  sealed trait CurrencyType
  object CurrencyType {
    case object USD extends CurrencyType
    case object EUR extends CurrencyType
  }

  /**
   * EXERCISE 3
   *
   * Using only sealed traits and case classes, develop a model of a stock
   * symbol. Ensure there exists a value for Apple's stock (APPL).
   */
  sealed trait StockSymbol
  object StockSymbol {
    case object AAPL extends StockSymbol
    case object ORCL extends StockSymbol
  }

  /**
   * EXERCISE 4
   *
   * Using only sealed traits and case classes, develop a model of a portfolio
   * held by a user of the web application.
   */
  case class Portfolio(userId: Long, stocks: Map[(StockSymbol, CurrencyType), BigDecimal])

  /**
   * EXERCISE 5
   *
   * Using only sealed traits and case classes, develop a model of a user of
   * the web application.
   */
  final case class User(id: Long, name: String)

  /**
   * EXERCISE 6
   *
   * Using only sealed traits and case classes, develop a model of a trade type.
   * Example trade types might include Buy and Sell.
   */
  sealed trait TradeType
  object TradeType{
    case object Buy
    case object Sell
  }

  /**
   * EXERCISE 7
   *
   * Using only sealed traits and case classes, develop a model of a trade,
   * which involves a particular trade type of a specific stock symbol at
   * specific prices.
   */
  case class Trade(tip: TradeType, stock: StockSymbol, exchange: Exchange, currency: CurrencyType, amount: BigDecimal)
}
