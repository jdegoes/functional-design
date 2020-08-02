package net.degoes

import java.time.{ Instant, LocalDate, LocalDateTime }

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
  sealed trait CreditCard
  object CreditCard {
    case class Visa(number: Number, name: String, expirationDate: LocalDate, cvv: SecurityCode) extends CreditCard
    case class MasterCard()                                                                     extends CreditCard
  }

  case class Number(number: List[Digit])
  case class SecurityCode(first: Digit, second: Digit, third: Digit)

  sealed trait Digit
  object Digit {
    case object $1 extends Digit
    case object $2 extends Digit
    case object $3 extends Digit
    case object $4 extends Digit
    case object $5 extends Digit
    case object $6 extends Digit
    case object $7 extends Digit
    case object $8 extends Digit
    case object $9 extends Digit
  }

  /**
   * EXERCISE 2
   *
   * Using only sealed traits and case classes, create an immutable data model
   * of a product, which could be a physical product, such as a gallon of milk,
   * or a digital product, such as a book or movie, or access to an event, such
   * as a music concert or film showing.
   */
  sealed trait Product
  object Product {
    case class PhysicalProduct(price: Double, weight: Double, dimension: Dimension) extends Product
    case class DigitalProduct(price: Double, producer: String, title: String)       extends Product
    case class EventAccess(price: Double, date: LocalDate, place: String)           extends Product
  }

  case class Dimension(width: Double, height: Double)

  /**
   * EXERCISE 3
   *
   * Using only sealed traits and case classes, create an immutable data model
   * of a product price, which could be one-time purchase fee, or a recurring
   * fee on some regular interval.
   */
  sealed trait PricingScheme
  object PricingScheme {
    case class Subscription(price: Double, expirationDate: LocalDate) extends PricingScheme
    case class Purchase(price: Double, discount: Double)              extends PricingScheme
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
  sealed trait Event
  object Event {
    sealed trait UserEvent extends Event
    object UserEvent {
      case class UserPurchase(id: Int, item: String, price: Double, time: Instant, userName: String) extends UserEvent
      case class UserAccountCreated(id: Int, userName: String, time: Instant)                        extends UserEvent
    }

    sealed trait DeviceEvent extends Event {
      case class SensorUpdated(id: Int, deviceId: Int, time: Instant, reading: Option[Double]) extends DeviceEvent
      case class DeviceActivated(id: Int, deviceId: Int, time: Instant)                        extends DeviceEvent
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
  case class Document(
    id: DocId,
    title: String,
    content: DocContent,
    extension: Extension,
    dateCreated: LocalDateTime,
    size: Double,
    access: AccessType,
    permissions: List[DocPermissions]
  )

  sealed trait Extension {
    case object TXT extends Extension
    case object XLS extends Extension
    case object DOC extends Extension
  }

  /**
   * EXERCISE 2
   *
   * Using only sealed traits and case classes, create a model of the access
   * type that a given user might have with respect to a document. For example,
   * some users might have read-only permission on a document.
   */
  sealed trait AccessType
  object AccessType {
    case object Admin extends AccessType
    case object Guest extends AccessType
    case object User  extends AccessType
  }

  /**
   * EXERCISE 3
   *
   * Using only sealed traits and case classes, create a model of the
   * permissions that a user has on a set of documents they have access to.
   * Do not store the document contents themselves in this model.
   */
  sealed trait DocPermissions
  object DocPermissions {
    case object ChangeDirectory extends DocPermissions
    case object ReadOnly        extends DocPermissions
    case object Delete          extends DocPermissions
    case object Write           extends DocPermissions
    case object Copy            extends DocPermissions
  }
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
  case class Customer(name: String, Age: Int, address: Address, gender: Gender)

  case class Address(city: String, distinct: String, street: String, houseNumber: Int, apartment: Int)

  sealed trait Gender
  object Gender {
    case object Male   extends Gender
    case object Female extends Gender
  }

  /**
   * EXERCISE 2
   *
   * Using only sealed traits and case classes, develop a model of an account
   * type. For example, one account type allows the user to write checks
   * against a given currency. Another account type allows the user to earn
   * interest at a given rate for the holdings in a given currency.
   */
  sealed trait AccountType
  object AccountType {
    case object Deposit extends AccountType
    case object Credit  extends AccountType
    case object Card    extends AccountType
  }

  /**
   * EXERCISE 3
   *
   * Using only sealed traits and case classes, develop a model of a bank
   * account, including details on the type of bank account, holdings, customer
   * who owns the bank account, and customers who have access to the bank account.
   */
  case class Account(
    owner: Customer,
    balance: Double,
    lastWithdraw: Double,
    lastWithdrawDate: LocalDateTime,
    accountType: AccountType,
    hasAccess: List[Customer]
  )
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
  sealed trait Exchange
  object Exchange {
    case object NewYorkStockExchange extends Exchange
    case object Nasdaq               extends Exchange
    case object LondonStockExchange  extends Exchange
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
    case object GBP extends CurrencyType
  }

  /**
   * EXERCISE 3
   *
   * Using only sealed traits and case classes, develop a model of a stock
   * symbol. Ensure there exists a value for Apple's stock (APPL).
   */
  sealed trait StockSymbol
  object StockSymbol {
    case object APPL extends StockSymbol
    case object DIS  extends StockSymbol
    case object MSFT extends StockSymbol
    case object TSLA extends StockSymbol
  }

  /**
   * EXERCISE 4
   *
   * Using only sealed traits and case classes, develop a model of a portfolio
   * held by a user of the web application.
   */
  case class Portfolio(map: Map[Exchange, List[Stock]])

  /**
   * EXERCISE 5
   *
   * Using only sealed traits and case classes, develop a model of a user of
   * the web application.
   */
  case class User(login: String, pass: String, stocks: List[Stock], totalCost: Double, lastUpdate: Stock)

  case class Stock(
    fullName: String,
    symbol: StockSymbol,
    value: Double,
    currencyType: CurrencyType,
    lastChange: Double,
    exchange: Exchange
  )

  /**
   * EXERCISE 6
   *
   * Using only sealed traits and case classes, develop a model of a trade type.
   * Example trade types might include Buy and Sell.
   */
  sealed trait TradeType
  object TradeType {
    case object Buy  extends TradeType
    case object Sell extends TradeType
  }

  /**
   * EXERCISE 7
   *
   * Using only sealed traits and case classes, develop a model of a trade,
   * which involves a particular trade type of a specific stock symbol at
   * specific prices.
   */
  case class Trade(
    tradeType: TradeType,
    exchange: Exchange,
    who: User,
    stock: Stock,
    quantity: Int,
    totalCost: Double,
    commission: Double,
    timestamp: LocalDateTime
  )
}
