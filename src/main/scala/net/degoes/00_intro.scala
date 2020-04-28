package net.degoes

import zio._
import zio.duration._

/**
 * Functional design appears in a number of Scala libraries, including ZIO.
 * It provides very powerful, concise, and type-safe solutions to a many
 * complex problems within given domains.
 */
object tour {
  object effect {

    /**
     * The shard function creates N workers reading from a Queue, if one of
     * them fails, then wait for the other ones to process their current item,
     * but terminate all the workers.
     *
     * Returns the first error, or never return, if there is no error.
     */
    def shard[A](
      queue: Queue[A],
      count: Int,
      worker: A => Task[Unit]
    ): Task[Throwable] = {
      val qworker =
        ZIO.uninterruptible {
          for {
            a <- ZIO.interruptible(queue.take)
            _ <- worker(a).onError(_ => queue.offer(a))
          } yield ()
        }.forever

      val qworkers = List.fill(count)(qworker)

      (for {
        fiber <- ZIO.forkAll(qworkers)
        list  <- fiber.join
      } yield list.head).flip
    }
  }

  object schedule {
    type Response

    /**
     * Constructs a schedule that will use exponential falloff, starting from
     * 10 milliseconds, until the delay becomes 60 seconds, when it will switch
     * to using a fixed spacing of 60 seconds between recurrences, until it
     * reaches 100 recurrences at that spacing, at which point the schedule
     * will no longer recur.
     */
    val schedule =
      (Schedule.exponential(10.millis).whileOutput(_ < 60.seconds) andThen
        (Schedule.fixed(60.seconds) && Schedule.recurs(100))).jittered

    def flakyRequest(url: String): Task[Response] = ???

    lazy val example = flakyRequest("https://google.com").retry(schedule)
  }
}
