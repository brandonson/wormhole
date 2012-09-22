package wormhole

import akka.util.Timeout
import akka.util.duration._
import java.util.concurrent.TimeUnit
import akka.dispatch.Await
import akka.dispatch.Future

/**
 * Contains implicit constants and a method for use with the akka ask pattern.  Timeouts and wait durations are currently 5 seconds.
 */
package object actor {

	private val WaitSeconds = 5
	
	implicit val timeout = new Timeout(WaitSeconds, TimeUnit.SECONDS)
	implicit val duration = WaitSeconds seconds
	
	def fetch[T](future:Future[T]):T = Await.result(future, duration)
}