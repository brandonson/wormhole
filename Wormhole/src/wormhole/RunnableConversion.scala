package wormhole

import java.util.TimerTask
/**
 * Allows conversions from simple no-arg methods to Runnables or TimerTasks.
 */
object RunnableConversion {
	implicit def methodToRunnable(conv:() => _) = new Runnable {def run(){conv()}}
	implicit def methodToTimerTask(conv:()=> _):TimerTask = new TimerTask{def run(){conv()}}
}