package wormhole

import java.util.TimerTask

object RunnableConversion {
	implicit def methodToTimerTask(conv:()=> _):TimerTask = new TimerTask{def run(){conv()}}
}