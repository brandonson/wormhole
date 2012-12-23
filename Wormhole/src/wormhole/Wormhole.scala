package wormhole

import wormhole.lobby.WormholeLobbyClient
import java.net.Socket
import javax.swing.UIManager
import javax.swing.JOptionPane
import java.io.IOException
import wormhole.lobby.WormholeMainClient
import org.slf4j.LoggerFactory
/**
 * Main class for clients.
 */
object Wormhole {

	val log = LoggerFactory.getLogger("Wormhole Main")
	
	def main(args:Array[String]){
		UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName())
		//Get server name/ip address
		val in = JOptionPane.showInputDialog(null, "Server:", "Wormhole Client", JOptionPane.PLAIN_MESSAGE)
		if(in!=null){
			try{
				log.info("Connecting to " + in)
				//try and connect
				val data = new SocketInfoData(new Socket(in, CONNECTION_PORT))
				clientConnection = data.out
				log.debug("Connection established")
				new Thread(new WormholeMainClient(data)).start()
			}catch{
				case err:IOException =>
					log.debug("Error connecting to " + in, err)
					//if it fails, tell the user, then exit (user must restart the program themselves.)
					JOptionPane.showMessageDialog(null, "Failed to connect.", "Wormhole Client", JOptionPane.PLAIN_MESSAGE)
					System.exit(0)
			}
		}else{
			log.trace("No server given, exiting")
		}
	}
}
