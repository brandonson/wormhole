package wormhole

import wormhole.lobby.WormholeLobbyClient
import java.net.Socket
import javax.swing.UIManager
import javax.swing.JOptionPane
import java.io.IOException
import wormhole.lobby.WormholeMainClient
/**
 * Main class for clients.
 */
object Wormhole {

	def main(args:Array[String]){
		UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName())
		//Get server name/ip address
		val in = JOptionPane.showInputDialog(null, "Server:", "Wormhole Client", JOptionPane.PLAIN_MESSAGE)
		if(in!=null){
			try{
				//try and connect
				val data = new SocketInfoData(new Socket(in, CONNECTION_PORT))
				clientConnection = data.out
				new Thread(new WormholeMainClient(data)).start()
			}catch{
				case _:IOException =>
					//if it fails, tell the user, then exit (user must restart the program themselves.)
					JOptionPane.showMessageDialog(null, "Failed to connect.", "Wormhole Client", JOptionPane.PLAIN_MESSAGE)
					System.exit(0)
			}
		}
	}
}
