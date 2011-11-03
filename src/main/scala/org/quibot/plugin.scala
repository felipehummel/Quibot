package org.quibot.plugins

import org.quibot._
import org.spartacusse.ircbot._

trait QuiBotPlugin {
	val commands = new collection.mutable.ArrayBuffer[Command]
	private var bot: Option[QuiBot] = None //injected later
	
	def setBot(bot: QuiBot)	= this.bot = Some(bot)

	def respondTo(regexStr: String)(f: MatchedMessage => Unit) {
        val regex = ("(?i)"+regexStr).r //adding case insensitive
        commands += Command(regex)(f)  
    }

    def say(channel: Channel, msgs: String*) = bot map { quibot => quibot say (channel, msgs:_*) }
    def sayTo(channel: Channel, userNick: String, msgs: String*) = bot map { quibot =>  quibot sayTo(channel, userNick, msgs:_*) }
    def sayAllChannels(msgs: String*) = bot map { quibot => 
	    quibot.joinedChannels.foreach { c => quibot say(c, msgs:_*) } 
	}
}