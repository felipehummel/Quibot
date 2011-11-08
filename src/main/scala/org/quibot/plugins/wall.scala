package org.quibot.plugins

import org.quibot._
import org.spartacusse.ircbot.{User, Message, Channel}

case class WallPlugin extends QuiBotPlugin with RedisPlugin with MessageRandomizer {
	private val clearMessages = Array("Everything is clean!", "All white and clear now", "What the hell you just did!?!!? What if someone didn't read one of the messages!?! :D", "I'm not cleaning that up! ... ... ... ... ... ... Ok, just did.", "Clear yourself! Nevermind, I'll do it!", "Look at this filthy floor. I'm on it.")

	respondTo("wall +add +(.*)") { msg =>
		if (msg.groups.size > 0 && msg.groups(0) != "")
			addMessage(msg.groups(0), msg, msg.user)
		else 
			reply(msg, "Empty Message?")
	}

	respondTo("wall +clear$") { msg =>
		clearWall(msg.channel)
		reply(msg, randomize(clearMessages))
	}

	respondTo("wall( show| list| all)? *$") { msg => wall(msg) }
	respondTo("wall +del(ete)? *$") { msg => deleteLast(msg) }

	def addMessage (wallMessage: String, msg: MatchedMessage, by: User) = {
		redis.lpush(wallKey(msg.channel), wallMessage)
		reply(msg, "Entry added: '"+wallMessage+"'")
	}

	def clearWall(channel: Channel) = redis.del(wallKey(channel))		

	def deleteLast(msg: MatchedMessage) = {
		redis.rpop(wallKey(msg.channel)) match {
			case Some(m) => reply(msg, "Entry deleted: '"+m+"'")
			case None => reply(msg, "Nothing to delete here!")
		}
	}

	def wallKey(channel: Channel) = "wall:"+ channel.name

	def wall(msg: MatchedMessage) = {
		redis.llen(wallKey(msg.channel)) match {
			case Some(0) => reply(msg, "Nothing on the wall =(")
			case Some(n) => {
				val wallList = redis.lrange(wallKey(msg.channel), 0, n)
				wallList map { list =>
					say (msg.channel, " ------- THE WALL ("+msg.channel.name+") ------- ")
					for ( i <- 0 until list.size) {
						say (msg.channel, (i + 1) + " - " + list(i).getOrElse(""))
					}
					say (msg.channel, " ------------------------------------------------ ")
				}
			}
			case None => reply(msg, "Nothing on the wall =(")
		}
	}
}