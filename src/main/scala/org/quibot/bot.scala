package org.quibot

import java.net.Socket
import java.util.concurrent.{Executors, Callable}
import java.io.File

import scala.util.matching.Regex

import org.spartacusse.ircbot._
import org.quibot.plugins._

case class Command(val regex: Regex)(command: MatchedMessage => Unit) {
    def matchesMsg(msg: String) = !regex.findAllIn(msg).isEmpty
    def execute(msg: Message) = command(MatchedMessage(msg,regex))
}

case class MatchedMessage(msg: Message, regex: Regex) {
    val groups = regex.unapplySeq(msg.message).getOrElse(List())
    def text = msg.message
    def channel = msg.channel
    def user = msg.user
}

case class QuiBot(nick:Nick, host:String, port:Int, joinedChannels: Channel*) extends IrcBot {
    joinedChannels foreach { channel => this ! Join(channel) }
    val commands = new collection.mutable.ArrayBuffer[Command]

    def removeNickFromMessage(msg: Message, nick: Nick) = {
        msg.copy( message = msg.message.trim.substring(nick.nickname.length+1).trim )
    }

    def reactOnIrcMessage(ircMessage:IrcMessage) =
        ircMessage match {
            case d @ Disconnected()                     => exit()
            case p @ Participant(channel, participants) => {
               println("Joined channel " + channel + "; participants are: " + (participants mkString " "))   
            }
            case m @ Message(channel, user, message) => {
                 if (message.trim startsWith nick.nickname) {
                    commands filter ( _ matchesMsg message ) foreach { _ execute(removeNickFromMessage(m, nick)) }
                }
             }
            case i @ Invitation(sender, nick, channel)    => ()
            case de @ PartMsg(user, channel, reason)     => ()
            case cm @ ConnUpMsg(host, nick, message)     => {
               if (nick == this.nick) 
                   println("Joined server " + host +"; msg : "+message)
            }
            case km @ KickMsg(user, channel, nick, message)     =>()
    }

    def say(channel: Channel, msgs: String*) {
        this ! Say(channel, msgs toList)
    }
    def sayTo(channel: Channel, userNick: String, msgs: String*) {
        this ! Say(channel, msgs map ( msg => userNick + ": " + msg ) toList)
    }

    def use (plugins: QuiBotPlugin*) {
        for( plugin <- plugins) {
            this.commands appendAll plugin.commands 
            plugin setBot this
        }
    }
}

object Bot {
   def main(args: Array[String]): Unit = {
        val properties = new java.util.Properties();
        properties.load(new java.io.FileInputStream(args(0)))
        val nick = properties.getProperty("nick")
        val ircServer = properties.getProperty("ircServer")
        val port = properties.getProperty("port").toInt
        val channels = properties.getProperty("channels") split "," map ( c => Channel(c.trim) )
        val bot = new QuiBot(nick, ircServer, port, channels :_*)
        bot use ( GitPlugin(properties.getProperty("gitRepositoryDir")),
                  SwearingPlugin(),
                  WallPlugin(),
                  BasicPlugin() )
        bot.start
        println("Starting Bot!")

        System.out.println("----- Interactive QuiBot -----\n!Enter a line of text to talk to channel: "+channels(0));
        Iterator.continually(Console.readLine).takeWhile(_ != "").foreach(line => bot say (channels(0), line))

   }
}