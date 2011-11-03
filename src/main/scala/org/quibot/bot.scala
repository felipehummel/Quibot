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

case class QuiBot(nick:Nick, host:String, port:Int, options: collection.mutable.Map[String, String], joinChannels: Channel*) extends IrcBot {

    joinChannels foreach { channel => this ! Join(channel) }
    val commands = new collection.mutable.ArrayBuffer[Command]

    respondTo("ping *$") { msg =>
        sayTo(msg.channel, msg.user.nick.nickname, "PONG!")
    }

    def removeNickFromMessage(msg: Message, nick: Nick) = {
        msg.copy( message = msg.message.substring(nick.nickname.length+1).trim )
    }

    def reactOnIrcMessage(ircMessage:IrcMessage) =
        ircMessage match {
            case d @ Disconnected()                     => exit()
            case p @ Participant(channel, participants) => {
                if (joinChannels contains channel) {
                   println("Joined channel " + channel + "; participants are: " + (participants mkString " "))   
                }
            }
            case m @ Message(channel, user, message) => {
                 if (message startsWith nick.nickname) {
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

    def respondTo(regexStr: String)(f: MatchedMessage => Unit) {
        val regex = ("(?i)"+regexStr).r //adding case insensitive
        commands += Command(regex)(f)  
    }
    def say(channel: Channel, msgs: String*) {
        this ! Say(channel, msgs toList)
    }
    def sayTo(channel: Channel, userNick: String, msgs: String*) {
        this ! Say(channel, msgs map ( msg => userNick + ": " + msg ) toList)
    }

}

object Bot {
   def main(args: Array[String]): Unit = {
        val options = collection.mutable.Map( "gitRepo" -> "/home/felipe/workspace/quati/quati")
        val bot1 = new QuiBot("gerenteDoBanco", "10.60.1.22", 6667, options, "#teste2") 
                        with SwearingCommands with GitCommands
        bot1.start

        println("Starting Bot!")
   }
}