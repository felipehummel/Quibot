package org.quibot

import scala.util.matching.Regex

import org.spartacusse.ircbot._
import org.quibot.plugins._

import scala.actors._
import scala.actors.Actor._

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

case class RawMessage(message:String)

sealed abstract class ChatMessage
case class Message(channel:Channel, user:User, message:String)   extends ChatMessage
case class Disconnected()                                        extends ChatMessage
case class Participant(channel:Channel, participants:List[Nick]) extends ChatMessage
case class Invitation(sender:User, nick:Nick, channel:Channel)   extends ChatMessage
case class PartMsg(user:User, channel:Channel, reason:String)     extends ChatMessage
case class ConnUpMsg(host:String, nick:Nick, message:String)       extends ChatMessage
case class KickMsg(user:User, channel:Channel, nick:Nick, reason:String) extends ChatMessage


sealed abstract class ChatCommand
case class Quit(reason:String)                   extends ChatCommand
case class Join(channel:Channel)                 extends ChatCommand
case class Say(channel:Channel, messages:List[String]) extends ChatCommand
object Say {
  def apply(channel:Channel, messages:String*):Say = Say(channel, messages.toList)
}
case class Invite(nick:Nick, channel:Channel)    extends ChatCommand
case class Part(channel:Channel, reason:String)  extends ChatCommand
case class Kick(nick:Nick, channel:Channel, reason:String) extends ChatCommand

/** value objects */

case class Channel(name:String) {
  override def toString = name.toString
}
object Channel {
  implicit def channelFromString(channelName:String):Channel = Channel(channelName)
}

case class Nick(nickname:String) {
  override def toString = nickname.toString
}
object Nick {
  implicit def nickFromString(nickname:String):Nick = Nick(nickname)
}
case class Name(name:String)
case class Host(hostname:String)
case class User(nick:Nick, name:Name, host:Host) {
  override def toString = "["+nick.nickname+"|"+name.name+"|"+host.hostname+"]"
}

abstract class QuiBot(val nick: Nick, val joinedChannels: Channel*) 
  extends Actor {
  joinedChannels foreach { channel => this ! Join(channel) }
  val commands = new collection.mutable.ArrayBuffer[Command]

  def use (plugins: QuiBotPlugin*) {
      for( plugin <- plugins) {
          this.commands appendAll plugin.commands 
          plugin setBot this
      }
  }

  def removeNickFromMessage(msg: Message) = {
    msg.copy( message = msg.message.trim.substring(nick.nickname.length+1).trim )
  }

  def reactOnIrcMessage(ircMessage:ChatMessage) =
    ircMessage match {
          case d @ Disconnected()                     => exit()
          case p @ Participant(channel, participants) => {
             println("Joined channel " + channel + "; participants are: " + (participants mkString " "))   
          }
          case m @ Message(channel, user, message) => {
               if (message.trim startsWith nick.nickname) {
                  val msg = removeNickFromMessage(m)
                  commands filter ( _ matchesMsg msg.message ) foreach { _ execute(msg) }
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

    final def sayTo(channel: Channel, userNick: String, msgs: String*) {
        say(channel, msgs map ( msg => userNick + ": " + msg ):_*)
    }

    def act() =
      loop {
        react {
           case m:ChatMessage => reactOnIrcMessage(m)
        }
      }
}

class MockQuiBot(testFile: String, nick:Nick, joinedChannels: Channel*) 
  extends QuiBot(nick, joinedChannels:_*) {

    override def start = {
        super.start()
        val fakeUser = User(Nick("fakeNick"), Name("fakeUser"), Host("fakeHost"))
        io.Source.fromFile(testFile).getLines.foreach(line => this ! Message(joinedChannels(0), fakeUser, line))
        this
    }

    override def say(channel: Channel, msgs: String*) {
        msgs foreach ( msg => println("bot: "+msg) )
    }
}