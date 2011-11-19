package org.spartacusse.ircbot

import java.net.Socket
import java.io.{BufferedReader, PrintWriter, InputStreamReader, OutputStreamWriter}
import java.util.concurrent.{Executors, Callable}

import scala.actors._
import scala.actors.Actor._
import org.quibot.Channel
import org.quibot._

//All code in this file is heavily based on https://bitbucket.org/rpelisse/ircboot/overview with minor tweaks

/**
 * this class reads lines from in and generates RawMessage for the actor
 */
case class IrcClientInputReader(actor:Actor, in:BufferedReader) extends Callable[Unit] {

  def readLine():Option[String] = {
    var line:String = in.readLine
    if (line != null) Some(line) else None
  }

  def call():Unit =
    readLine() match {
      case Some(l) => actor ! RawMessage(l) ; call()
      case None    => ()
    }

}

/**
 * IrcClient connects to a server, treats and delivers messages from/to the server
 * see: http://oreilly.com/pub/h/1963
 * see: http://irchelp.org/irchelp/rfc/rfc.html
 */
case class IrcClient(nick:Nick, actor:Actor, host:String, port:Int) extends Actor {

  import IrcClient._

  // hardly-coded name for the client
  val name = "SpartaBot"

  // open a socket to the server and then open the in and out streams
  val socket = new Socket(host, port)
  val in = new BufferedReader(new InputStreamReader(socket.getInputStream()))
  val out = new PrintWriter(new OutputStreamWriter(socket.getOutputStream()))

  // launch a thread that listens from the in stream using an IrcClientInputReader
  val executorService = Executors.newSingleThreadExecutor()
  executorService.submit(IrcClientInputReader(this, in))

  // initialize the dialog with the server
  sendMessage("NICK " + nick.nickname)
  sendMessage("USER " + nick.nickname + " 8 *  : " + name)

  /**
   * send a raw message to the IRC server
   */
  def sendMessage(messages:String*):Unit = sendMessage(messages.toList)

  def sendMessage(messages:List[String]):Unit = {
    for (m <- messages) out.println(m)
    out.flush()
  }

  /**
   * this act() method from Actor loops and reacts on RawMessages and IrcCommands
   * it tries to parse/specialize and then emits Messages
   * it serializes IrcCommands and sends them to the IRC server
   */
  final def act() =
    loop {
      react {
   // TODO defines a regex to keep consistency
   case RawMessage(m) if m startsWith ("ERROR :Closing Link") => {
     actor ! Disconnected()
     exit
   }
   // TODO do we want to forward this PING event to the bot?
   case RawMessage(m) if m startsWith ("PING :") => {
     val pingMessage = m substring 6
     sendMessage("PONG :" + pingMessage)
   }
   // :irc.w3.org 353 spartabot = #spartacusse :spartabot johann betehess
   case RawMessage(m) if (JOINMSG findFirstMatchIn m).isDefined => {
     val hit = (JOINMSG findFirstMatchIn m).get
     // TODO use regex here
     val chunks = (m split " ").toList
     val channel = Channel(hit.group(3))
     val participants = (chunks drop 6) map (Nick(_))
     actor ! Participant(channel, participants)
   }
   // :betehess!bertails@66.31.43.2 PRIVMSG #spartacusse :salut sparta
   // TODO try to optimize to match only once
   case RawMessage(m) if (PRIVMSG findFirstMatchIn m).isDefined => {
     val hit = (PRIVMSG findFirstMatchIn m).get
     val sender = User(Nick(hit.group(1)), Name(hit.group(2)), Host(hit.group(3)))
     val channel = Channel(hit.group(4))
     val message = hit.group(5)
     actor ! Message(channel, sender, message)
   }
        // :johann!johann@98.239.3.24 INVITE test :#spartacusse
   case RawMessage(m) if (INVITEMSG findFirstMatchIn m).isDefined => {
     val hit = (INVITEMSG findFirstMatchIn m).get
     val sender = User(Nick(hit.group(1)), Name(hit.group(2)), Host(hit.group(3)))
     val nick = Nick(hit.group(4))
     val channel = Channel(hit.group(5))
     actor ! Invitation(sender, nick, channel)
   }
   case RawMessage(m) if (PARTMSG findFirstMatchIn m).isDefined => {
     val hit = (PARTMSG findFirstMatchIn m).get
     val user = User(Nick(hit.group(1)), Name(hit.group(2)), Host(hit.group(3)))
     val channel = Channel(hit.group(4))
     val reason = hit.group(5)
     actor ! PartMsg(user, channel, reason)
   }
        case RawMessage(m) if (CONNUPMSG findFirstMatchIn m).isDefined => {
     val hit = (CONNUPMSG findFirstMatchIn m).get
     val host = hit.group(1)
     val nick = Nick(hit.group(2))
     val message = hit.group(3)
     actor ! ConnUpMsg(host, nick, message)
   }
        case RawMessage(m) if (KICKMSG findFirstMatchIn m).isDefined => {
     val hit = (KICKMSG findFirstMatchIn m).get
     val sender = User(Nick(hit.group(1)), Name(hit.group(2)), Host(hit.group(3)))
     val nick = Nick(hit.group(5))
     val channel = Channel(hit.group(4))
     val reason = hit.group(6)
     actor ! KickMsg(sender, channel, nick, reason)
   }
   case Quit(reason) => sendMessage("QUIT :" + reason)
   case Join(channel) => sendMessage("JOIN " + channel.name)
   case Part(channel,reason) => sendMessage("PART " + channel.name + " :" + reason)
   case Say(channel, messages) => {
     val oneMessage = messages.toList map ("PRIVMSG " + channel.name + " :" + _)
     sendMessage(oneMessage)
   }        
   case Invite(nick,channel) => sendMessage("INVITE " + nick.nickname + " " + channel.name)
        case Kick(nick, channel, reason) => sendMessage("KICK "+ channel.name + " " + nick.nickname + " :" + reason)
      }
   }
}

/**
 * Companion object for IrcClient
 * defines a set of constants
 */
object IrcClient {

  // :betehess!bertails@66.31.43.2 PRIVMSG #spartacusse :salut sparta
  final val PRIVMSG = """^:(\w+)!(.+?)@([\d\.]+) PRIVMSG ([\w#&-_]+) :(.*)$""".r
  // :irc.w3.org 353 sparta-test = #spartacusse :sparta-test johann @betehess
  final val JOINMSG = """^:([^ ]+) 353 ([^ ]+) = ([\w#&-_]+) :(.*)$""".r
  // :johann!johann@98.239.3.24 INVITE test :#spartacusse
  final val INVITEMSG = """^:(\w+)!(.+?)@([\d\.]+) INVITE ([^ ]+) :(.*)$""".r
  // :test!test@98.239.3.24 PART #spartacusse
  final val PARTMSG = """^:(\w+)!(.+?)@([\d\.]+) PART ([\w#&-_]+) :(.*)$""".r
  // :irc.w3.org 001 johann :Welcome to the W3C IRC Network johann
  final val CONNUPMSG = """^:([^ ]+) 001 ([^ ]+) :(.*)$""".r
  // :johann!johann@98.239.3.24 KICK #sico test :va t en
  final val KICKMSG = """^:(\w+)!(.+?)@([\d\.]+) KICK ([^ ]+) ([^ ]+) :(.*)$""".r
}

/**
 * IrcBot implements a skeleton for an IRC bot
 * a real bot must extends this class
 * this is an Actor where act() is already defined
 * and reacts on IrcMessages regarding reactOnIrcMessage()
 */
class IrcQuiBot(host: String, port: Int, nick: Nick, joinedChannels: Channel*) 
  extends QuiBot(nick, joinedChannels:_*) {

  val ircClient = IrcClient(nick, this.asInstanceOf[Actor], host, port)

  final override def act() =
    loop {
      react {
         case m:ChatMessage => reactOnIrcMessage(m)
         case m:ChatCommand => ircClient forward m
      }
    }

  /**
   * redefines start() to also start the inner IrcClient
   */
  override def start():Actor = {
    ircClient.start()
    startActor()
  }

  def startActor():Actor = {
    super.start()
  }

  /**
   * redefines exit() to also exit the inner IrcClient
   */
  final override def exit():Nothing = {
    //ircClient.exit() //TODO: FIND WAY TO STOP THIS
    super.exit()
  }

}