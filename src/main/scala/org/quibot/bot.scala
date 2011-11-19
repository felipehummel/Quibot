package org.quibot

import java.net.Socket
import java.util.concurrent.{Executors, Callable}
import java.io.File

import scala.util.matching.Regex

import org.spartacusse.ircbot._
import org.quibot.plugins._

object Bot {
   def main(args: Array[String]): Unit = {
    try {
        val properties = new java.util.Properties();
        properties.load(new java.io.FileInputStream(args(0)))
        val nick = properties.getProperty("nick")
        val ircServer = properties.getProperty("ircServer")
        val port = properties.getProperty("port").trim.toInt
        val channels = properties.getProperty("channels") split "," map ( c => Channel(c.trim) )
        val testing = args.length > 1 && args(1) == "--testing"
        val bot: QuiBot = if (testing)
                            new MockQuiBot("test.data", nick, channels:_*)
                          else
                            new IrcQuiBot(ircServer, port, nick, channels :_*)
        bot use ( GitPlugin(properties.getProperty("gitRepositoryDir")),
                  SwearingPlugin(),
                  WallPlugin(),
                  ReviewPlugin(),
                  VideoPlugin(),
                  BasicPlugin() )
        bot.start
        println("Starting Bot!")

        System.out.println("----- Interactive QuiBot -----\n!Enter a line of text to talk to channel: "+channels(0));
        Iterator.continually(Console.readLine).takeWhile(_ != "").foreach(line => bot say (channels(0), line))
     } catch {
       case e: Exception => e.printStackTrace
     }
   }
}
