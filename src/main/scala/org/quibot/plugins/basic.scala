package org.quibot.plugins

import org.quibot._

case class BasicPlugin extends QuiBotPlugin {
    respondTo("ping *$") { msg =>
        sayTo(msg.channel, msg.user.nick.nickname, "PONG!")
    }
}