package org.quibot.plugins

import org.quibot._

case class SwearingPlugin extends QuiBotPlugin {
    respondTo("que time é teu\\? *$") { msg =>
        sayTo(msg.channel, msg.user.nick.nickname, "bateu na trave e entrou no teu")
    }    
    respondTo("(foda|caralho|fuder|fuck| cu)( +|$)") { msg =>
        sayTo(msg.channel, msg.user.nick.nickname, "manere seu linguajar robin, você ainda é um menino!")
    }
    respondTo("porra+ *$") { msg =>
        sayTo(msg.channel, msg.user.nick.nickname, "porra é o cu da cachorra")
    }
    respondTo("cachorra morreu teu cu (é|eh|e) meu *$") { msg =>
        sayTo(msg.channel, msg.user.nick.nickname, "ahhh meu cu é seu? Estás insinuando que sou viado?")
    }
    respondTo("sim,? tu [ée]s viado!? *$") { msg =>
        sayTo(msg.channel, msg.user.nick.nickname, "viado é rolimã, como tu e tua irmã")
    }
}