package org.quibot.plugins

import org.quibot._

case class SwearingPlugin extends QuiBotPlugin with Randomizer {
    val swearingMessages = randomIterator( Array("manere seu linguajar robin, você ainda é um menino!", "teu cu","vai se fuderem", "VOCES NAUM MERECEM FALAR COM MEU BEBE", "seu madeerfãquer! seila como escreve", "pega essa porra desse arquivo e enfia no cu",
        "vou zerar tua conta seu fdp", "Seu saldo é de R$ 37,23 ... ... ... ... POBRE!", "pega na minha e balança", "vai tomar bem no meio da berada do seu cu", "I like turtles", "vou comer o robin hoje", "vou comer a tia do batimaaaa", "teu cu é meu", "e a mãe vai bem?") )

    respondTo("que time é teu\\? *$") { msg =>
        reply(msg, "bateu na trave e entrou no teu")
    }    
    respondTo("(cuzao|vai pra merda|vai se fuder|te fode|vai tomar no cu|teu cu|chupa|gay|baitola|bixa|foda|caralho|fuder|fuck| cu|bosta)( +|$)") { msg =>
        reply(msg, swearingMessages.next)
    }
    respondTo("porra+ *$") { msg =>
        reply(msg, "porra é o cu da cachorra")
    }
    respondTo("cachorra morreu teu cu (é|eh|e) meu *$") { msg =>
        reply(msg, "ahhh meu cu é seu? Estás insinuando que sou viado?")
    }
    respondTo("sim,? tu [ée]s viado!? *$") { msg =>
        reply(msg, "viado é rolimã, como tu e tua irmã")
    }
    respondTo("viado *$") { msg =>
        reply(msg, "viado é rolimã, como tu e tua irmã")
    }
    respondTo(" *d[ea] onde vo?c[eê]? tirou is[st]o? *$") { msg =>
        reply(msg, "do cu é claro robin! De onde mais seria?")
    }
}