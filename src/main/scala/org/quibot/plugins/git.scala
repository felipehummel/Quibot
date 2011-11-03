package org.quibot.plugins

import org.quibot._
import java.io.File

trait GitCommands extends QuiBot with CLICommands {
    currDir = Some(new File(options.getOrElse("gitRepo", ".")))

    val timer = new java.util.Timer()
    timer schedule ( new java.util.TimerTask {
        override def run() = fetchGit
    }, 10000, 300000) // 5 min
    
    respondTo("git +log *(.*)$") { msg =>
        val branch = if (msg.groups.size > 0 && msg.groups(0) != "") msg.groups(0) else "origin/master"
        println(msg.groups+ " :: "+msg.groups.size + " :: "+branch)
        println("git log --oneline -n 5 "+branch)
        val (_, content) = exec("git log --oneline -n 5 "+branch)
        say(msg.channel, "[branch "+branch+"]")
        say(msg.channel, content map ( commit => "    " + commit ) toArray :_*)
    }

    def getCommitMessages(lowerCommit: String, upperCommit: String, branch: String, commitLimit: Int = 10) = {
        val (_, lines) = exec("git log "+lowerCommit+"..."+upperCommit+" --oneline")
        if (lines.size > commitLimit) 
            (lines.take(commitLimit)) ::: List("... (more commits)")
        else
            lines
    }
    def truncateCommitMsg(msg: String, limit: Int = 100) = if (msg.length > limit) msg.take(100) + "..."  else msg

    def fetchGit = {
        // val lines = exec("git fetch")
        val lines = """ap okepokaopka
        aepokaopkaeok
        aekoaeopkae
 + b8ef533...4326864 wip-anselmo -> origin/wip-anselmo  (forced update)
   97a0c882909e..05db5143730bf  wip-lg     -> origin/wip-lg
 * [new branch]      wip-lg-credipre -> origin/wip-lg-credipre
 + 89f2ab1...02875b5 wip-rodrigo_data -> origin/wip-rodrigo_data  (forced update)
 * [new tag]         0.11       -> 0.11
 aeoijeaiojaeoijaejae
 aeiopjeioajaeiojioaejoaeij""" split "\n"
        val regex = """(([a-z0-9]+)\.\.([a-z0-9]+))? +([^ ]+) +-> ([^ ]+).*$""".r
        var messages = List[String]()
        if (lines.size != 0) {
            for (line <- lines) {
                val matchIterator = regex findAllIn line
                val subgroups = matchIterator.matchData.flatMap( m => m.subgroups).toList
                if (subgroups.size > 0) {
                    line(1) match {
                        case ' ' => {
                            val commits = getCommitMessages(subgroups(1), subgroups(2), subgroups(4))
                            messages :::= commits map (commit => "         "+truncateCommitMsg(commit))
                            messages ::= ":::: [new commits] " + subgroups(4)
                        }
                        case '*' => {
                            if (line contains "[new tag]")
                                messages ::= "**** [new tag] "+subgroups(4)
                            else
                                messages ::= "**** [new branch] "+subgroups(4)
                        }
                        case '+' => messages ::= "++++ [forced update] "+subgroups(4)
                        case _ => messages ::= line
                    }
                }
            }
            messages ::= "-----------!! Repository updates !!-----------"
            for (channel <- joinChannels) {
                say(channel, messages:_*)
            }
        }
    }
}