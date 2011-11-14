package org.quibot

import java.io.File
import java.io.{BufferedReader, PrintWriter, InputStreamReader, OutputStreamWriter}
import scalaj.http.Http

trait CLICommands {
    var currDir : Option[File] = None
    val runTime = Runtime.getRuntime

    /**
    * originally based on: http://code.google.com/p/scala-utilities/source/browse/trunk/helper.scala
    */
    def exec (cmd : String) : (Int, List[String]) = {
        val builder = new ProcessBuilder(cmd.split(" "):_*)
        builder.directory(currDir.getOrElse(new File(".")))
        builder.redirectErrorStream(true)
        val process = builder.start()
        val resultBuffer = new BufferedReader(new InputStreamReader(process.getInputStream))
        var line : String = null
        var lineList : List[String] = Nil
        do {
            line = resultBuffer.readLine
            if (line != null) 
                lineList = line :: lineList
        } while (line != null)
        process.waitFor
        resultBuffer.close
        (process.exitValue, lineList.reverse)
    }
}

trait Randomizer {
    val rand = new scala.util.Random
    def randomIterator[T](msgs: IndexedSeq[T]) : Iterator[T] = Iterator.continually ( randomize(msgs) )
    def randomize[T](msgs: IndexedSeq[T]) : T = msgs((rand.nextFloat * msgs.size).toInt)
}

trait HttpHelper {
    def http(url: String) = Http(url)
}