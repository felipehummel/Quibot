package org.quibot

import java.io.File
import java.io.{BufferedReader, PrintWriter, InputStreamReader, OutputStreamWriter}

trait CLICommands {
    var currDir : Option[File] = None
    val runTime = Runtime.getRuntime

    /**
    * from: http://code.google.com/p/scala-utilities/source/browse/trunk/helper.scala
    */
    def exec (cmd : String) : (Int, List[String]) = {
        val process = if (currDir.isDefined) runTime.exec (cmd, null, currDir.getOrElse(new File("."))) else runTime.exec(cmd)
        val resultBuffer = new BufferedReader(new InputStreamReader(process.getInputStream))
        var line : String = null
        var lineList : List[String] = Nil

        do {
            line = resultBuffer.readLine
            if (line != null) {
                lineList = line :: lineList
            }
        } while (line != null)

        process.waitFor
        resultBuffer.close

        (process.exitValue, lineList.reverse)
    }
}