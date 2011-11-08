package org.quibot.plugins

import org.quibot._
import scalaj.http.Http

trait HttpHelper {
    def http(url: String) = Http(url)

}