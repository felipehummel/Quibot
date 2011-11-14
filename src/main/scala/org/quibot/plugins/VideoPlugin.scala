package org.quibot.plugins

import org.quibot._
import org.spartacusse.ircbot.{User, Message, Channel}
import scalaj.http.HttpOptions
import scala.xml._

case class VideoPlugin extends QuiBotPlugin with HttpHelper with Randomizer {
	val orderBy = randomIterator( Array("relevance", "published", "viewCount", "rating") )
	respondTo("random +video +(.+)$") { msg =>
		val q = msg.groups(0)
		println("[INFO] random video | query: "+q)
		val result = http("http://gdata.youtube.com/feeds/api/videos")
						.params("vq" -> q,
								"orderBy" -> orderBy.next,
								"start-index" -> "1",
								"max-results" -> "50")
						.options(HttpOptions.readTimeout(10000))
						.asXml
		val entries = result \ "entry" toArray
		val chosenEntry = randomize(entries)
		val title = (chosenEntry \ "title").first.text
		val href = (((chosenEntry \ "link").first) \ "@href").text
		say(msg.channel, title+" => "+href.replace("&feature=youtube_gdata", ""))
	}

	respondTo("video +(.+)$") { msg =>
		val q = msg.groups(0)
		println("[INFO] video | query: "+q)
		val result = http("http://gdata.youtube.com/feeds/api/videos")
						.params("vq" -> q,
								"orderBy" -> "relevance",
								"start-index" -> "1",
								"max-results" -> "5")
						.options(HttpOptions.readTimeout(10000))
						.asXml
		val entries = result \ "entry"
		entries foreach { entry =>
			val title = (entry \ "title").first.text
			val href = (((entry \ "link").first) \ "@href").text
			say(msg.channel, title+" => "+href.replace("&feature=youtube_gdata", ""))
		}
	}
}
