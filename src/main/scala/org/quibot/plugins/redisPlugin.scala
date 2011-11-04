package org.quibot.plugins

import org.quibot._
import com.redis._

trait RedisPlugin extends QuiBotPlugin {
	val redis = new RedisClient("localhost", 6379)
}