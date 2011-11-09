package org.quibot.plugins

import org.quibot._
import org.spartacusse.ircbot.{User, Message, Channel}

case class ReviewPlugin extends QuiBotPlugin with RedisPlugin {
	respondTo("reviews? +show +([^ ]+) *$") { msg =>
		if (msg.groups.size > 0 && msg.groups(0) != "") {
			val branch = msg.groups(0)
			if (isOnReview(branch)) {
				if (isFinished(branch)) 
					say(msg.channel, "According to "+reviewer(branch)+", branch: "+branch+ " is already good to go to master =)")
				branchComments(msg, branch) match {
					case Nil => reply(msg, "No comments yet for branch ["+branch+"] | "+branchObs(branch))
					case Some(l:List[String]) => {
						say(msg.channel, "Branch ["+branch+"] | "+branchObs(branch))
						say(msg.channel, "    Comments:")
						l.foreach( comment => say(msg.channel, "    - " + comment) )
					}	 
				}
			}
		}
	}

	respondTo("reviews? +help *$") { msg =>
		val help = List( "review show [branch]              ----> show info about a branch being reviewed",
						 "review add [branch] [description] ----> request review on a branch. You can  give a brief description",
						 "review del [branch]               ----> delete a review request from the system",
						 "review                            ----> show all review requests status",
						 "review ok [branch]                ----> give your O.K. that the branch is ready to go to master",
						 "review fix [branch]               ----> leave a fix suggestion/order for the given branch")
		say(msg.channel, help)
	}

	respondTo("reviews? add +([^ ]+) +(.*)$") { msg =>
		if (msg.groups.size > 0 && msg.groups(0) != "") {
			val branch = msg.groups(0)
			val obs = if (msg.groups.size > 1) msg.groups(1) else ""
			if (!isOnReview(branch)) {
				addReviewBranch(branch, obs)
				say(msg.channel, "Branch ["+branch+"] added for review! OBS: "+obs) 
			}
			else
				reply(msg, "Branch ["+branch+"] is already for review! You may delete it with 'review del "+branch+"'") 
		}
	}
	respondTo("reviews? +ok +([^ ]+) *$") { msg =>
		if (msg.groups.size > 0 && msg.groups(0) != "") {
			val branch = msg.groups(0)
			okBranch(branch, msg.user.nick.nickname)
			say(msg.channel, "Branch ["+branch+"] is ready for master!")
		}
	}

	respondTo("reviews? *$") { msg =>
		say(msg.channel, allReviewsPretty.getOrElse(List[String]("No review task on the system.")))
	}

	respondTo("reviews? +fix +([^ ]+) +(.*)$") { msg =>
		if (msg.groups.size > 1 && msg.groups(0) != "") {
			val branch = msg.groups(0)
			val comment = msg.groups(1)
			addComment(branch, comment)
			reply(msg, "Comment added to the review of branch ["+branch+"]")
		}
	}

	respondTo("reviews? +del +([^ ]+) *$") { msg =>
		if (msg.groups.size > 0 && msg.groups(0) != "") {
			val branch = msg.groups(0)
			removeBranch(branch)
			say(msg.channel, "Branch ["+branch+"] was removed from review system!")
		}
	}

	def branchKey(branch: String) = "review:"+branch
	def branchCommentsKey(branch: String) = "review:"+branch+":comments"
	def branchOkKey(branch: String) = branchKey(branch)+":ok"

	def isOnReview(branch: String) = redis.get(branchKey(branch)).isDefined
	def isFinished(branch: String) = redis.get(branchOkKey(branch)).isDefined
	def reviewer(branch: String) = redis.get(branchOkKey(branch)).getOrElse("No one")
	def branchObs(branch: String) = redis.get(branchKey(branch)).getOrElse("")

	def allReviewsPretty : Option[List[String]] = redis.llen("reviews") match {
		case None | Some(0) => None
		case Some(n) => redis.lrange("reviews", 0, n) map { list  =>  
			list.flatten map { branch =>
				val numberComments = redis.llen(branchCommentsKey(branch)).getOrElse(0) + " comments on this review"
				if (isFinished(branch)) 
					"DONE! | ["+branch+"] | "+numberComments+" | reviewed-by: "+reviewer(branch) +" | "+branchObs(branch)
				else 
					"TODO! | ["+branch+"] | "+numberComments+" | "+branchObs(branch)
			}
		}
	}

	def allReviews : Option[List[String]] = redis.llen("reviews") match {
		case None | Some(0) => None
		case Some(n) => redis.lrange("reviews", 0, n) map { list  =>  list.flatten }
	}


	def addComment(branch: String, comment: String) = {
		redis.lpush(branchCommentsKey(branch), comment)	
	}

	def addReviewBranch(branch: String, obs: String) = {
		redis.lpush("reviews", branch)
		redis.set(branchKey(branch), obs)
	}
	def okBranch(branch: String, reviewer: String) = redis.set(branchOkKey(branch), reviewer)

	def branchComments(msg: MatchedMessage, branch: String) = {
		redis.llen(branchCommentsKey(branch)) match {
			case None | Some(0) => Nil
			case Some(n) => redis.lrange(branchCommentsKey(branch), 0, n) map ( _ flatten )
		}
	}
	def removeBranch(branch: String) = {
		redis.del(branchKey(branch))
		redis.del(branchCommentsKey(branch))
		redis.del(branchOkKey(branch))
		redis.lrem("reviews", 1, branch)
	}
}