object FeedOptimizer {
  case class StoryEvent(id: Int, time: Int, score: Int, height: Int)
  case class Feed(totalScore: Int = 0, stories: Vector[Int] = Vector[Int]())

  def main(args: Array[String]): Unit = {
    val inputIter = io.Source.fromFile("other/input.txt").getLines()

    val n +: window +: maxHeight +: _ = inputIter.next().split(" ").map(_.toInt).toIndexedSeq

    var storyEvents = Vector[StoryEvent]()
    var id = 0

    for (ln <- inputIter.take(n)) {
      ln.split(" ") match {
        case Array("S", time, score, height) =>
          id += 1
          storyEvents = storyEvents :+ StoryEvent(id, time.toInt, score.toInt, height.toInt)

        case Array("R", time) =>
          val cutoffTime = time.toInt - window
          storyEvents = storyEvents.filter(s => s.time >= cutoffTime)

          val feed = optimalStoryFeed(storyEvents, maxHeight)

          if (feed.stories.nonEmpty) {
            println(s"${feed.totalScore} ${feed.stories.length} ${feed.stories.mkString(" ")}")
          }
          else {
            println(s"${feed.totalScore} ${feed.stories.length}")
          }
      }
    }
  }

  def optimalStoryFeed(storyEvents: Vector[StoryEvent], maxHeight: Int): Feed = {
    val feeds = Array.fill(storyEvents.length + 1)(Array.fill(maxHeight + 1)(Feed()))

    for (i <- storyEvents.indices) {
      for (j <- 0 to maxHeight) {
        val storyEvent = storyEvents(i)
        val previousFeedIfStoryIsIgnored = feeds(i)(j)

        if (j >= storyEvent.height) {
          val previousFeedIfStoryIsTaken = feeds(i)(j - storyEvent.height)

          if (storyEvent.score + previousFeedIfStoryIsTaken.totalScore == previousFeedIfStoryIsIgnored.totalScore) {
            if (previousFeedIfStoryIsTaken.stories.length + 1 < previousFeedIfStoryIsIgnored.stories.length) {
              feeds(i + 1)(j) = Feed(
                storyEvent.score + previousFeedIfStoryIsTaken.totalScore,
                previousFeedIfStoryIsTaken.stories :+ storyEvent.id
              )
            }
            else {
              feeds(i + 1)(j) = previousFeedIfStoryIsIgnored
            }
          }
          else if (storyEvent.score + previousFeedIfStoryIsTaken.totalScore > previousFeedIfStoryIsIgnored.totalScore) {
            feeds(i + 1)(j) = Feed(
              storyEvent.score + previousFeedIfStoryIsTaken.totalScore,
              previousFeedIfStoryIsTaken.stories :+ storyEvent.id
            )
          }
          else {
            feeds(i + 1)(j) = previousFeedIfStoryIsIgnored
          }
        }
        else {
          feeds(i + 1)(j) = previousFeedIfStoryIsIgnored
        }
      }
    }

    feeds.last.last
  }
}
