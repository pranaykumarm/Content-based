

package com.caspida.algorithms.generic.cbrecommendations


import com.caspida.algorithms.generic.clusters.AlgorithmFlow
import com.caspida.algorithms.security.clusters.CounterModel
import org.slf4j.{Logger, LoggerFactory}

import scala.util.parsing.json.JSONObject


class CBRecommendationsFlow (filter : String, training_count : Int, window : Int, options : Int)
  extends AlgorithmFlow (filter : String, training_count : Int, window : Int, options : Int) with Serializable {

  val modelName = "cbFiltering"

  var aggressive = if (options == 1) true else false
  aggressive = false

  logger.info("aggression: {}", aggressive)

  logger.info("instantiating CB")
  val CB = new CBReco()

  var scores = Array[Float](0)

  val build_win: Int = 30000
  val bins: Int = 1000
  val use_bins: Int = 10

  var thr: Float = 0

  override def compute(line: String, counter: Int, training: Boolean): String = {

    if (line == "") return ("")
    line
  }


  override def check(line: String, counter: Int, training: Boolean): String = {

    var returnVal: String = ""

    if (line == "") return line

    val json = CounterModel.getMyJSON(line)

    val featureMap = scala.collection.JavaConversions.mapAsScalaMap(json)
    val stream = json.get("stream")
    val user = json.get("valueA")
    val date = json.get("date")
    val F: Int = 3

    //logger.info("calling score when counter {}",counter)
    val f = CB.score(user, featureMap, F)
    //logger.info("score_fac {} counter {}", f, counter)

    if (counter % build_win == 0) {

      logger.info("calling breaks when counter {}", counter)
      thr = CB.breaks(scores, bins, use_bins)

      scores = new Array[Float](0)
    }

    if (f == -1){

      if (aggressive){
        logger.info("New event at counter {}", counter)
        //returnVal = "user "+A+ " accessing machine "+B+" is anomalous"

        val outMap = Map(
          "date" -> date,
          "stream" -> stream,
          "key" -> user,
          "score" -> 1,
          "B" -> "",
          "description" -> (" new user ")
        )

        val returnJson = new JSONObject(outMap)
        returnVal += returnJson.toString() + "\n"
      }

    }

    else {

      scores = scores :+ f

      if (f<=thr){
        logger.info("Anomaly at counter {}", counter)
        //returnVal = "user "+A+ " accessing machine "+B+" is anomalous"
        val outMap = Map(
          "date" -> date,
          "stream" -> stream,
          "key" -> user,
          "score" -> 4,
          "B" -> "",
          "description" -> (" abnormal activity ")
        )

        val returnJson = new JSONObject(outMap)
        returnVal += returnJson.toString() + "\n"
      }
    }

    //logger.info("calling upd_profile when counter {}",counter)
    CB.upd_profile(user, featureMap)

    logger.info("Checking user {} ", user)


    returnVal
  }
}

class CBReco {

  var profile = collection.mutable.HashMap[String, collection.mutable.HashMap[String, Int]]()
  var total = collection.mutable.HashMap[String, Int]()

  final val logger: Logger = LoggerFactory.getLogger(classOf[CBReco])

  def upd_profile(user: String, featureMap : scala.collection.mutable.Map[String, String]){

    //updating profile
    if (profile.contains(user)) {
      val feat_pro = profile(user)

      for ((k, value) <- featureMap){
        if (value == "1" &&  k != "stream" && k != "date" && k != "count")
        feat_pro.update(k, feat_pro.getOrElseUpdate(k, 0) + 1)
      }
    }

    else{
      profile += user -> collection.mutable.HashMap[String, Int]()
      var feat_pro = profile(user)

      for ((fName, value) <- featureMap) {
        if (value == "1") {
          feat_pro.update(fName, 1)
        }
      }
    }

    //updating total
    total.update(user, total.getOrElseUpdate(user, 0) + 1)
  }

  def breaks(scores: Array[Float], bins: Int, use_bins: Int): Float = {

    val min = scores.min
    val l = (scores.max - min)/bins

    var counts = new Array[Int](0)
    for (i <- 1 to use_bins){
      counts = counts :+ 0
    }

    var b = 0

    for (i <- 0 until scores.length){
      b = ((scores(i) - min)/l).asInstanceOf[Int]
      if (b<10){
        counts(b) += 1
      }
    }

    val k = counts.indexOf(counts.min)
    val break: Float = min + l*(k+1)

    logger.info("computed breaks {}", break)
    return break

  }

  def score(user: String, features: scala.collection.mutable.Map[String, String], F: Int): Float = {

    if (!profile.contains(user)) return -1

    var sco: Double = 0
    for ((k,v) <- features){
      if (k != "stream" && k != "date" && k != "count") {
        sco += profile(user).getOrElse(k, 0)
      }
    }
    sco = (1.0 * sco)/ (total(user) * F)

    return sco.asInstanceOf[Float]
  }
}