package com.caspida.algorithms.generic.cbrecommendations

import java.util

import org.apache.commons.math3.ml.clustering.{CentroidCluster, KMeansPlusPlusClusterer}

/**
 * Created by bartimaeus on 9/9/14.
 */


class HashDist {

  def dist(event1: collection.mutable.HashMap[String, String], event2: collection.mutable.HashMap[String, String], F: Int): Double ={
    var matched = 0
    for ((feat, value) <- event1){
      if (value == "1" &&  feat != "stream" && feat != "date" && feat != "count"){
        if (event2(feat) == "1"){matched += 1}
      }
    }

    val d = F - matched
    val dis = math.sqrt(2*d*d)
    return dis
  }

}

class HD extends org.apache.commons.math3.ml.distance.DistanceMeasure{
  def compute(event1: collection.mutable.HashMap[String, String], event2: collection.mutable.HashMap[String, String], F: Int): Double={
    var matched = 0
    for ((feat, value) <- event1){
      if (value == "1" &&  feat != "stream" && feat != "date" && feat != "count"){
        if (event2(feat) == "1"){matched += 1}
      }
    }

    val d = F - matched
    val dis = math.sqrt(2*d*d)
    return dis
  }

  override def compute(a: Array[Double], b: Array[Double]){}
}

object KMeansPlusPlusClusterer {

  def main(args: Array[String]): Unit ={

    val KM = new KMeansPlusPlusClusterer[MyClass](1, 2, HD)

    var point0 = new Array[Double](0)
    point0 = point0 :+ 0.0
    point0 = point0 :+ 0.0
    point0 = point0 :+ 0.0

    var point1 = new Array[Double](0)
    point1 = point1 :+ 1.0
    point1 = point1 :+ 1.0
    point1 = point1 :+ 1.0

    var points = new Array[Array[Double]](0)
    points = points :+ point0
    points = points :+ point1

    System.out.println(point0.length)

    class MyClass(group: Array[Double]) extends org.apache.commons.math3.ml.clustering.Clusterable{
      override def getPoint: Array[Double] = group
    }


    val clusters = points.map(x => new MyClass(x))

    System.out.println(clusters.length)

    //val mySet = util.List[Array[Double]];
    //util.Collections.addAll(mySet, points);

    val featureMap: util.Collection[MyClass] =
      scala.collection.JavaConversions.asJavaCollection(clusters)

    val clus: util.List[CentroidCluster[MyClass]] = KM.cluster(featureMap)

    val p: CentroidCluster[MyClass] = clus.get(0)

    val q: util.List[MyClass] = p.getPoints()

    val u: MyClass = q.get(1)

    val v: Array[Double] = u.getPoint()


    System.out.println(v(0))

  }

}

