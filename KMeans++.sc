import java.util

import org.apache.commons.math3.ml.clustering.KMeansPlusPlusClusterer

val KM = new KMeansPlusPlusClusterer[MyClass](4, 10)

//var clus = KM.cluster()
val point0 = new Array[Double](0)
point0 :+ 0
point0 :+ 0
point0 :+ 0
val point1 = new Array[Double](0)
point1 :+ 1
point1 :+ 1
point1 :+ 1

val points = new Array[Array[Double]](0)
points :+ point0
points :+ point1

class MyClass(group: Array[Double]) extends org.apache.commons.math3.ml.clustering.Clusterable{
  override def getPoint: Array[Double] = group
}


val clusters = points.map( x => new MyClass(x))

//val mySet = util.List[Array[Double]];
//util.Collections.addAll(mySet, points);

val featureMap: util.Collection[MyClass] =
  scala.collection.JavaConversions.asJavaCollection(clusters)
val clus = KM.cluster(featureMap)
System.out.println(clus.get(1))