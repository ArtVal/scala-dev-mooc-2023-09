package module1.datacollections.homework

import scala.util.Random

class BallsExperiment {
  private val random = new Random
  private val urn: List[Int] = List.fill(3)(1) ++ List.fill(3)(0)

  def isFirstBlackSecondWhite(): Boolean = {
    val (firstBall, urn2) = takeBall(urn)
    val secondBall = takeBall(urn2)._1
    val received = List(firstBall, secondBall)
    received match {
      case 0::1::Nil => true
      case _ => false
    }
  }

  private def takeBall(urn: List[Int]): (Int, List[Int]) = {
    val position =  random.nextInt(urn.length)
    val ball = urn(position)
    val urnWithoutBall = urn.zipWithIndex.filter(_._2 != position).map(_._1)
    (ball, urnWithoutBall)
  }
}

object BallsTest {
  def main(args: Array[String]): Unit = {
    val count = 10000
    val listOfExperiments: List[BallsExperiment] = List.fill(count)(new BallsExperiment)
    val countOfExperiments = listOfExperiments.map(_.isFirstBlackSecondWhite())
    val countOfPositiveExperiments: Float = countOfExperiments.count(_ == true)
    println(countOfPositiveExperiments / count)
  }
}