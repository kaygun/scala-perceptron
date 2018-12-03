package perceptron

import perceptron.neural._

object Main {

  import scala.util.Random.nextInt
  import scala.io.Source.fromFile

  def sigmoid(x:Double) = 1.0/(1.0 + math.exp(-x))
  def relu(x:Double) = math.max(x,0.0)

  def train(net:network, xs: Array[Array[Double]], ys: Array[Double], epochs:Int, batchSize:Int, tol:Double):Array[Double] = {
    val size = xs.length
    var err = Array[Double]()
    for(i <- 0 to epochs-1) {
       val j = math.abs(nextInt)%size
       val x = xs(j)
       val y = ys(j)
       val d = net.forward(x)(0) - y
       net.backprop(Array(d))
       if(i % batchSize == 0)
         err = Array(0.0) ++ err
       if(math.abs(d) > tol)
         err(0) += 1.0/batchSize
    }
    return(err.reverse)
  }

  def main(args: Array[String]) {
      val file = fromFile(args(0))
      val size = args(1).toInt
      val eta = args(2).toDouble
      val epochs = args(3).toInt
      val batchSize = args(4).toInt
      val tol = args(5).toDouble
      val data = file.mkString.split("\n").map(x=>x.split("\t").map(_.toDouble).reverse)
      val ys = data.map(_.head)
      val xs = data.map(_.tail)

      val net = network(Array((size,10,sigmoid,eta),(10,3,sigmoid,eta),(3,1,sigmoid,eta)))
      val err = train(net, xs, ys, epochs, batchSize, tol)
      err.foreach(x=>println("%4.3f".format(x)))
  }
}
