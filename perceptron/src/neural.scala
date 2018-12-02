package perceptron

object neural {

  import breeze.linalg._

  case class node(size: Int, fn: Double=>Double, eta: Double) {

    private var input = DenseVector.rand[Double](size+1)
    private var calc = 0.0

    var weights = DenseVector.rand[Double](size+1)
    
    def forward(x: Array[Double]): Double = {
        input = DenseVector.vertcat(DenseVector(1.0), DenseVector(x))
        calc = fn(weights.dot(input))
        return(calc)
    }

    def backprop(delta: Double): DenseVector[Double] = {
        val der = (fn(calc + eta/2) - fn(calc - eta/2))/eta
        val res = (-delta*eta/(der+eta*Math.random))*input
        weights += res
        return(res)
    }
  }
  
  case class network(shape:Array[(Int, Int, Double=>Double, Double)]) {
    val nodes = shape.map({ case(size,num,fn,eta) => (1 to num).map(i => node(size,fn,eta)).toArray })
    def forward(xs:Array[Double]) = nodes.foldLeft(xs)((ys,ns) => ns.map(_.forward(ys)))
    def backprop(ds:Array[Double]) = nodes.foldRight(ds)((ns,ys) => (ns,ys).zipped.map((n,d) => n.backprop(d)).reduce(_+_).toArray)
  }

}

