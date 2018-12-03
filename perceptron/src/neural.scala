package perceptron

object neural {

  import breeze.linalg._

  case class node(size: Int, fn: Double=>Double, eta: Double) {
    private var input = DenseVector.rand[Double](size+1)
    private var calc = 0.0
    var weights = DenseVector.rand[Double](size+1)
    
    def forward(x: Array[Double]): Double = {
        input = DenseVector(Array(1.0) ++ x)
        calc = fn(weights.dot(input))
        calc
    }

    def backprop(delta: Double): DenseVector[Double] = {
        val ider = eta/(fn(calc + eta/2) - fn(calc - eta/2) + eta*Math.random)
        val res = (-delta*eta*ider)*input
        weights += res
        res
    }
  }
  
  case class layer(size: Int, num: Int, fn: Double=>Double, eta: Double) {
    val nodes = Range(0,num).map(i=>node(size,fn,eta)).toArray

    def forward(xs: Array[Double]) = 
      nodes.map(_.forward(xs)).toArray
    
    def backprop(ds: Array[Double]) =
      (nodes,ds).zipped
                .map({case (n,d)=> n.backprop(d)})
                .reduce(_+_)
                .toArray
  }

  case class network(shape:Array[(Int, Int, Double=>Double, Double)]) {
    val layers = shape.map({case (n,m,fn,eta) => layer(n,m,fn,eta)})

    def forward(xs:Array[Double]) = layers.foldLeft(xs)((ys,ns)=>ns.forward(ys))
    def backprop(ds:Array[Double]) = layers.foldRight(ds)((ns,ys)=>ns.backprop(ys))
  }

}

