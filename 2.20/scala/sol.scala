object SameParity {

  def isEven(x: Int): Boolean = {
    return x%2 == 1
  }

  def sameParity(xs: List[Int]): List[Int] = {
    if(xs.isEmpty)
      return Nil
    val x = xs.head
    
    if(isEven(x)){
      return xs.filter(isEven)
    }
    else{
      return xs.filter( (x: Int ) => !isEven(x) )
    }
  }
  def main(args: Array[String]): Unit = {
    println(sameParity(List(1, 2, 3, 4, 5, 6, 7)))
    println(sameParity(List(2, 3, 4, 5, 6, 7)))
  }
}
