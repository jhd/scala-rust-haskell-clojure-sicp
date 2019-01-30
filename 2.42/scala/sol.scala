import scala.collection.mutable.ListBuffer

object Queens {

  def queens(boardSize: Int): Array[Array[Int]] = {
    return queenCol(boardSize, boardSize).filter( (position: Array[Int]) => isSafe(position))
  }

  def queenCol(k: Int, boardSize: Int): Array[Array[Int]] = {
    if(k == 1){
      return firstBoard(boardSize)
    }
    var existing = queenCol(k - 1, boardSize).filter( (position: Array[Int]) => isSafe(position))
    var candidates = Array.empty[Array[Int]]

    for( i <- 1 to boardSize ){
      existing.map( (position: Array[Int]) => {
          var candidate: Array[Int] = position :+ i
          candidates = candidates :+ candidate
        })
    }
    return candidates

  }

  def isSafe(positions: Array[Int]): Boolean = {
    if( positions.length < 1 ){
      return true
    }

    for ((queen, i) <- positions.reverse.tail.zipWithIndex){
              if( queen == positions.last ||
                  queen + i + 1 == positions.last ||
                  queen - i - 1 == positions.last ){
                return false
              }
    }

    return true
  }

  def firstBoard(k: Int): Array[Array[Int]] = {
    var board: Array[Array[Int]] = Array.empty[Array[Int]]
    for(i <- 1 to k){
      board = board :+ Array(i)
    }
    return board
  }

  def printBoard(board: Array[Array[Int]]): Unit = {
    board.foreach( (b) => {
      print("[")
      print(b.mkString(""))
      print("]")
    })
    println()
  }

  def main(args: Array[String]): Unit = {
    println(printBoard(queens(4)));
    println(printBoard(queens(8)));
  }
}

