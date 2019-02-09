object Huffman {

  case class Tree (
    left: Option[Tree],
    right: Option[Tree],
    symbols: Array[String],
    weight: Int
  )

  def isLeaf(branch: Tree):Boolean = {
    return branch.left == None && branch.right == None
  }

  def printEncoded(message: Array[Int]): Unit = {
    message.foreach( (b) => print(b) )
    println()
  }

  def printDecoded(message: Array[String]): Unit = {
    message.foreach( (str) => print(str + " "))
    println()
  }

  def printTree(tree: Tree): Unit = {
    println("Tree : " + tree.symbols.mkString(", ") + "; " + tree.weight)
    if(!isLeaf(tree)) {
      printTree(tree.left.get)
      printTree(tree.right.get)
    }

  }

  def encodeSymbol(sym: String, tree: Tree): Array[Int] = {
    var encoded = Array.empty[Int]
    var currentBranch = tree

    while (!isLeaf(currentBranch)) {
      if(currentBranch.left.get.symbols.contains(sym)) {
        encoded = encoded :+ 0
        currentBranch = currentBranch.left.get
      }
      else {
        encoded = encoded :+ 1
        currentBranch = currentBranch.right.get
      }
    }
    println("Encoded " + sym + " as " + encoded.mkString(" "))

    return encoded
  }

  def encode(message: Array[String], tree: Tree): Array[Int] = {
    var results = Array.empty[Int]

    message.foreach( (sym) => results = results ++ encodeSymbol(sym, tree))

    return results
  }

  def decode(bits: Array[Int], tree: Tree): Array[String] = {

    var result = Array.empty[String]
    var currentBranch = tree
    var i = -1
    
    for(bit <- bits) {
      i += 1
      if(isLeaf(currentBranch)) {
        println("Decoded : " +i+" : "+ currentBranch.symbols(0))
        result = result :+ currentBranch.symbols(0)
        currentBranch = tree
      }
      if(bit == 0) {
        currentBranch = currentBranch.left.get
      }
      else {
        currentBranch = currentBranch.right.get
      }
    }

      if(isLeaf(currentBranch)) {
        println("Decoded : " +i+" : "+ currentBranch.symbols(0))
        result = result :+ currentBranch.symbols(0)
        currentBranch = tree
      }
    return result
  }

  def combineBranches(left: Tree, right: Tree): Tree = {
    return new Tree(Some(left), Some(right), left.symbols ++ right.symbols, left.weight + right.weight)
  }

  def successiveMerge(pairs: Array[Tree]): Tree = {
    var merged = pairs.clone

    while(merged.length > 1) {
      val first = merged(0)
      val second = merged(1)
      merged = merged.drop(2)
      merged = combineBranches(first, second) +: merged
      merged = merged.sortWith(_.weight < _.weight)
    }
    
    return merged.head
  }

  def generateHuffmanTree(freqList: Array[(String, Int)]): Tree = {
    var leafs = Array.empty[Tree]
    freqList.foreach( (leaf) => {leafs = new Tree(None, None, Array(leaf._1), leaf._2) +: leafs})

    leafs = leafs.sortWith(_.weight < _.weight)

    return successiveMerge(leafs)
  }

  def main(args: Array[String]): Unit = {
    var tree = generateHuffmanTree(
                        Array(("NA", 16), ("YIP", 9), ("SHA", 3), ("A", 2), 
                              ("GET", 2), ("JOB", 2), ("BOOM", 1), ("WAH", 1)))
    var encoded = encode(Array("NA", "YIP", "SHA", "JOB"), tree)
    printTree(tree)
    printEncoded(encoded)
    printDecoded(decode(encoded, tree))
  }
}
