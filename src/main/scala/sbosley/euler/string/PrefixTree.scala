package sbosley.euler.string

import scala.annotation.tailrec
import scala.collection.mutable

class PrefixTree {

  private val root = new Node("")

  class Node(wordSoFar: String,
             children: mutable.Map[Char, Node] = mutable.Map[Char, Node](),
             private var nodeIsWord: Boolean = false) {

    def addChild(c: Char, n: Node): Unit = children += (c -> n)
    def getChild(c: Char): Option[Node] = children.get(c)
    def getOrAddChild(c: Char): Node = {
      val node = getChild(c)
      node match {
        case Some(n) => n
        case None =>
          val newNode = new Node(wordSoFar + c)
          children.put(c, newNode)
          newNode
      }
    }
    def isPrefix: Boolean = children.nonEmpty
    def isWord: Boolean = nodeIsWord
    def setIsWord(): Unit = nodeIsWord = true
  }

  def isPrefix(s: String): Boolean = {
    findNode(s, root).exists(_.isPrefix)
  }

  def isWord(s: String): Boolean = {
    findNode(s, root).exists(_.isWord)
  }

  def addWord(word: String): Unit = addWordRecursive(word, root)

  @tailrec
  private def addWordRecursive(s: String, currNode: Node): Unit = {
    if (s.isEmpty) currNode.setIsWord()
    else addWordRecursive(s.tail, currNode.getOrAddChild(s.head))
  }

  private def findNode(s: String, currNode: Node): Option[Node] = {
    if (s.isEmpty) Some(currNode)
    else currNode.getChild(s.head).flatMap(findNode(s.tail, _))
  }
}
