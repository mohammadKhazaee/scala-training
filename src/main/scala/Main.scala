import LinkedList.flatten

object Main {
  //
  //  sealed trait Animal[T]
  //
  //  case class Dog[T](name: T,friend: Animal[T]) extends Animal[T]
  //  case class NULL[T](value: T) extends Animal[T]
  //
  //  val a = Dog(Dog(1, NULL(2)), NULL(3))

  def main(args: Array[String]): Unit = {
    val test = Node(1, Node(2, LastNode(3)))
    val test2 = Node(4, Node(5, LastNode(6)))
    val test3 = LastNode(7)
    val test1 = Node(test, Node(test2, LastNode(test3)))
    val test0 = LastNode(test)

    println("--------------------------------------------------------------------")
    //    println(test.head)
    //    println(test.init)
    //    println(test.tail)
    //    println(test.last)
    //    println(test take 2)
    //    println(test.drop(1))
    //    println(test.takeWhile(k => k < 3))
    //    println(test.dropWhile(k => k < 0))
    //    println(test3.map(k => k * 2))
    println(flatten(test1))

    //    println(test1.tail)
    //    println(test1.init)
  }
}