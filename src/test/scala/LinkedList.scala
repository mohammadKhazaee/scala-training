import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LinkedList extends AnyFlatSpec with Matchers {
  val test = Node(1, Node(2, LastNode(3)))

  "LinkedList" should "calculate of a ll" in {
    test.head shouldBe 1
    test.init shouldBe Node(1, LastNode(2))
    test.last shouldBe 3
    test.tail shouldBe Node(2, LastNode(3))
    test take 2 shouldBe Node(1, LastNode(2))
    test.drop(1) shouldBe Node(2, LastNode(3))
  }
}
