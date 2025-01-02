object LinkedList {
  sealed trait LL[+T] {
    def head: T

    def tail: LL[T]

    def init: LL[T]

    def last: T

    def take(c: Int): LL[T]

    def drop(c: Int): LL[T]

    def takeWhile(p: T => Boolean): LL[T]

    def dropWhile(p: T => Boolean): LL[T]

    def map[U](f: T => U): LL[U]
  }

  case class Node[+T](value: T, next: LL[T]) extends LL[T] {
    override def head: T = value

    override def tail: LL[T] = next match {
      case Node(_, _) => Node(value, next.tail)
      case LastNode(_) => next
    }

    override def init: LL[T] = next match {
      case Node(_, _) => Node(value, next.init)
      case LastNode(_) => LastNode(value)
    }

    override def last: T = next.last

    override def take(c: Int): LL[T] = if (c < 0) throw new Exception("invalid input") else c match {
      case 0 => throw new Exception("cant take 0 item")
      case 1 => LastNode(value)
      case _ => Node(value, next.take(c - 1))
    }

    override def drop(c: Int): LL[T] = if (c < 0) throw new Exception("invalid input") else c match {
      case 0 => this
      case _ => next.drop(c - 1)
    }

    override def takeWhile(p: T => Boolean): LL[T] = (p(value), next) match {
      case (false, _) => throw new Exception("cant drop first item")
      case (_, n: Node[T]) => if (p(n.value)) Node(value, n.takeWhile(p)) else LastNode(value)
      case (_, n: LastNode[T]) => if (p(n.value)) Node(value, n) else LastNode(value)
    }

    override def dropWhile(p: T => Boolean): LL[T] = (p(value), next) match {
      case (true, n: LastNode[T]) => if (!p(n.value)) n else throw new Exception("cant dropWhile last item")
      case (true, n: Node[T]) => if (!p(n.value)) n else n.dropWhile(p)
      case _ => this
    }

    override def map[U](f: T => U): LL[U] = Node(f(value), next.map(f))
  }

  case class LastNode[+T](value: T) extends LL[T] {
    override def head: T = value

    override def tail: LL[T] = throw new Exception("invalid input")

    override def init: LL[T] = throw new Exception("invalid input")

    override def last: T = value

    override def take(c: Int): LL[T] = if (c < 0) throw new Exception("invalid input") else c match {
      case 0 => throw new Exception("cant take 0 item")
      case _ => this
    }

    override def drop(c: Int): LL[T] = if (c == 0) this else throw new Exception("invalid input")

    override def takeWhile(p: T => Boolean): LL[T] = throw new Exception("cant takeWhile last item")

    override def dropWhile(p: T => Boolean): LL[T] = throw new Exception("cant dropWhile last item")

    override def map[U](f: T => U): LL[U] = LastNode(f(value))
  }

  def concat[T](f: LL[T], s: LL[T]): LL[T] = {
    f match {
      case LastNode(value) => Node(value, s)
      case Node(value, next) => Node(value, concat(next, s))
    }
  }

  def flatten[T](in: LL[LL[T]]): LL[T] = {
    in match {
      case l: LastNode[LL[T]] => l.value
      case Node(value, next) => concat(value, flatten(next))
    }
  }
}
