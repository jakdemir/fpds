package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      k <- arbitrary[Int]
      m <- oneOf(const(empty), genHeap)
    } yield insert(k, m)
  )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll{ a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  lazy val genMap: Gen[Map[Int, Int]] = oneOf(
    const(Map.empty[Int, Int]),
    for {
      k <- arbitrary[Int]
      v <- arbitrary[Int]
      m <- oneOf(const(Map.empty[Int, Int]), genMap)
    } yield m.updated(k, v)
  )

  property("findmin2") = forAll { (a: Int, b: Int) =>
    val heap1 = insert(a, empty)
    val heap2 = insert(b, heap1)
    findMin(heap2) == Math.min(a, b)
  }

  property("delete for emtpy") = forAll { a: Int =>
    val heap1 = insert(a, empty)
    empty == deleteMin(heap1)
  }

  property("findminHeaps") = forAll { (a: Int, b:Int) =>
    val heap1 = insert(a, empty)
    val heap2 = insert(b, empty)

    val melded = meld(heap1, heap2)
    findMin(melded) == Math.min(a, b)
  }

  property("getsorted") = forAll {(a: Int, b: Int) =>
    val heap = insert(b, insert(a, empty))
    getSorted(heap) == Seq(Math.min(a, b), Math.max(a, b))
  }

  def getSorted(heap: H): List[Any] = {
    if (heap == empty){
      List()
    }else{
      List(findMin(heap), getSorted(deleteMin(heap)))
    }
  }

}
