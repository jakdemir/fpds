package streams

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import Bloxorz._

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) =>
        require(block.isLegal) // The solution must always lead to legal blocks
        move match {
          case Left => block.left
          case Right => block.right
          case Up => block.up
          case Down => block.down
        }
    }
  }

  trait Level1 extends SolutionChecker {
      /* terrain for level 1*/

    val level =
    //"0123456789"
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }


	test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0,0)), "0,0")
      assert(terrain(Pos(1,1)), "1,1") // start
      assert(terrain(Pos(4,7)), "4,7") // goal
      assert(terrain(Pos(5,8)), "5,8")
      assert(!terrain(Pos(5,9)), "5,9")
      assert(terrain(Pos(4,9)), "4,9")
      assert(!terrain(Pos(6,8)), "6,8")
      assert(!terrain(Pos(4,11)), "4,11")
      assert(!terrain(Pos(-1,0)), "-1,0")
      assert(!terrain(Pos(0,-1)), "0,-1")
    }
  }

	test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1,1))
    }
  }

  test("isStanding: Standing"){
    new Level1 {
      assert(Block(Pos(1,1), Pos(1,1)).isStanding)
    }
  }

  test("isStanding: Not Standing"){
    new Level1 {
      assert(!Block(Pos(1,1), Pos(2,1)).isStanding)
    }
  }

  test("isLegal: Legal"){
    new Level1 {
      assert(Block(Pos(1,1), Pos(2,1)).isLegal)
    }
  }

  test("isLegal: Illegal"){
    new Level1 {
      assert(!Block(Pos(0,2), Pos(0,3)).isLegal)
    }
  }

  test("isLegal: More Legal"){
    new Level1 {
      assert(Block(Pos(0,0), Pos(0,0)).isLegal)
    }
  }

  test("isLegal: More illegal"){
    new Level1 {
      assert(!Block(Pos(100,100), Pos(100,101)).isLegal)
    }
  }

  test("startBlock: true"){
    new Level1 {
      assert(startBlock == Block(Pos(1,1), Pos(1,1)))
    }
  }

  test("startBlock: false"){
    new Level1 {
      assert(startBlock != Block(Pos(7,7), Pos(7,7)))
    }
  }

  test("neighborsWithHistory: test1") {
    new Level1 {
      assert(neighborsWithHistory(Block(Pos(1, 1), Pos(1, 1)), List(Left, Up)) == Stream(
        (Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
        (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))
      ))
    }
  }

  test("neighborsWithHistory: test2") {
    new Level1 {
      assert(neighborsWithHistory(Block(Pos(3, 3), Pos(3, 4)), List(Left, Up)) == Stream(
        (Block(Pos(3, 2), Pos(3, 2)), List(Left, Left, Up)),
        (Block(Pos(3, 5), Pos(3, 5)), List(Right, Left, Up)),
        (Block(Pos(2, 3), Pos(2, 4)), List(Up, Left, Up))
      ))
    }
  }
//  def newNeighborsOnly(neighbors: Stream[(Block, List[Move])],explored: Set[Block]): Stream[(Block, List[Move])] = {

    test("newNeighbors: test1") {
    new Level1 {
      assert(newNeighborsOnly(
        Set(
          (Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
          (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))
        ).toStream,
        Set(Block(Pos(1,1),Pos(1,1)), Block(Pos(1,4),Pos(1,4)))) ==
        Set((Block(Pos(2,2),Pos(2,3)),List(Down, Right, Left, Up))).toStream
      )
    }
  }

  //def from(initial: Stream[(Block, List[Move])],explored: Set[Block]): Stream[(Block, List[Move])]

  test("from: test1") {
    new Level1 {
      assert(from(
        Stream((Block(Pos(2, 2), Pos(2, 2)), List())), Set()).take(10) == Stream((Block(Pos(2,2),Pos(2,2)),List()), (Block(Pos(2,0),Pos(2,1)),List(Left)), (Block(Pos(2,3),Pos(2,4)),List(Right)), (Block(Pos(0,2),Pos(1,2)),List(Up)), (Block(Pos(1,0),Pos(1,1)),List(Up, Left)), (Block(Pos(2,5),Pos(2,5)),List(Right, Right)), (Block(Pos(1,3),Pos(1,4)),List(Up, Right)), (Block(Pos(3,3),Pos(3,4)),List(Down, Right)), (Block(Pos(0,1),Pos(1,1)),List(Left, Up)), (Block(Pos(1,2),Pos(1,2)),List(Right, Up, Left)))
      )
    }
  }
  //lazy val pathsFromStart: Stream[(Block, List[Move])]

  test("pathsFromStart: 1") {
    new Level1 {
      assert(pathsFromStart.take(1) == Stream((Block(Pos(1,1),Pos(1,1)),List())))
    }
  }

  test("pathsFromStart: 2") {
    new Level1 {
      assert(pathsFromStart.take(2) == Stream((Block(Pos(1,1),Pos(1,1)),List()), (Block(Pos(1,2),Pos(1,3)),List(Right))))
    }
  }

  test("pathsFromStart: 5") {
    new Level1 {
      assert(pathsFromStart.take(5) == Stream((Block(Pos(1,1),Pos(1,1)),List()), (Block(Pos(1,2),Pos(1,3)),List(Right)), (Block(Pos(2,1),Pos(3,1)),List(Down)), (Block(Pos(1,4),Pos(1,4)),List(Right, Right)), (Block(Pos(2,2),Pos(2,3)),List(Down, Right))))
    }
  }

  test("pathsToGoal: 1") {
    new Level1 {
      assert(pathsToGoal.take(3) == Stream((Block(Pos(4,7),Pos(4,7)),List(Down, Right, Right, Right, Down, Right, Right)), (Block(Pos(4,7),Pos(4,7)),List(Right, Down, Down, Right, Right, Down, Right)), (Block(Pos(4,7),Pos(4,7)),List(Right, Down, Right, Right, Down, Down, Right))))
    }
  }


  test("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) == Block(goal, goal))
    }
  }


	test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }

}
