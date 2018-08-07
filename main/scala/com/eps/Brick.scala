package com.eps

object Brick {

  def main( args : Array[String] ) : Unit = {

    val makeBricks15 = makeBricks( _ : Int, _ : Int, _ : Int, 1, 5 )

    Console.println( makeBricks15(3, 1, 8) )
    Console.println( makeBricks15(3, 1, 9))
    Console.println( makeBricks15(3, 2, 9) )
    Console.println( makeBricks15(3, 2, 10))

    var amts = List( (1,5), (3,1))
    Console.println( makeBricksTuple( amts, 8 ))
    amts = List( (1,5), (3,1))
    Console.println( makeBricksTuple( amts, 9 ))
    amts = List( (2,5), (3,1))
    Console.println( makeBricksTuple( amts, 9 ))
    amts = List( (2,5), (3,1))
    Console.println( makeBricksTuple( amts, 10 ))

    val sizes = List(5,1)
    var counts = List(1,3)
    Console.println( makeBricksLists( sizes, counts, 8 ))
    Console.println( makeBricksLists( sizes, counts, 9 ))

    counts = List(2,3)
    Console.println( makeBricksLists( sizes, counts, 9 ))
    counts = List(2,3)
    Console.println( makeBricksLists( sizes, counts, 10 ))

    //
    val makeBricksList15 = makeBricksLists( sizes, _ : List[Int], _ : Int )
    Console.println( makeBricksList15( List(1,3), 8 ))
    Console.println( makeBricksList15( List(1,3), 9 ))
    Console.println( makeBricksList15( List(2,3), 9 ))
    Console.println( makeBricksList15( List(2,3), 10 ))
  }

  def makeBricksLists( brickSizes : List[Int], brickAmounts : List[Int], goal : Int ) : Boolean = {
    val ts = brickSizes.zip(brickAmounts)
    makeBricksTuple(ts,goal)
  }

  def makeBricksTuple( coins : List[(Int,Int)], amount : Int ) : Boolean = {

    if( amount == 0 ){
      true
    }
    else if( coins.isEmpty ){
      false
    }
    else {
      if( coins.head._2 > 0 && amount >= coins.head._1 ){
        if( coins.head._2 == 1 ){
          makeBricksTuple( coins.tail, amount - ( coins.head._1 ))
        }
        else {
          makeBricksTuple( ((coins.head._1,coins.head._2 -1) +: coins.tail ), amount - coins.head._1 )
        }
      }
      else {
        makeBricksTuple( coins.tail, amount )
      }
    }

  }

  def makeBricks( small : Int, big : Int, goal : Int, smallSize : Int, bigSize : Int ) : Boolean = {

    if( goal == 0 ){
      true
    }
    else {

      if( big > 0 && goal >= bigSize ){
        makeBricks( small, big - 1, goal - bigSize, smallSize, bigSize )
      }
      else if( small > 0 && goal >= smallSize ) {
        makeBricks( small - 1, big, goal - smallSize, smallSize, bigSize )
      }
      else {
        false
      }
    }
  }

}
