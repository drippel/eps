package com.eps

import scala.collection.mutable.HashMap

object MTest {


  val add :  ( Int, Int ) => Int = { _ + _ }
  val sub :  ( Int, Int ) => Int = { _ - _ }
  val inc : ( Int ) => Int = { add( _, 1 ) }


  val ops = HashMap[String, (Int,Int) => Int]()

  ops.put( "+", add )
  ops.put( "-", sub )
  // ops.put( "-", inc )

  def main( args : Array[String] ) = {
    Console.println( ops("+")( 1, 2 ) )
  }

}
