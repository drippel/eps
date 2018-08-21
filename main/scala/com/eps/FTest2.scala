package com.eps

object FTest2 {

  def m1(x:Int) = x+3
  val f1 = (x:Int) => x+3

  val callIt = ( x : Int, f : (Int) => Int ) => { f(x) }

  def main( args : Array[String] ) : Unit = {
    Console.println( "ftest..." )
    Console.println( callIt( 1, m1 ) )
    Console.println( callIt( 1, f1 ) )
  }

}
