package com.eps

object FTester {

  class ITestContext( val third : Int = 100 )

  implicit val i : ITestContext = new ITestContext(5)

  def main( args : Array[String] ) : Unit = {
    Console.println("test...")
    Console.println( addThree( 1, 2, 3 ) )

    val withFirst = addThree( 1, _ : Int, _ : Int )
    Console.println( withFirst( 2, 3 ) )

    val atTheEnd = addThree( _ :Int, _ : Int, 7 )
    Console.println( atTheEnd( 2, 3 ) )

    val letsSee = addThree( _ : Int, _ : Int, c = 10 )
    Console.println( letsSee( 2, 3 ) )

    Console.println( iTest( 2, 3 ) )
    val iTestWith6 = iTest( _ : Int, _ : Int )( new ITestContext(6) )

    Console.println( iTestWith6( 2, 3 ) )
  }

  def addThree( a : Int, b : Int, c : Int ) : String = {
    "a:" + a + "b:" + b + "c:" + c
  }

  def iTest( a : Int, b : Int )( implicit ctx : ITestContext ) : String = {
    "a:"+ a +", b:"+ b + " ctx:" + ctx.third
  }

}
