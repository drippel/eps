package com.eps

import java.lang.reflect.Method

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
    val f = ops("+")

    val c = add.getClass

    val ms = c.getMethods

    for( m <- ms.filter( (mt:Method) => { "apply".equalsIgnoreCase( mt.getName ) } ) ){
      Console.println("name:" + m.getName )
      Console.println("return:" + m.getReturnType().getCanonicalName()  )

      for( p <- m.getParameters ){
        Console.println( p.getName )
        Console.println( p.getType().getClass.getName )
        Console.println( p.getType() )

      }
    }


  }

}
