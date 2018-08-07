package com.eps

import scala.collection.mutable.ListBuffer

object LTest {

  def main( args : Array[String] ) : Unit = {

    val l = ListBuffer(1,2,3)
    Console.println(l.head)
    Console.println(l.last)

    l += 4
    Console.println(l.head)
    Console.println(l.last)
  }

}
