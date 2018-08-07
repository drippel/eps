package com.eps

import scala.collection.mutable.{HashMap, ListBuffer, Stack}

class Word( val nm : String ) {
  val steps = ListBuffer[Word]()

  def execute( runtime : Runtime ) = {}
}

// basic built in words that can be dropped onto the program
class Noop() extends Word( "NOOP" ){
  override def execute(runtime : Runtime ) = {
    // truly noop
  }
}

class NumberValue( val n : Number ) extends Word( "NUMBER" ){

  // the runtime action of the number word
  // is to push the value on the stack
  override def execute(runtime : Runtime) : Unit = {
    runtime.stack.push(n)
  }
}

class Add() extends Word( "+" ){

  override def execute(runtime : Runtime) : Unit = {
    val a = runtime.stack.pop().asInstanceOf[Long]
    val b = runtime.stack.pop.asInstanceOf[Long]
    runtime.stack.push( a + b )
  }
}
class CreatedWord( nm : String ) extends Word( nm ){}

class Forth {

  val dictionary = HashMap[String,Word]()
}

class Parser(){}

class Compiler(){}

class Interpreter(){}

class Program(){
  val steps = ListBuffer[Word]()
}

class Runtime(){

  // the stack
  val stack = Stack[Any]()

  // return stack
  val rstack = Stack[Any]()

  // local stack
  val lstack = Stack[Any]()

  // heap
  val heap = HashMap[String,Any]()

}

object Forth {

  val p = """1 2 +"""
  def main( args : Array[String] ) : Unit = {
    Console.println("forth...")
  }
}
