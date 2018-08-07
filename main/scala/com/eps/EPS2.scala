package com.eps

import java.io.FileReader
import java.util.Date

import org.apache.commons.io.IOUtils

import scala.collection.mutable.{HashMap, ListBuffer, Stack}

object EPS2 {


  /*
  Forth has the following

  program - instruction list
  stack   - global stack
  dictionary - map of words
  word    - function
  rstack  - return stack - set and cleared between execution of words
  lstack  - local stack  - also used only within a word execution
  heap    - global variable map
   */


  // compile - build the program
  // produces a list of statements/words
  // so we have a list of instructions and pointer to current instruction
  // part of compilation will calc the

  // execute current instruction
  // is the item a word - execute the word
  // if its not a word - push value onto stack

  // this is fun but I don't want to create a forth interpreter

  // constant casting is annoying
  // mismatch between typing of languages

  //  The all-important data stack
  // global stack
  val stack = new Stack[Any]()

  def spush( any : Any ) = {
    if( any.isInstanceOf[Word] ){
      Console.println( any.asInstanceOf[Word].name )
      executeWord( any.asInstanceOf[Word] )
    }
    else {
      stack.push(any)
    }
  }
  def spop() = { stack.pop() }
  def speek() = { stack.top }

  // function stack
  val rstack = new Stack[Any]()
  def rpush( any : Any ) = { rstack.push(any) }
  def rpop() = { rstack.pop() }
  def rpeek() = { rstack.top }

  // control stack
  val cstack = new Stack[Any]()

  // The heap. Maps names to data (i.e. variables)
  val heap =  new HashMap[String,Any]()

  case class Word( val name : String, val func : () => Unit )

  def executeWord( word : Word ) : Unit = {

    // clear the rstack
    rstack.clear()

    word.func()

    // clear the rstack
    rstack.clear()

  }

  // read the file name in stack
  // push the contents onto stack
  def read_file() : Unit = {
    rpush( spop.asInstanceOf[String] )
    rpush( new FileReader( rpop.asInstanceOf[String] ) )
    rpush( IOUtils.toString( rpop.asInstanceOf[FileReader] ) )
    spush( rpop.asInstanceOf[String] )
  }

  def filter_chars() : Unit = {
    //  Takes data on the stack and places back a copy with all
    //  nonalphanumeric chars replaced by white space.
    // This is not in style. RE is too high-level, but using it
    // for doing this fast and short. Push the pattern onto stack
    // Push the result onto the stack
    rpush( spop.asInstanceOf[String].toLowerCase )
    spush( rpop.asInstanceOf[String].map(
      ( c : Char ) => {
        if( c.isLetterOrDigit ){ c }
        else{ ' ' }
      } ) )
  }

  def scan() = {
    // Takes a string on the stack and scans for words, placing
    // the list of words back on the stack
    // Again, split() is too high-level for this style, but using
    // it for doing this fast and short. Left as exercise.
    stack.pushAll( spop().asInstanceOf[String].split(' ').filter( ( s : String ) => { s.trim.length > 1 } ).toList )
  }


  def remove_stop_words() = {

    // Takes a list of words on the stack and removes stop words.
    spush(
      IOUtils.toString( new FileReader( "../src/main/resources/stop_words.txt" ) )
        .split(',')
        .toList )

    // add single-letter words
    // stack.pushAll( list( string.ascii_lowercase ) )

    heap.put( "stop_words", spop() )

    // # Again, this is too high-level for this style, but using it
    // # for doing this fast and short. Left as exercise.
    heap.put( "words", ListBuffer[String]() )

    while( stack.length > 0 ) {

      if( heap("stop_words" ).asInstanceOf[List[String]].contains( speek().asInstanceOf[String] ) ) {
        spop() // pop it and drop it
      }
      else {
        heap("words").asInstanceOf[ListBuffer[String]] += spop().asInstanceOf[String] // # pop it, store it
      }
      // Console.println( stack.length() )
    }

    stack.pushAll( heap.get("words").get.asInstanceOf[ListBuffer[String]].toList )
    // # Load the words onto the stack

    heap.remove("stop_words")
    heap.remove( "words" ) // # Not needed
  }



  def frequencies() = {

    // Takes a list of words and returns a dictionary associating
    // words with frequencies of occurrence.
    heap( "word_freqs" ) = HashMap[String,Int]()

    // A little flavour of the real Forth style here...
    while( stack.length > 0 ) {

      // # ... but the following line is not in style, because the
      // # naive implementation would be too slow
      if( heap("word_freqs").asInstanceOf[HashMap[String,Int]].contains( speek.asInstanceOf[String] ) ) {

        // # Increment the frequency, postfix style: f 1 +
        spush(
          heap("word_freqs").asInstanceOf[HashMap[String,Int]]( speek.asInstanceOf[String]) ) //  push f
        spush(1) // push 1
        spush( spop().asInstanceOf[Int] + spop().asInstanceOf[Int] ) //  add
      }
      else {
        spush(1) // #Push 1 in stack[2]
        // Load the updated freq back onto the heap
      }

      // TODO: cheat
      val t = (spop.asInstanceOf[Int], spop().asInstanceOf[String]).swap

      heap("word_freqs").asInstanceOf[HashMap[String,Int]].put( t._1, t._2 )

    }

    // Push the result onto the stack
    spush( heap("word_freqs") )

    heap.remove("word_freqs") // # We don't need this variable anymore
  }

  def sort() = {
    // we have a map of String,Int on the stack
    // sort them and add all to the stack
    // Not in style , left as exercise
    val ts = spop.asInstanceOf[HashMap[String,Int]]
    val ss = ts.toList.sortWith( ( t1 : (String,Int), t2 : (String,Int)) => { t1._2 < t2._2 } )
    stack.pushAll( ss )
  }

  // The main function
  // input 300 ms
  // p&p   17000 ms
  def main( args : Array[String] ) : Unit = {

    // use the stack - need to work on the print loop
    val start = (new Date()).getTime

    Console.println( "eps2..." )
    // stack.push( "../src/main/resources/input.txt" )
    spush( "../src/main/resources/pride-and-prejudice.txt" )
    spush( Word( "read_file", read_file ) )
    spush( Word( "filter_chars", filter_chars ) )
    spush( Word( "scan", scan ) )
    spush( Word( "remove_stop_words", remove_stop_words ) )
    spush( Word( "frequencies", frequencies ) )
    spush( Word( "sort", sort ) )

    spush(0)

    // Check stack length against 1, because after we process
    // the last word there will be one item left
    while( speek.asInstanceOf[Int] < 25 && stack.length > 1 ) {

      heap.put( "i", spop() )

      val (w, f) = spop().asInstanceOf[(String,Int)]
      Console.println( w +" - " + f )
      spush( heap("i"))
      spush(1)
      spush( spop().asInstanceOf[Int] + spop().asInstanceOf[Int] )
    }

    val end = (new Date()).getTime
    Console.println( "runtime:" + ( end - start ))
  }

}


