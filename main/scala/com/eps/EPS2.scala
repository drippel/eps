package com.eps

import java.io.FileReader
import java.util.Date

import org.apache.commons.io.IOUtils

import scala.collection.mutable.{HashMap, ListBuffer, Stack}

object EPS2 {

  //  The all-important data stack
  // global stack
  val stack = new Stack[Any]()

  // function stack
  val rstack = new Stack[Any]()

  // The heap. Maps names to data (i.e. variables)
  val heap =  new HashMap[String,Any]()

  case class Word( val name : String, val func : () => Unit )

  val words = HashMap[String, Word]()

  def executeWord( word : Word ) : Unit = {

    // clear the rstack
    rstack.clear()

    // look up the word
    words.get(word.name) match {

      case w : Some[Word] => {

        // execute the word
        try{
          w.get.func()
        }
        catch{
          case t : Throwable => {
            t.printStackTrace()
          }
        }

      }
      case None => {

        // throw an exception or carry on

      }

    }

    // clear the rstack
    rstack.clear()

  }

  // read the file name in stack
  // push the contents onto stack
  def read_file() : Unit = {
    rstack.push( stack.pop().asInstanceOf[String] )
    rstack.push( new FileReader( rstack.pop().asInstanceOf[String] ) )
    rstack.push( IOUtils.toString( rstack.pop.asInstanceOf[FileReader] ) )
    push( rstack.pop.asInstanceOf[String] )
  }

  def filter_chars() : Unit = {
    //  Takes data on the stack and places back a copy with all
    //  nonalphanumeric chars replaced by white space.
    // This is not in style. RE is too high-level, but using it
    // for doing this fast and short. Push the pattern onto stack
    // Push the result onto the stack
    stack.push( stack.pop().asInstanceOf[String].toLowerCase )
    stack.push( stack.pop().asInstanceOf[String].map(
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
    stack.pushAll( stack.pop().asInstanceOf[String].split(' ').filter( ( s : String ) => { s.trim.length > 1 } ))
  }


  def remove_stop_words() = {

    // Takes a list of words on the stack and removes stop words.
    stack.push(
      IOUtils.toString( new FileReader( "../src/main/resources/stop_words.txt" ) )
        .split(',')
        .toList )

    // add single-letter words
    // stack.pushAll( list( string.ascii_lowercase ) )

    heap.put( "stop_words", stack.pop() )

    // # Again, this is too high-level for this style, but using it
    // # for doing this fast and short. Left as exercise.
    heap.put( "words", ListBuffer[String]() )

    while( stack.length > 0 ) {

      if( heap("stop_words" ).asInstanceOf[List[String]].contains( stack(0) ) ) {
        stack.pop() // pop it and drop it
      }
      else {
        heap("words").asInstanceOf[ListBuffer[String]] += stack.pop().asInstanceOf[String] // # pop it, store it
      }
    }

    stack.pushAll( heap.get("words").get.asInstanceOf[ListBuffer[String]] )
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
      if( heap("word_freqs").asInstanceOf[HashMap[String,Int]].contains( stack.top.asInstanceOf[String] ) ) {

        // # Increment the frequency, postfix style: f 1 +
        stack.push(
          heap("word_freqs").asInstanceOf[HashMap[String,Int]](stack.top.asInstanceOf[String]) ) //  push f
        stack.push(1) // push 1
        stack.push( stack.pop().asInstanceOf[Int] + stack.pop().asInstanceOf[Int] ) //  add
      }
      else {
        stack.push(1) // #Push 1 in stack[2]
        // Load the updated freq back onto the heap
      }

      // TODO: cheat
      val t = (stack.pop.asInstanceOf[Int], stack.pop().asInstanceOf[String]).swap

      heap("word_freqs").asInstanceOf[HashMap[String,Int]].put( t._1, t._2 )

    }

    // Push the result onto the stack
    stack.push( heap("word_freqs") )

    heap.remove("word_freqs") // # We don't need this variable anymore
  }

  def sort() = {
    // we have a map of String,Int on the stack
    // sort them and add all to the stack
    // Not in style , left as exercise
    val ts = stack.pop.asInstanceOf[HashMap[String,Int]]
    val ss = ts.toList.sortWith( ( t1 : (String,Int), t2 : (String,Int)) => { t1._2 < t2._2 } )
    stack.pushAll( ss )
  }

  def push( a : Any ) : Unit = {

    if( a.isInstanceOf[Word] ){
      executeWord( a.asInstanceOf[Word])
    }
    else {
      stack.push(a)
    }

  }

  // The main function
  // input 300 ms
  // p&p   17000 ms
  def main( args : Array[String] ) : Unit = {

    var w = Word( "read_file", read_file )
    words.put( w.name, w )

    w = Word( "filter_chars", filter_chars )
    words.put( w.name, w )

    w = Word( "frequencies", frequencies )
    words.put( w.name, w )

    w = Word( "remove_stop_words", remove_stop_words )
    words.put( w.name, w )

    w = Word( "scan", scan )
    words.put( w.name, w )

    w = Word( "sort", sort )
    words.put( w.name, w )

    val start = (new Date()).getTime

    Console.println( "eps2..." )
    // stack.push( "../src/main/resources/input.txt" )
    push( "../src/main/resources/pride-and-prejudice.txt" )
    push( Word( "read_file", read_file ) )
    push( Word( "filter_chars", filter_chars ) )
    push( Word( "scan", scan ) )
    push( Word( "remove_stop_words", remove_stop_words ) )
    push( Word( "frequencies", frequencies ) )
    push( Word( "sort", sort ) )

    push(0)
    // Check stack length against 1, because after we process
    // the last word there will be one item left
    while( stack.top.asInstanceOf[Int] < 25 && stack.length > 1 ) {

      heap.put( "i", stack.pop() )

      val (w, f) = stack.pop().asInstanceOf[(String,Int)]
      Console.println( w +" - " + f )
      stack.push( heap("i"))
      stack.push(1)
      stack.push( stack.pop().asInstanceOf[Int] + stack.pop().asInstanceOf[Int] )
    }

    val end = (new Date()).getTime
    Console.println( "runtime:" + ( end - start ))
  }

}


