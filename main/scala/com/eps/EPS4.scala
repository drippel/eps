package com.eps

import java.io.{BufferedReader, FileReader}
import java.util.Date

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap

object EPS4 {


  // The shared mutable data
  val data = ListBuffer[Char]()
  val filtered = ListBuffer[Char]()
  val words = ListBuffer[String]()
  val removed = ListBuffer[String]()
  val word_freqs = HashMap[String,Int]()
  val sorted = ListBuffer[(String,Int)]()

  // The procedures
  def read_file( path_to_file : String ) = {
    // Takes a path to a file and assigns the entire
    // contents of the file to the global variable data
    val b = new BufferedReader( new FileReader( path_to_file ) )
    b.lines().forEach( (s : String) => { data.appendAll( (s + "\n" ) ) } )

  }

  def filter_chars_and_normalize() = {
    // Replaces all nonalphanumeric chars in data with white space
    for( c <- data ){
      if( !c.isLetterOrDigit ){
        filtered.append( ' ' )
      }
      else {
        filtered.append(c.toLower)
      }
    }
  }

  def scan() = {
    // Scans data for words, filling the global variable words
    val data_str = filtered.mkString
    words.appendAll( data_str.split( ' ' ) )

  }

  def remove_stop_words() = {

    val br = new BufferedReader( new FileReader("../src/main/resources/stop_words.txt") )
    val raw = new StringBuffer()
    br.lines().forEach( ( s : String ) => { raw.append(s)} )

    val stop_words = raw.toString.split(',')

    // add single-letter words

    val indexes = ListBuffer[Int]()

    for( word <- words ){
      if( !stop_words.contains( word ) && word.length >= 2 ){
        removed += word
      }
    }

  }

  def frequencies() = {

    // Creates a list of pairs associating
    // words with frequencies
    for( w <- removed ) {

        if( word_freqs.contains( w ) ){
            word_freqs.put( w, word_freqs(w) + 1 )
        }
        else{
            word_freqs.put( w, 1 )
        }
     }
  }

  def sort() = {
    // Sorts word_freqs by frequency
    sorted ++= word_freqs.toList.sortBy( (a : (String,Int) ) => { a._2 } )
  }

  def print() = {

    for( tf <- sorted.reverse.slice(0,25) ){
      Console.println( tf._1 + " - " + tf._2 )
    }
  }



  def main( args : Array[String] ) : Unit = {
    try{
      Console.println( "eps4..." )
      val start = (new Date()).getTime
      // The main function
      // read_file( "../src/main/resources/input.txt" )
      Console.println( "read file..." )
      read_file( "../src/main/resources/pride-and-prejudice.txt" )
      Console.println( data.length )
      Console.println( "filter..." )
      filter_chars_and_normalize()
      Console.println( "scan..." )
      scan()
      Console.println( "stop words..." )
      remove_stop_words()
      Console.println( "freq..." )
      frequencies()
      Console.println( "sort..." )
      sort()
      Console.println( "print..." )
      print()
      val end = (new Date()).getTime
      Console.println( "time:"+ ( end - start))
    }
    catch {
      case t : Throwable => {
        t.printStackTrace()
      }
    }
  }
}
