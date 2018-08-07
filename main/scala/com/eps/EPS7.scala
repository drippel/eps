package com.eps

import java.util.Date

import scala.collection.mutable.HashMap
import scala.io.Source

object EPS7 {

  // Mileage may vary. If this crashes, make it lower
  // RECURSION_LIMIT = 9500
  // We add a few more, because, contrary to the name,
  // this doesn't just rule recursion: it rules the
  // depth of the call stack
  // sys.setrecursionlimit(RECURSION_LIMIT+10)

  def count( word_list : List[String], stopwords : List[String], wordfreqs : HashMap[String,Int]) : Unit = {
    // What to do with an empty list
    if( word_list.isEmpty ){
      return
    }
    else{
      // The inductive case, what to do with a list of words
      // Process the head word
      val word = word_list(0)
      if( !stopwords.contains(word) ){
        if( wordfreqs.contains(word) ){
          wordfreqs(word) += 1
        }
        else{
          wordfreqs(word) = 1
        }
      }
      //# Process the tail
      count( word_list.tail, stopwords, wordfreqs)
    }
  }

  def wf_print( words : List[(String,Int)] ) : Unit = {

    if( words.isEmpty ){
      return
    }
    else{
      val (w, c) = words.head
      Console.println( w +"-" + c )
      wf_print(words.tail)
    }
  }


  def norm( s : String ) = {
    for( c <- s ) yield {
      if( c.isLetterOrDigit ){
        c.toLower
      }
      else {
        ' '
      }
    }
  }


  def main( args : Array[String] ) = {
    Console.println("eps7...")
    val start = (new Date()).getTime
    val lines = Source.fromFile("../src/main/resources/stop_words.txt").getLines()
    val stop_words = (for( l <- lines ) yield { l.split(',') }).flatten.toList

    val wl = Source.fromFile("../src/main/resources/pride-and-prejudice.txt")
      .getLines()
      .map( _ +"\n" )
      .map( norm(_) )
      .map( _.split(' ' ))
      .flatten.toList
      .filter( _.length > 1 )
      .filter( !stop_words.contains(_) )

    val word_freqs = HashMap[String,Int]()
    // Theoretically, we would just call count(words, word_freqs)
    // Try doing that and see what happens.
    // for i in range(0, len(words), RECURSION_LIMIT):
    // count(words[i:i+RECURSION_LIMIT], stop_words, word_freqs)
    count( wl, stop_words, word_freqs)

    val sorted = word_freqs.toList.sortBy( ( t : (String,Int) )  => { -t._2 ;} ).slice(0,25)
    wf_print( sorted )
    val end = (new Date()).getTime
    Console.println( "time:"+ ( end - start))
  }

}
