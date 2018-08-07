package com.eps

import java.io.{BufferedReader, FileReader}
import java.util.Date

import scala.collection.mutable.{HashMap, ListBuffer}

object EPS5 {

  // The functions
  def read_file( path_to_file : String ) = {
    // Takes a path to a file and returns the entire contents of the file as a string
    val raw = new StringBuffer()
    val br = new BufferedReader( new FileReader( path_to_file) )
    br.lines().forEach( ( s : String ) => { raw.append((s + "\n")) } )
    raw.toString
  }

  def filter_chars_and_normalize( str_data : String ) : String = {
    // Takes a string and returns a copy with all nonalphanumeric chars replaced by white space
    str_data.map( (c : Char) => { if( c.isLetterOrDigit ){ c.toLower }else{ ' ' } } )
  }

  def scan(str_data : String ) : List[String] = {
    // Takes a string and scans for words, returning a list of words.
    return str_data.split( ' ' ).toList
  }

  def remove_stop_words(word_list : List[String] ) : List[String] = {

    // Takes a list of words and returns a copy with all stop words removed
    val stop_words = ListBuffer[String]()
    val br = new BufferedReader( new FileReader( "../src/main/resources/stop_words.txt" ) )

    br.lines().forEach( ( l : String ) => { stop_words.appendAll( l.split(','))})

    for( word <- word_list if( !stop_words.contains(word) && word.length > 1 )) yield ( word )
  }

  def frequencies( word_list : List[String] ) = {

    // Takes a list of words and returns a dictionary associating words with frequencies of occurrence
    val word_freqs = HashMap[String,Int]()

    for( w <- word_list ) {
      if( word_freqs.contains(w) ){
        word_freqs.update(w, (word_freqs(w) + 1) )
      }
      else{
        word_freqs(w) = 1
      }
    }

    word_freqs
  }

  def sort(word_freq : HashMap[String,Int] ) : List[(String,Int)] = {
    // Takes a dictionary of words and their frequencies
    // and returns a list of pairs where the entries are
    // sorted by frequency
    word_freq.toList.sortBy( ( a : (String,Int) ) => { a._2 } ).reverse
  }

  def print_all( word_freqs : List[(String,Int)]) : Unit = {
    // Takes a list of pairs where the entries are sorted by frequency and print them recursively.
    if( word_freqs.length > 0 ){
      Console.println( word_freqs(0)._1 +" - "+ word_freqs(0)._2 )
      print_all( word_freqs.tail )
    }
  }

  def main( args : Array[String] ) = {
    Console.println("eps5...")
    val start = (new Date()).getTime
    // print_all(sort(frequencies(remove_stop_words(scan(filter_chars_and_normalize(read_file("../src/main/resources/input.txt")))))).slice(0,25))
    print_all(sort(frequencies(remove_stop_words(scan(filter_chars_and_normalize(read_file("../src/main/resources/pride-and-prejudice.txt" )))))).slice(0,25))
    val end = (new Date()).getTime
    Console.println( "time:"+ ( end - start))
  }

}
