package com.eps

import java.util.Date

import scala.collection.mutable.HashMap
import scala.io.Source

object EPS9 {

  // The One class for this example
  class TFTheOne( var value : Any ) {

  def bind( func : (Any) => Any ) = {
    value = func(value)
    this
  }

  def printme() = {
    Console.println(value)
  }

  }

  // The functions
  def read_file( path_to_file : Any ) : Any = {
    val s = path_to_file.asInstanceOf[String]
    // read in the file
    val lines = Source.fromFile(s).getLines().map( ( _ + "\n" ))
    lines.mkString
  }

  def filter_chars( str_data : Any ) : Any = {
    val s = str_data.asInstanceOf[String]
    val t = s.map( ( c : Char ) => { if( c.isLetterOrDigit ){ c.toLower }else{ ' ' } } )
    t.mkString
  }

  def normalize( str_data : Any ) : Any = {
    str_data.asInstanceOf[String].toLowerCase()
  }

  def scan( str_data : Any ) : Any = {
    str_data.asInstanceOf[String].split(' ').toList
  }

  def remove_stop_words( wl : Any ) : Any = {

    val word_list = wl.asInstanceOf[List[String]]

    val stop_words = Source.fromFile("../src/main/resources/stop_words.txt")
      .getLines()
      .map( (s:String) => { s.split(',')} )
      .toList
      .flatten

      word_list.filter( (w:String) => { w.length > 1 && !stop_words.contains(w)} )
  }

  def frequencies( wl : Any ) : Any = {

    val word_list = wl.asInstanceOf[List[String]]

    val word_freqs = new HashMap[String,Int]()

    for( w <- word_list ){
      if( word_freqs.contains(w) ){
        word_freqs(w) += 1
      }
      else {
        word_freqs(w) = 1
      }
    }

    word_freqs
  }

  def sort( wl : Any ) : Any = {
    val word_freq = wl.asInstanceOf[HashMap[String,Int]]
    word_freq.toList.sortBy( -_._2 ).toList
  }

  def top25_freqs( wl : Any ) : Any = {
    val word_freqs = wl.asInstanceOf[List[(String,Int)]]

    var top25 = for( tf <- word_freqs.slice(0,25) ) yield {
      tf._1 + " - " + tf._2
    }

    top25.mkString( "\n" )

  }

  def main( args : Array[String] ) = {
  // The main function
    val st = (new Date()).getTime
  new TFTheOne( "../src/main/resources/pride-and-prejudice.txt" )
    .bind(read_file)
    .bind(filter_chars)
    .bind(normalize)
    .bind(scan)
    .bind(remove_stop_words)
    .bind(frequencies)
    .bind(sort)
    .bind(top25_freqs)
    .printme()

    val end = (new Date()).getTime
    Console.println("time:" + ( end - st ))
  }


}
