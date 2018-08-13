package com.eps

import java.util.Date

import scala.collection.mutable.HashMap
import scala.io.Source

object EPS8a {

  def main( args : Array[String] ) = {
    Console.println("eps8...")
    val start = (new Date()).getTime
    // The main function
    read_file( "../src/main/resources/pride-and-prejudice.txt" )
    val end = (new Date()).getTime
    Console.println("time:" + ( end - start ))
  }

  type GenFunc = ( Any* ) => Unit

  // The functions
  def read_file( path_to_file : String ) :  Unit  = {

    filter_chars(Source.fromFile(path_to_file).toList)

  }

  def filter_chars( str_data : List[Char] ) = {

    normalize( str_data.map( (c:Char) => { if( c.isLetterOrDigit ){c.toLower}else{' ' }} ),
      scan )
  }

  def normalize( str_data : List[Char],
        f : (String,
              (List[String],
                 (List[String],
                   ( HashMap[String,Int],
                     ( List[(String,Int)],
                       () => Unit ) => Unit ) => Unit ) => Unit ) => Unit ) => Unit ) : Unit = {

    // convert a List[Char] to string
    f( str_data.mkString, remove_stop_words )
  }

  def scan(str_data : String,
           f : (List[String],
                 (List[String],
                   ( HashMap[String,Int],
                     ( List[(String,Int)],
                       () => Unit ) => Unit ) => Unit ) => Unit ) => Unit ) : Unit = {
    f(str_data.split(' ').toList, frequencies )
  }

  def remove_stop_words(word_list : List[String],
    f : (List[String], ( HashMap[String,Int], ( List[(String,Int)], () => Unit ) => Unit ) => Unit ) => Unit ) : Unit = {
    val stop_words = Source.fromFile("../src/main/resources/stop_words.txt")
      .getLines()
      .toList
      .map( _.split(',') ).flatten

    // add single-letter words
    val data = word_list.filter( ( w : String) => { !stop_words.contains(w) && w.length > 1 } )
    f( data, sort  )
  }

  def frequencies(word_list : List[String],
                  f : ( HashMap[String,Int],
                        ( List[(String,Int)], () => Unit ) => Unit ) => Unit ) : Unit = {

    val wf = HashMap[String,Int]()
    for( w <- word_list ) {
      if( wf.contains(w) ){
        wf(w) += 1
      }
      else{
        wf(w) = 1
      }
    }

    f(wf, print_text )
  }

  def sort(wf : HashMap[String,Int],
           f : ( List[(String,Int)], () => Unit ) => Unit ) : Unit = {
    f( wf.toList.sortBy( -_._2 ), noop )
  }

  def print_text( word_freqs : List[(String,Int)], f : () => Unit ) = {
    for( t <- word_freqs.slice(0,25) ){
      Console.println( t._1 + "-" + t._2 )
    }
    f()
  }

  def noop() : Unit = {
    Console.println("noop...")
  }

}
