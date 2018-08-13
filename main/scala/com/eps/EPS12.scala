package com.eps

import java.util
import java.util.Date

import scala.collection.mutable.{ArrayBuffer, HashMap}
import scala.io.Source

object EPS12 {

  // Auxiliary functions that can't be lambdas
  def extract_words(obj : HashMap[String,Any], path_to_file : String ) = {
    obj("data") = {
        Source.fromFile( path_to_file )
          .getLines()
          .map( _ + "\n" ).toList
          .mkString
          .map( ( c: Char ) => { if( c.isLetterOrDigit ){ c.toLower }else{ ' ' } } )
        }

  }

  def load_stop_words(obj : HashMap[String,Any] ) : Any = {
    obj("stop_words") = Source.fromFile("../src/main/resources/stop_words.txt")
        .getLines()
        .map( (s:String) => { s.split(',')} )
        .toList
        .flatten
  }

  def increment_count(obj : HashMap[String,Any], w : String ) = {
    obj("freqs").asInstanceOf[HashMap[String,Int]](w) = ( obj("freqs").asInstanceOf[HashMap[String,Int]].getOrElse(w,0) + 1 )
  }

  val initWords : ( String ) => Unit = { extract_words( data_storage_obj, _ ) }
  val getWords = () => {
    data_storage_obj("data").asInstanceOf[String].split(' ').toList
  }

  val data_storage_obj = new HashMap[String,Any]()
  data_storage_obj += ( "data" ->  "" )
  data_storage_obj += ( "init" -> initWords )
  data_storage_obj += ( "words" -> getWords )


  val initStopWords = () => { load_stop_words(stop_words_obj) }
  val isStopWord : (String) => Boolean = { stop_words_obj("stop_words").asInstanceOf[List[String]].contains(_) }

  val stop_words_obj = new HashMap[String,Any]()
  stop_words_obj += ( "stop_words"   -> new ArrayBuffer[String]() )
  stop_words_obj += ( "init"         -> initStopWords )
  stop_words_obj += ( "is_stop_word" -> isStopWord )

  val inc       : (String) => Unit               = { increment_count(word_freqs_obj, _ ) }

  val getSorted = () => { word_freqs_obj("freqs").asInstanceOf[HashMap[String,Int]].toList.sortBy( -_._2 ) }

  val word_freqs_obj = new HashMap[String,Any]()
  word_freqs_obj += ( "freqs" -> new HashMap[String,Int]() )
  word_freqs_obj += ( "increment_count" -> inc )
  word_freqs_obj += ( "sorted" ->  getSorted )




  def main( args : Array[String] ) : Unit = {
    Console.println( "eps12...")
    val st = (new Date()).getTime
    data_storage_obj("init").asInstanceOf[(String) => Unit]( "../src/main/resources/pride-and-prejudice.txt" )
    stop_words_obj("init").asInstanceOf[ () => Any]()

    for( w <- data_storage_obj("words").asInstanceOf[() => List[String]]() ) {
      if( w.length() > 1 && !stop_words_obj("is_stop_word").asInstanceOf[(String) => Boolean](w) ){
        word_freqs_obj("increment_count").asInstanceOf[(String) => Unit](w)
      }
    }

    val word_freqs = word_freqs_obj("sorted").asInstanceOf[() => List[(String,Int)]]()
    for( (w, c) <- word_freqs.slice(0,25) ){
      Console.println( w +" - "+ c )
    }
    val end = (new Date()).getTime
    Console.println( "time:"+ (end - st) )
  }

}
