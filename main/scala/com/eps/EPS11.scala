package com.eps

import java.util.Date

import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.io.Source

object EPS11 {

  class DataStorageManager {

    // Models the contents of the file
    var data : String = ""

    def dispatch( message : List[Any] ) = {
      if( "init".equalsIgnoreCase( message(0).asInstanceOf[String] ) ){
        init( message(1).asInstanceOf[String] )
      }
      else if( "words".equalsIgnoreCase(message(0).asInstanceOf[String] ) ){
        words()
      }
      else {
        Console.println("Message not understood " + message(0).asInstanceOf[String] )
        throw new Exception("Message not understood " + message(0).asInstanceOf[String] )
      }
    }

    def init( path_to_file : String ) = {
      data = {
        Source.fromFile( path_to_file )
          .getLines()
          .map( _ + "\n" ).toList
          .mkString
          .map( ( c: Char ) => { if( c.isLetterOrDigit ){ c.toLower }else{ ' ' } } )
        }
    }

    def words() : List[String] = {
      // Returns the list words in storage
      data.split(' ').toList
    }

  }

  class StopWordManager {

    // Models the stop word filter
    val stop_words = ListBuffer[String]()

    def dispatch( message : List[Any] ) = {

      if( "init".equalsIgnoreCase( message(0).asInstanceOf[String] ) ) {
        init()
      }
      else if( "is_stop_word".equalsIgnoreCase( message(0).asInstanceOf[String] ) ){
        is_stop_word(message(1).asInstanceOf[String])
      }
      else{
        Console.println("Message not understood " + message(0).asInstanceOf[String] )
        throw new Exception("Message not understood " + message(0).asInstanceOf[String] )
      }
    }

    def init() = {
      stop_words.appendAll(
        Source.fromFile("../src/main/resources/stop_words.txt")
          .getLines()
          .map( (s:String) => { s.split(',')} )
          .toList
          .flatten
      )
    }

    def is_stop_word(word : String ) = {
      word.length < 2 || stop_words.contains(word)
    }
  }

  class WordFrequencyManager {
    // Keeps the word frequency data
    val  word_freqs = HashMap[String,Int]()

    def dispatch( message : List[Any] ) = {

      if( "increment_count".equalsIgnoreCase(message(0).asInstanceOf[String] ) ){
        increment_count(message(1).asInstanceOf[String])
      }
      else if( "sorted".equalsIgnoreCase(message(0).asInstanceOf[String])){
        sorted()
      }
      else{
        Console.println("Message not understood " + message(0).asInstanceOf[String] )
        throw new Exception("Message not understood " + message(0) )
      }
    }

    def increment_count( word : String ) = {
      word_freqs(word) = (word_freqs.getOrElse(word,0) + 1 )
    }

    def sorted() = {
      word_freqs.toList.sortBy( -_._2 )
    }
  }

  class WordFrequencyController() {

    def dispatch( message : List[Any] ) = {
      message(0) match {
        case "init" => {
          init(message(1).asInstanceOf[String])
        }
        case "run" => {
          run()
        }
        case _ => {
          Console.println("Message not understood " + message(0).asInstanceOf[String] )
          throw new Exception("Message not understood " + message(0))
        }
      }
    }

    var storage_manager   = new DataStorageManager()
    var stop_word_manager = new StopWordManager()
    var word_freq_manager = new WordFrequencyManager()

    def init( path_to_file : String ) = {
      storage_manager.dispatch( List( "init", path_to_file) )
      stop_word_manager.dispatch( List("init") )
    }

    def run() = {

      for( w <- storage_manager.dispatch( List("words") ).asInstanceOf[List[String]] ) {
        if( !stop_word_manager.dispatch( List("is_stop_word", w ) ).asInstanceOf[Boolean] ){
          word_freq_manager.dispatch(List("increment_count", w))
        }
      }

      val word_freqs = word_freq_manager.dispatch( List("sorted")).asInstanceOf[List[(String,Int)]]
      for( (w, c) <- word_freqs.slice(0,25) ){
        Console.println( w + " - " + c )
      }
    }
  }

  def main( args : Array[String] ) : Unit = {
    Console.println( "eps11..." )
    val st = (new Date()).getTime
    try{
    // The main function
    val wfcontroller = new WordFrequencyController()
      wfcontroller.dispatch( List( "init", "../src/main/resources/pride-and-prejudice.txt" ) )
      wfcontroller.dispatch( List("run" ))
    }
    catch{
      case t : Throwable => { t.printStackTrace() }
    }
    val end = (new Date()).getTime
    Console.println( "time:"+ (end - st) )
  }

}
