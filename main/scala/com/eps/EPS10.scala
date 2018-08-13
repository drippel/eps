package com.eps

import java.util.Date

import scala.collection.mutable.HashMap
import scala.io.Source

object EPS10 {

  // The classes
  class TFExercise(){
    def info() ={ this.getClass.getName }
  }

  class DataStorageManager( val path_to_file : String ) extends TFExercise {

    // Models the contents of the file
    val data : String =
    {
      Source.fromFile( path_to_file )
        .getLines()
        .map( _ + "\n" ).toList
        .mkString
        .map( ( c: Char ) => { if( c.isLetterOrDigit ){ c.toLower }else{ ' ' } } )

    }

    def words() = {
      // Returns the list words in storage """
      data.split(' ')
    }

    override def info() = {
      super.info() + ": My major data structure is a " + data.getClass.getName
    }
  }

  class StopWordManager extends TFExercise{
    // Models the stop word filter

    val stop_words : List[String] = {
      Source.fromFile("../src/main/resources/stop_words.txt")
        .getLines()
        .map( (s:String) => { s.split(',')} )
        .toList
        .flatten
    }

    def is_stop_word(word : String ) : Boolean = { word.length < 2 || stop_words.contains(word) }

    override def info() = {
      super.info() + ": My major data structure is a " + stop_words.getClass.getName
    }
  }

  class WordFrequencyManager extends TFExercise{
    // Keeps the word frequency data

    val word_freqs = HashMap[String,Int]()

    def increment_count(word : String ) = {
      word_freqs(word) = (word_freqs.getOrElse(word,0) + 1 )
    }

    def sorted() = { word_freqs.toList.sortBy( - _._2 ) }

    override def info() = {
      super.info() + ": My major data structure is a " + word_freqs.getClass.getName
    }
  }

  class WordFrequencyController( path_to_file : String ) extends TFExercise {

  val storage_manager = new DataStorageManager(path_to_file)
  val stop_word_manager = new StopWordManager()
  val word_freq_manager = new WordFrequencyManager()

  def run() = {

    for( w <- storage_manager.words() ){
      if( !stop_word_manager.is_stop_word(w) ){
        word_freq_manager.increment_count(w)
      }
    }

    val word_freqs = word_freq_manager.sorted()
    for( (w,c) <- word_freqs.slice(0,25) ){
      Console.println( w +" - "+ c )
    }
    }
  }

  // # The main function
  def main( args : Array[String] ) = {
    val st = (new Date()).getTime
    new WordFrequencyController("../src/main/resources/pride-and-prejudice.txt").run()
    val end = (new Date()).getTime
    Console.println( "time:"+ (end - st) )
  }


}
