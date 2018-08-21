package com.eps

import java.util.Date

import scala.collection.mutable.HashMap
import scala.io.Source

object EPS13 {

  // The abstract things

  // """ Models the contents of the file """
  abstract class IDataStorage {
    def words() : List[String]
  }

  // """ Models the stop word filter """
  abstract class IStopWordFilter {
    def is_stop_word(word : String) : Boolean
  }

  // """ Keeps the word frequency data """
  abstract class IWordFrequencyCounter {

    def increment_count(word : String)

    // """ Returns the words and their frequencies, sorted by frequency"""
    def sorted() : List[(String, Int)]
  }

  // The concrete things

  class DataStorageManager(path_to_file : String) extends IDataStorage {

    val data = {
      Source.fromFile(path_to_file)
        .getLines()
        .map(_ + "\n").toList
        .mkString
        .map((c : Char) => {
          if( c.isLetterOrDigit ) {
            c.toLower
          } else {
            ' '
          }
        })
    }

    def words() = { data.split(' ').toList }

  }

  class StopWordManager extends IStopWordFilter {

    val stop_words = {
      Source.fromFile("../src/main/resources/stop_words.txt")
        .getLines()
        .map((s : String) => {
          s.split(',')
        })
        .toList
        .flatten
    }

    def is_stop_word(word : String) : Boolean = {
      stop_words.contains(word)
    }
  }

  class WordFrequencyManager extends IWordFrequencyCounter {

    val word_freqs = HashMap[String, Int]()

    def increment_count(word : String) = {
      word_freqs(word) = (word_freqs.getOrElse(word,0) + 1 )
    }

    def sorted() = { word_freqs.toList.sortBy(-_._2) }
  }


  // The application object
  class WordFrequencyController(val path_to_file : String) {

    val storage = new DataStorageManager(path_to_file)
    val stop_word_manager = new StopWordManager()
    val word_freq_counter = new WordFrequencyManager()

    def run() = {
      for( w <- storage.words() ) {
        if( w.length > 1 && !stop_word_manager.is_stop_word(w) ) {
          word_freq_counter.increment_count(w)
        }
      }

      val word_freqs = word_freq_counter.sorted()
      for( (w, c) <- word_freqs.slice(0, 25) ) {
        Console.println(w + " - " + c)
      }
    }

  }

  //  The main function

  def main(args : Array[String]) = {
    Console.println("eps13...")
    val st = (new Date()).getTime
    new WordFrequencyController("../src/main/resources/pride-and-prejudice.txt").run()
    val end = (new Date()).getTime
    Console.println( "time:"+ (end - st) )
  }

}
