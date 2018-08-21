package com.eps

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object EPS14 {

  // # The "I'll call you back" Word Frequency Framework
  class WordFrequencyFramework {

    val load_event_handlers = ArrayBuffer[(String) => Unit]()
    val dowork_event_handlers = ArrayBuffer[() => Unit]()
    val end_event_handlers = ArrayBuffer[() => Unit]()

    def register_for_load_event(handler : (String) => Unit) = {
      load_event_handlers += handler
    }

    def register_for_dowork_event(handler : () => Unit) {
      dowork_event_handlers += handler
    }

    def register_for_end_event(handler : () => Unit) {
      end_event_handlers += handler
    }

    def run(path_to_file : String) = {

      for( h <- load_event_handlers ) {
        h(path_to_file)
      }

      for( h <- dowork_event_handlers ) {
        h()
      }

      for( h <- end_event_handlers ) {
        h()
      }

    }

  }

  // The entities of the application
  class DataStorage(val wfapp : WordFrequencyFramework, val stop_word_filter : StopWordFilter) {

    // """ Models the contents of the file """
    var data = ""
    val word_event_handlers = ArrayBuffer[(String) => Unit]()

    def load(path_to_file : String) = {
      data = Source.fromFile(path_to_file)
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

    def produce_words() = {
      """ Iterates through the list words in storage calling back handlers for words """
      for( w <- data.split(' ') ) {
        if( !stop_word_filter.is_stop_word(w) ) {
          for( h <- word_event_handlers ) {
            h(w)
          }
        }
      }
    }

    def register_for_word_event(handler : (String) => Unit) = {
      word_event_handlers.append(handler)
    }

    wfapp.register_for_load_event(load)
    wfapp.register_for_dowork_event(produce_words)
  }

  class StopWordFilter(val wfapp : WordFrequencyFramework) {

    // """ Models the stop word filter """
    var stop_words = List[String]()

    wfapp.register_for_load_event(load)

    def load(ignore : Any) = {
      stop_words = Source.fromFile("../src/main/resources/stop_words.txt")
        .getLines()
        .map((s : String) => {
          s.split(',')
        })
        .toList
        .flatten
    }

    def is_stop_word(word : String) : Boolean = {
      word.length < 2 || stop_words.contains(word)
    }
  }

  class WordFrequencyCounter(wfapp : WordFrequencyFramework, data_storage : DataStorage) {

    // """ Keeps the word frequency data """
    val word_freqs = mutable.HashMap[String, Int]()

    data_storage.register_for_word_event(increment_count)
    wfapp.register_for_end_event(print_freqs)

    def increment_count(word : String) = {
      word_freqs(word) = (word_freqs.getOrElse(word, 0) + 1)
    }

    def print_freqs() = {
      val wf = word_freqs.toList.sortBy(-_._2)
      for( (w, c) <- wf.slice(0, 25) ) {
        Console.println(w + " - " + c)
      }
    }
  }

  // # The main function


  def main(args : Array[String]) = {
    Console.println("eps14...")
    val wfapp = new WordFrequencyFramework()
    val stop_word_filter = new StopWordFilter(wfapp)
    val data_storage = new DataStorage(wfapp, stop_word_filter)
    val word_freq_counter = new WordFrequencyCounter(wfapp, data_storage)
    wfapp.run("../src/main/resources/pride-and-prejudice.txt")
  }

}
