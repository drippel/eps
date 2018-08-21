package com.eps

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.Source

object EPS15 {

  // # The event management substrate
  class EventManager {

    val subscriptions = mutable.HashMap[String, ListBuffer[(List[Any]) => Unit]]()

    def subscribe(event_type : String, handler : (List[Any]) => Unit) = {

      if( subscriptions.contains(event_type) ) {
        subscriptions(event_type) += handler
      }
      else {
        subscriptions(event_type) = new ListBuffer[(List[Any]) => Unit]()
        subscriptions(event_type) += handler
      }
    }

    def publish(event : List[Any]) = {
      val event_type = event(0).asInstanceOf[String]
      if( subscriptions.contains(event_type) ) {
        for( h <- subscriptions(event_type) ) {
          h(event)
        }
      }
    }

  }

  //  The application entities
  class DataStorage(val event_manager : EventManager) {

    var data = ""

    event_manager.subscribe("load", load)
    event_manager.subscribe("start", produce_words)

    def load(event : List[Any]) : Unit = {
      data = Source.fromFile(event(1).asInstanceOf[String])
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

    def produce_words(event : List[Any]) : Unit = {
      for( w <- data.split(' ') ) {
        event_manager.publish(List("word", w))
      }

      event_manager.publish(List("eof", None))
    }
  }

  class StopWordFilter(val event_manager : EventManager) {

    var stop_words = List[String]()

    // """ Models the stop word filter """
    event_manager.subscribe("load", load)
    event_manager.subscribe("word", is_stop_word)

    def load(event : List[Any]) : Unit = {
      stop_words = Source.fromFile("../src/main/resources/stop_words.txt")
        .getLines()
        .map((s : String) => {
          s.split(',')
        })
        .toList
        .flatten
    }

    def is_stop_word(event : List[Any]) : Unit = {
      val word = event(1).asInstanceOf[String]
      if( word.length > 1 && !stop_words.contains(word) ) {
        event_manager.publish(List("valid_word", word))
      }
    }
  }

  class WordFrequencyCounter(event_manager : EventManager) {

    val word_freqs = mutable.HashMap[String, Int]()

    // """ Keeps the word frequency data """
    event_manager.subscribe("valid_word", increment_count)
    event_manager.subscribe("print", print_freqs)

    def increment_count(event : List[Any]) : Unit = {
      val word = event(1).asInstanceOf[String]
      word_freqs(word) = (word_freqs.getOrElse(word, 0) + 1)
    }

    def print_freqs(event : List[Any]) : Unit = {
      val wfs = word_freqs.toList.sortBy(-_._2)
      for( (w, c) <- wfs.slice(0, 25) ) {
        Console.println(w + " - " + c)
      }
    }

  }

  class WordFrequencyApplication(val event_manager : EventManager) {

    event_manager.subscribe("run", run)
    event_manager.subscribe("eof", stop)

    def run(event : List[Any]) : Unit = {
      val path_to_file = event(1).asInstanceOf[String]
      event_manager.publish(List("load", path_to_file))
      event_manager.publish(List("start", None))
    }

    def stop(event : List[Any]) : Unit = {
      event_manager.publish(List("print", None))
    }

  }


  def main(args : Array[String]) = {
    Console.println("eps15...")
    //  The main function
    val em = new EventManager()
    val ds = new DataStorage(em)
    val swf = new StopWordFilter(em)
    val wfc = new WordFrequencyCounter(em)
    new WordFrequencyApplication(em)
    em.publish(List("run", "../src/main/resources/pride-and-prejudice.txt"))
  }

}
