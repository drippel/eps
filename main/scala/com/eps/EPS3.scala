package com.eps

import java.io.FileInputStream

import scala.collection.mutable.{HashMap, ListBuffer}
import scala.io.BufferedSource
import scala.util.control.Breaks._

object EPS3 {

  def main( args : Array[String] ) = {
    Console.println("eps3...")
    process()
  }

  def process(): Unit = {

    val word_freqs = ListBuffer[(String,Int)]()

    val stop_words = ListBuffer[String]()
    val bs = new BufferedSource( new FileInputStream( "../src/main/resources/stop_words.txt" )).getLines()

    for( l <- bs ){
      stop_words.appendAll( l.split(","))
    }

    // stop_words.foreach( Console.println(_))

    // # iterate through the file one line at a time
    // val is = new BufferedSource( new FileInputStream( "../src/main/resources/input.txt" )).getLines()
    val is = new BufferedSource( new FileInputStream( "../src/main/resources/pride-and-prejudice.txt" )).getLines()
    for( line <- is ) {

      var start_char = -1
      var i = 0

      val fline = line + "\n"

      // loop through each character in the line
      for( c <- fline ) {
        if( start_char == -1 ){
            if( c.isLetterOrDigit ){
                // We found the start of a word
                start_char = i
            }
        }
        else {

            if( !c.isLetterOrDigit ) {
                // We found the end of a word. Process it
                var found = false
                var word = fline.substring(start_char,i).toLowerCase()

                // Ignore stop words and words less than 2 chars
                if( !stop_words.contains(word) && word.length > 1 ){

                    var pair_index = 0

                    breakable{

                      // Let's see if it already exists in word_freq
                      for( pair <- word_freqs ) {

                          if( word.equalsIgnoreCase(pair._1) ){
                            word_freqs(pair_index) = (pair._1, pair._2 + 1)
                            found = true
                            break
                          }
                          pair_index += 1
                      }
                    }

                    if( !found ) {
                        Console.println( "adding:"+ word)
                        val t = (word, 1)
                        word_freqs += t
                    }
                    else if( word_freqs.size > 1 ) {

                      // Console.println( "sorting:"+ word_freqs(pair_index)._1 + " -"+ word_freqs(pair_index)._2 )

                        // We may need to reorder
                        for( n <- pair_index to 0 by -1 ) {

                            if( word_freqs(pair_index)._2 > word_freqs(n)._2 ){
                                // swap
                                val t = word_freqs(pair_index)
                                word_freqs(pair_index) = word_freqs(n)
                                word_freqs(n) = t
                                pair_index = n
                            }
                        }
                    }

                }
                // Let's reset
                start_char = -1
            }
          }
          i = i + 1
        }
      }

    val end = if( word_freqs.length >= 25 ){ 25 }
    else { word_freqs.length }

      for( i <- 0 until end ) {
        Console.println( word_freqs(i)._1 + " - " + word_freqs(i)._2 )
      }


  }

}
