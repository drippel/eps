package com.eps

import java.io.{BufferedReader, File, FileReader, RandomAccessFile}
import java.util.Date

import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks._

object EPS1 {

  def main( args : Array[String] ) : Unit = {

    val start = new Date()

    Console.println( "eps1..." )
    // val s = "../src/main/resources/input.txt"
    val s = "../src/main/resources/pride-and-prejudice.txt"

    var data = ListBuffer[Any]()

    // read the stop words file
    var f = new BufferedReader( new FileReader(new File("../src/main/resources/stop_words.txt")))
    data += f.readLine().split(',')
    f.close

    // data 0 : list of stop words

    data += ""   // data 1 : current line
    data += -1   // data 2 : start of word
    data += 0    // data 3 : current index
    data += false // data 4 : was word found
    data += ""    // data 5 : current word
    data += ""    // data 6 : word, NNNN
    data += 0     // data 7 : frequency

    // open secondary memory
    val wf = new RandomAccessFile( "../src/main/resources/word_freqs.txt", "rw" )

    // open the input file
    f = new BufferedReader( new FileReader(new File( s )))

    // loop over input file lines
    // use scala breakable/break - not really idiomatic scala
    Console.println( "reading..." )
    breakable {
      while( true ) {

        data(1) = f.readLine()

        // if null - done
        if( data(1) == null ) {
          break
        }

        // append a newline
        if( !(data(1).asInstanceOf[String].endsWith( "\n" )) ){
          data(1) = data(1).asInstanceOf[String] + "\n"
        }

        // reset the indices
        data(2) = -1
        data(3) = 0

        // for each character in the line
        for( c <- data(1).asInstanceOf[String] ){

          // if we are not in a word - set start of word
          if( data(2) == -1 ){

            if( c.isLetterOrDigit ){
              data(2) = data(3)
            }

          }
          else {

            if( !c.isLetterOrDigit ){

              // we found the end of a word. process it
              data(4) = false
              data(5) = data(1).asInstanceOf[String].substring(data(2).asInstanceOf[Int],data(3).asInstanceOf[Int]).toLowerCase

              // ignore words
              if( data(5).asInstanceOf[String].length >= 2
                  && !data(0).asInstanceOf[Array[String]].contains(data(5).asInstanceOf[String]) ){

                // lets see if it already exists
                breakable{
                  while( true ){

                    data(6) = wf.readLine()
                    if( data(6) == null ){
                      break
                    }

                    // get the count and the word from the freq list
                    data(7) = data(6).asInstanceOf[String].split(',')(1).trim.toInt
                    data(6) = data(6).asInstanceOf[String].split(',')(0).trim

                    if( data(5).asInstanceOf[String].equals(data(6).asInstanceOf[String])){

                      data(7) = ( data(7).asInstanceOf[Int] + 1 )
                      data(4) = true
                      break

                    }
                  }
                }

                if( !data(4).asInstanceOf[Boolean] ){
                  // write to the end of the file
                  // Console.print( "%20s,%04d\n".format( data(5).asInstanceOf[String], 1 ) )
                  // Console.println( wf.getFilePointer )
                  wf.seek( wf.length() )
                  wf.writeBytes( "%20s,%04d\n".format( data(5).asInstanceOf[String], 1 ) )
                }
                else {
                  // Console.print( "%20s,%04d\n".format( data(5).asInstanceOf[String], data(7).asInstanceOf[Int] ) )
                  // Console.println( wf.getFilePointer )
                  wf.seek( (wf.getFilePointer - 26))
                  wf.writeBytes( "%20s,%04d\n".format( data(5).asInstanceOf[String], data(7).asInstanceOf[Int] ) )
                }

                // go back to the start of the wf file
                wf.seek(0)

              }

              data(2) = -1

            }

          }

          data(3) = ( data(3).asInstanceOf[Int] + 1 )
        }

      }
    }
    // end while

    // close the files
    f.close()

    //
    wf.seek(0)

    // print
    // PART 2
    // Now we need to find the 25 most frequently occuring words.
    // We don't need anything from the previous values in memory
    data = ListBuffer[Any]()

    // Let's use the first 25 entries for the top 25 words
    // = data + [[]]*(25 - len(data))
    data += null
    data += null
    data += null
    data += null
    data += null
    data += null
    data += null
    data += null
    data += null
    data += null
    data += null
    data += null
    data += null
    data += null
    data += null
    data += null
    data += null
    data += null
    data += null
    data += null
    data += null
    data += null
    data += null
    data += null
    data += null

    data += "" // data[25] is word,freq from file
    data += 0  // data[26] is freq

    // Loop over secondary memory file
    Console.println( "sorting..." )
    breakable{

      while( true ) {

        data(25) = wf.readLine()
        // Console.println( data(25) )

        if( data(25) == null ) {
          break
        }

        // Read it as integer
        data(26) = data(25).asInstanceOf[String].split(',')(1).trim.toInt
        // word
        data(25) = data(25).asInstanceOf[String].split(',')(0).trim

        // Check if this word has more counts than the ones in memory
        breakable{
          for( i <- 0 until 25 ) {

            // elimination of symbol i is exercise
            if( data(i) == null ||
              data(i).asInstanceOf[(String, Int)]._2 < data(26).asInstanceOf[Int] ) {

              data.insert(i, (data(25), data(26)))
              data(26) = null
              break
            }
          }
        }
      }
    }

    Console.println("printing...")
    for( tf <- data.slice(0,25) ) {
      // elimination of symbol tf is exercise
      if( tf != null ) {
        Console.println( tf.asInstanceOf[(String,Int)]._1 + " - " + tf.asInstanceOf[(String,Int)]._2 )
      }
    }

    // We're done
    wf.close()

    val end = new Date()
    Console.println( "runtime:"+ (end.getTime - start.getTime))
  }

}
