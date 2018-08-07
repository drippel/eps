package com.eps

import scala.io.Source

object EPS6 {

  def main( args : Array[String] ) : Unit = {
    Console.println("eps6...")
    def toLines( f : String ) : List[String] = { Source.fromFile(f).getLines().toList }
    def toWords( lines : List[String], c : Char ) = {
      for( l <- lines; nl <- l.split( c ).toList )
      yield{ nl }
    }
    def norms( s : String ) : String = { s.map( ( c : Char ) => { normc( c ) } ) }
    def normc( c : Char ) = { if( c.isLetterOrDigit ){ c.toLower }else{ ' ' } }
    val stops = toWords( toLines( "../src/main/resources/stop_words.txt" ), ',' )
    val book = toWords( toLines( "../src/main/resources/pride-and-prejudice.txt" ).map(norms(_))
      , ' ' ).filter( (w:String) => { !stops.contains(w) && w.length > 1})
    val ubook = book.toSet
    val counts = ubook.map( (w:String) => { (w,book.count(w.equals(_))) } )
    val top = counts.toList.sortBy( ( a : (String,Int) ) => { a._2 } ).reverse.slice(0,25)
    top.foreach( Console.println(_) )
  }

}
