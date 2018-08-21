package com.eps

import scala.collection.mutable.{HashMap, ListBuffer}

object Inspector {

  case class Mapping( val path : String, val method : String, val handler : String )

  class HttpMethod()

  class HttpRequest(){
    val path = ""
    val parameters = "" // map
    val headers = ""    // map
    val cookies = ""    // map
  }

  // always a text block
  class HttpResponse()

  abstract class Controller( val baseURL : String  ) {

    def mappings() : List[Mapping]

  }

  class OrderController extends Controller( "/order" ) {

    def mappings() = {
      val mps = ListBuffer[Mapping]()
      mps += Mapping( "/", "POST", "doPost" )
      mps.toList
    }

    def doGet( orderId : String ) = {
      Console.println("doPost")
    }
  }


  def main( args : Array[String] ) = {
    Console.println( "inspect" )

    val inst = new OrderController()
    val c = inst.getClass

  }

}
