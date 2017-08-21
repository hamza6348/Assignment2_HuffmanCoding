/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

import java.io.File
import scala.io.Source
package common{

 object common {
//testing file read
  def main(args: Array[String]) {
      println("Following is the content read:" )

      Source.fromFile("meno.1b.txt" ).foreach { 
         print 
      }
   }

}
}