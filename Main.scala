import scala.collection.mutable.ListBuffer

/**
  * Created by Fraser on 14/06/2017.
  */

object Main {

  def doubleChars(text: String): String = {
    var doubleText: String = ""
    for(i<-0 until text.length){
      doubleText += text(i)
      doubleText += text(i)
    }
    doubleText
  }

  def getSandwich(sandwich: String): String = {
    def iter(sandwich: String, index: Int, slicesOfBread: Int, filling: String, currentText: String): String = index match{
      case a if currentText == "bread" => iter(sandwich, index, slicesOfBread+1, filling, "")
      case a if index == sandwich.length => if (slicesOfBread == 2) filling else ""
      case a if slicesOfBread == 0 && sandwich(index) == 'b' => iter(sandwich, index+1, slicesOfBread, filling, sandwich(index).toString)
      case a if slicesOfBread == 1 && sandwich(index) == 'b' => iter(sandwich, index+1, slicesOfBread, currentText, sandwich(index).toString)
      case _ => iter(sandwich, index+1, slicesOfBread, filling, currentText+sandwich(index))
    }
    iter(sandwich, 0, 0, "", "")
  }

  def evenlySpaced(a: Int, b: Int, c: Int): Boolean ={
    a match {
      case a if a < b && b < c => if(b-a == c-b) true else false
      case a if a < c && c < b => if(c-a == b-c) true else false
      case a if b < a && a < c => if(a-b == c-a) true else false
      case a if b < c && c < a => if(c-b == a-c) true else false
      case a if c < a && a < b => if(a-c == b-a) true else false
      case a if c < b && b < a => if(b-c == a-b) true else false
    }
  }

  def fibonacci(sequenceNumber: Int): Int ={
    def iter(index: Int, sequenceNumber: Int, values: ListBuffer[Int]): Int = index match{
      case 0 => iter(index+1, sequenceNumber, values+=0)
      case a if index > sequenceNumber => values(index-1)
      case 1 => iter(index+1, sequenceNumber, values+=1)
      case _ => iter(index+1, sequenceNumber, values+=(values(index-1) + values(index-2)))
    }
    iter(0, sequenceNumber, new ListBuffer[Int])
  }

  def bunnyEars(noOfBunnies: Int): Int ={
    def iter(index: Int, noOfBunnies: Int, earTotal: Int): Int = noOfBunnies match{
      case 0 => 0
      case a if index == noOfBunnies => earTotal
      case _ => iter(index+1, noOfBunnies, earTotal+2)
    }
    iter(0, noOfBunnies, 0)
  }

  def nTwice(text: String, n: Int): String ={
    var cropped :String = ""
    //validates numbers
    if(n < text.length){
      cropped = text.substring(0, n)
      cropped += text.takeRight(n)
    }
    cropped
  }

  def endsLy(text: String): Boolean ={
      if(text.endsWith("ly")) true else false
  }

  def stringClean(text: String): String = {
    def iter(index: Int, text: String, lastChar: Char, newString: String): String = index match{
      case a if index == text.length => newString
      case 0 => iter(index+1, text, text(index), newString+text(index))
      case a if text(index) != lastChar => iter(index+1, text, text(index), newString+text(index))
      case _ => iter(index+1, text, lastChar, newString)
    }
    iter(0, text, ' ', "")
  }

  def filledDiamond(size: Int): Unit ={
    def drawLine(size: Int, space: Int): String = {
      val filled: Int = size-(space*2)//work out how much of the diamond is solid
      var line: String = ""//create the string for the line

      //loop for the entire row
      for(i<-0 until size){
        //if next char is a blank space
        if(i < space || i>= space+filled){
          line+=" "//add space
        } else {
          line+="a"//add char
        }
      }
      line+="\n"
      line
    }

    def iter(index: Int, diamond: String, blankSpace: Int, size: Int, decrease: Boolean): String = index match{
      case a if size == index => diamond//made full diamond, exit
      case 0 => var space = (size-1)/2; iter(index+1, diamond+drawLine(size, space), space-1, size, true)//for the first one work out where the middle is
      case a if blankSpace == 0 => iter(index+1, diamond+drawLine(size, blankSpace), blankSpace+1, size, false)//if in the middle, start adding blank space again
      case a if decrease == true => iter(index+1, diamond+drawLine(size, blankSpace), blankSpace-1, size, true)//if top half remove blank space for next line
      case a if decrease == false => iter(index+1, diamond+drawLine(size, blankSpace), blankSpace+1, size, false)//if on the bottom half add blank space for next line
    }

    //ensure that the size of the diamond is odd
    if(size%2 == 0){
      println(iter(0, "", 0, size+1, true))
    } else{
      println(iter(0, "", 0, size, true))
    }
  }

  def hollowDiamond(size: Int): Unit ={
    def drawLine(size: Int, space: Int): String = {
      val emptySpace = (size-(space*2))-2 //calculate how much empty space in the diamond

      var line: String = "" //start the string

      for(i<-0 until space){ line += " " } //for the left side add the gaps

      line+="a" //add the first char

      for(i<-0 until emptySpace){line += " "}//add space for the centre of the diamond

      if(size-(space*2)>1) line+="a"//if it has two sides add the second char

      for(i<-0 until space){ line += " " }//for the right side add the gaps

      line+="\n"//end the line
      line//return the string
    }

    //iterator
    def iter(index: Int, diamond: String, blankSpace: Int, size: Int, decrease: Boolean): String = index match{
      case a if size == index => diamond//made full diamond, exit
      case 0 => var space = (size-1)/2; iter(index+1, diamond+drawLine(size, space), space-1, size, true)//for the first one work out where the middle is
      case a if blankSpace == 0 => iter(index+1, diamond+drawLine(size, blankSpace), blankSpace+1, size, false)//if in the middle, start adding blank space again
      case a if decrease == true => iter(index+1, diamond+drawLine(size, blankSpace), blankSpace-1, size, true)//if top half remove blank space for next line
      case a if decrease == false => iter(index+1, diamond+drawLine(size, blankSpace), blankSpace+1, size, false)//if on the bottom half add blank space for next line
    }

    //if its an even number add 1 to make it odd, keeps the program working correctly
    if(size%2 == 0){
      println(iter(0, "", 0, size+1, true))
    } else{
      println(iter(0, "", 0, size, true))
    }
  }

  def main(args:Array[String]): Unit = {
    println("\nTask 1")
    println(doubleChars("The"))
    println(doubleChars("AAbb"))
    println(doubleChars("Hi-There"))

    println("\nTask 2")
    println(getSandwich("breadjambread"))
    println(getSandwich("xxbreadjambreadyy"))
    println(getSandwich("xxbreadyy"))

    println("\nTask 3")
    println(evenlySpaced(2, 4, 6))
    println(evenlySpaced(4, 6, 2))
    println(evenlySpaced(4, 6, 3))

    println("\nTask 4")
    println(fibonacci(0))
    println(fibonacci(1))
    println(fibonacci(2))

    println("\nTask 5")
    println(bunnyEars(0))
    println(bunnyEars(1))
    println(bunnyEars(2))

    println("\nTask 6")
    println(nTwice("Hello", 2))
    println(nTwice("Chocolate", 3))
    println(nTwice("Chocolate", 1))

    println("\nTask 7")
    println(endsLy("oddly"))
    println(endsLy("y"))
    println(endsLy("oddy"))

    println("\nTask 8")
    println(stringClean("yyzzza"))
    println(stringClean("abbbcdd"))
    println(stringClean("Hello"))

    println("\nTask 9")
    filledDiamond(5)

    println("\nTask 10")
    hollowDiamond(10)
  }
}