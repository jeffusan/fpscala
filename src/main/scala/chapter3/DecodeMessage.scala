package decode

case class Decoder(i: String) {

  def size(): Int = {
    val a = i split ""
    a.length match {
      case 0 => 0
      case 1 => decode(a)
      case _ =>
        val v = (a sliding 2).foldLeft(0){ (acc, x) => acc + decode(x)}
        if(i.length % 2 > 0) {
          decode(List(a.last)) + v
        } else {
          v
        }
    }
  } 

  def decode(x: Seq[String]): Int = x.size match {
    case 1 =>
      if(Decoder.m.contains(x.head)) {
        1
      } else {
        0
      }
    case _ =>
      List(Decoder.m.get(x.head), Decoder.m.get(x.mkString)).flatten.size
  }
  
}

object Decoder {

  val m = Map(
    "1" -> "a",
    "2" -> "b",
    "3" -> "c",
    "4" -> "d",
    "5" -> "e",
    "6" -> "f",
    "7" -> "g",
    "8" -> "h",
    "9" -> "i",
    "10" -> "j",
    "11" -> "k",
    "12" -> "l",
    "13" -> "m",
    "14" -> "n",
    "15" -> "o",
    "16" -> "p",
    "17" -> "q",
    "18" -> "r",
    "19" -> "s",
    "20" -> "t",
    "21" -> "u",
    "22" -> "v",
    "23" -> "w",
    "24" -> "x",
    "25" -> "y",
    "26" -> "z"
  )
  
}
