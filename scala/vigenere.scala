import scala.io.Source

object VigenereCipher {

  def repeatUntilSize(str: String, size: Int) : String = {
    val n = size / str.length
    val modulo = size % str.length

    (str * n) ++ str.slice(0, modulo)
  }

  def cipherChar(tk: (Char,Char)) : Char = {
    val (t,k) = tk
    ((t + k) % 91 + 32).toChar
  }

  def cipher(text: String, key: String ) : String = {
    val transText = text.map((c)=>(c-32).toChar)
    val transKey  = key.map((c)=>(c-32).toChar)
    val keyArray  = repeatUntilSize(transKey, transText.length)

    transText.zip(keyArray)
        .map(cipherChar)
        .mkString
  }

  def decipherChar(tk: (Char,Char)) : Char = {
    val (t,k) = tk
    ((t - k + 91) % 91 + 32).toChar
  }

  def decipher(text: String, key: String ) : String = {
    val transText = text.map((c)=>(c-32).toChar)
    val transKey  = key.map((c)=>(c-32).toChar)
    val keyArray  = repeatUntilSize(transKey, transText.length)

    transText.zip(keyArray)
             .map(decipherChar)
             .mkString
  }
  
  def main(args: Array[String]): Unit = {
    if (args.size == 0)
      println("Por favor utilize 3 argumentos de entrada:\nmodo (c/d)\narquivo_texto\nchave")
    else{
      val mode = args(0)
      val txt  = Source.fromFile(args(1)).getLines.mkString
      val key  = args(2)
      mode match {
        case "c" => println(cipher(txt,key))
        case "d" => println(decipher(txt,key))
      }
    }   
  }
}
