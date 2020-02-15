import scala.io.Source

def repeatUntilSize(str: String, size: Int) : String = {
  val n = size / str.length
  val modulo = size % str.length

  (str * n) ++ str.slice(0, modulo)
}
def cipherChar(tk:(Char,Char)) : Char = {
  val (t,k) = tk
  if(t == ' '){
    ' '
  }else{
    ((t + k) % 26 + 65).toChar
  }
}
/*
No momento so funciona com inputs sem espaço e com
todas as letras maiusculas.
o problema é que com espaços a chave e o texto desalinham, mesmo desconsiderando espaços'
*/
def cipher(text: String, key: String="somerandomstuff") : String = {
  val key_array   = repeatUntilSize(key, text.length)//.map(_.toByte)

  text.zip(key_array)
      .map(cipherChar)
      .mkString
}
/*
def decipher(text: String, key: String = "somerandomstuff") : String = {
  val ascii_array = text.map(_.toByte)
  val ascii_key   = repeatUntilSize(key, text.length ).map(_.toByte)
}
*/
