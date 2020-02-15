def repeatUntilSize(str: String, size: Int) : String = {
  val n = size / str.length
  val modulo = size % str.length

  (str * n) ++ str.slice(0, modulo)
}
def cipherChar(tk: (Char,Char)) : Char = {
  val (t,k) = tk
  ((t + k) % 26 + 65).toChar
}

def cipher(text: String, key: String="somerandomstuff") : String = {
  val transText = text.filterNot(_.isWhitespace)
                      .toUpperCase
  val transKey  = key.filterNot(_.isWhitespace)
                      .toUpperCase
  val keyArray  = repeatUntilSize(transKey, transText.length)

  transText.zip(keyArray)
      .map(cipherChar)
      .mkString
}

def decipherChar(tk: (Char,Char)) : Char = {
  val (t,k) = tk
  ((t - k + 26) % 26 + 65).toChar
}

def decipher(text: String, key: String = "somerandomstuff") : String = {
  val transText = text.filterNot(_.isWhitespace)
                      .toUpperCase
  val transKey  = key.filterNot(_.isWhitespace)
                      .toUpperCase
  val keyArray  = repeatUntilSize(transKey, transText.length)

  transText.zip(keyArray)
           .map(decipherChar)
           .mkString
}
