/*  Title:      Pure/General/utf8.scala
    Author:     Makarius

Variations on UTF-8.
*/

package isabelle


import java.nio.charset.Charset


object UTF8 {
  /* charset */

  val charset_name: String = "UTF-8"
  val charset: Charset = Charset.forName(charset_name)

  def bytes(s: String): Array[Byte] = s.getBytes(charset)


  /* permissive UTF-8 decoding */

  // see also https://en.wikipedia.org/wiki/UTF-8#Description
  // overlong encodings enable byte-stuffing of low-ASCII

  def decode_permissive(text: CharSequence): String = {
    val len = text.length
    val buf = new java.lang.StringBuilder(len)
    var code = -1
    var rest = 0
    def flush(): Unit = {
      if (code != -1) {
        if (rest == 0 && Character.isValidCodePoint(code))
          buf.appendCodePoint(code)
        else buf.append('\uFFFD')
        code = -1
        rest = 0
      }
    }
    def init(x: Int, n: Int): Unit = {
      flush()
      code = x
      rest = n
    }
    def push(x: Int): Unit = {
      if (rest <= 0) init(x, -1)
      else {
        code <<= 6
        code += x
        rest -= 1
      }
    }
    for (i <- 0 until len) {
      val c = text.charAt(i)
      if (c < 128) { flush(); buf.append(c) }
      else if ((c & 0xC0) == 0x80) push(c & 0x3F)
      else if ((c & 0xE0) == 0xC0) init(c & 0x1F, 1)
      else if ((c & 0xF0) == 0xE0) init(c & 0x0F, 2)
      else if ((c & 0xF8) == 0xF0) init(c & 0x07, 3)
    }
    flush()
    buf.toString
  }
}
