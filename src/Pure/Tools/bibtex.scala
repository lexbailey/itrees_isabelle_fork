/*  Title:      Pure/Tools/bibtex.scala
    Author:     Makarius

Some support for bibtex files.
*/

package isabelle


import scala.util.parsing.input.{Reader, CharSequenceReader}
import scala.util.parsing.combinator.RegexParsers


object Bibtex
{
  /** content **/

  val months = List(
    "jan",
    "feb",
    "mar",
    "apr",
    "may",
    "jun",
    "jul",
    "aug",
    "sep",
    "oct",
    "nov",
    "dec")

  val commands = List("preamble", "string")

  sealed case class Entry_Type(
    required: List[String],
    optional_crossref: List[String],
    optional: List[String])

  val entries =
    Map[String, Entry_Type](
      "Article" ->
        Entry_Type(
          List("author", "title"),
          List("journal", "year"),
          List("volume", "number", "pages", "month", "note")),
      "InProceedings" ->
        Entry_Type(
          List("author", "title"),
          List("booktitle", "year"),
          List("editor", "volume", "number", "series", "pages", "month", "address",
            "organization", "publisher", "note")),
      "InCollection" ->
        Entry_Type(
          List("author", "title", "booktitle"),
          List("publisher", "year"),
          List("editor", "volume", "number", "series", "type", "chapter", "pages",
            "edition", "month", "address", "note")),
      "InBook" ->
        Entry_Type(
         List("author", "editor", "title", "chapter"),
         List("publisher", "year"),
         List("volume", "number", "series", "type", "address", "edition", "month", "pages", "note")),
      "Proceedings" ->
        Entry_Type(
          List("title", "year"),
          List(),
          List("booktitle", "editor", "volume", "number", "series", "address", "month",
            "organization", "publisher", "note")),
      "Book" ->
        Entry_Type(
          List("author", "editor", "title"),
          List("publisher", "year"),
          List("volume", "number", "series", "address", "edition", "month", "note")),
      "Booklet" ->
        Entry_Type(
          List("title"),
          List(),
          List("author", "howpublished", "address", "month", "year", "note")),
      "PhdThesis" ->
        Entry_Type(
          List("author", "title", "school", "year"),
          List(),
          List("type", "address", "month", "note")),
      "MastersThesis" ->
        Entry_Type(
          List("author", "title", "school", "year"),
          List(),
          List("type", "address", "month", "note")),
      "TechReport" ->
        Entry_Type(
          List("author", "title", "institution", "year"),
          List(),
          List("type", "number", "address", "month", "note")),
      "Manual" ->
        Entry_Type(
          List("title"),
          List(),
          List("author", "organization", "address", "edition", "month", "year", "note")),
      "Unpublished" ->
        Entry_Type(
          List("author", "title", "note"),
          List(),
          List("month", "year")),
      "Misc" ->
        Entry_Type(
          List(),
          List(),
          List("author", "title", "howpublished", "month", "year", "note")))



  /** tokens and chunks **/

  object Token
  {
    object Kind extends Enumeration
    {
      val KEYWORD = Value("keyword")
      val NAT = Value("natural number")
      val IDENT = Value("identifier")
      val STRING = Value("string")
      val SPACE = Value("white space")
      val ERROR = Value("bad input")
    }
  }

  sealed case class Token(kind: Token.Kind.Value, val source: String)
  {
    def is_space: Boolean = kind == Token.Kind.SPACE
    def is_error: Boolean = kind == Token.Kind.ERROR
  }

  abstract class Chunk
  case class Ignored(source: String) extends Chunk
  case class Malformed(source: String) extends Chunk
  case class Item(tokens: List[Token]) extends Chunk
  {
    val name: String =
      tokens match {
        case Token(Token.Kind.KEYWORD, "@") :: body
        if !body.isEmpty && !body.exists(_.is_error) =>
          (body.filterNot(_.is_space), body.last) match {
            case (Token(Token.Kind.IDENT, id) :: Token(Token.Kind.KEYWORD, "{") :: _,
                  Token(Token.Kind.KEYWORD, "}")) => id
            case (Token(Token.Kind.IDENT, id) :: Token(Token.Kind.KEYWORD, "(") :: _,
                  Token(Token.Kind.KEYWORD, ")")) => id
            case _ => ""
          }
        case _ => ""
      }
    val entry_name: String = if (commands.contains(name.toLowerCase)) "" else name
    def is_wellformed: Boolean = name != ""
  }



  /** parsing **/

  // context of partial line-oriented scans
  abstract class Line_Context
  case class Delimited(quoted: Boolean, depth: Int) extends Line_Context
  val Finished = Delimited(false, 0)

  private def token(kind: Token.Kind.Value)(source: String): Token = Token(kind, source)
  private def keyword(source: String): Token = Token(Token.Kind.KEYWORD, source)


  // See also http://ctan.org/tex-archive/biblio/bibtex/base/bibtex.web
  // module @<Scan for and process a \.{.bib} command or database entry@>.

  object Parsers extends RegexParsers
  {
    /* white space and comments */

    override val whiteSpace = "".r

    private val space = """[ \t\n\r]+""".r ^^ token(Token.Kind.SPACE)
    private val spaces = rep(space)

    private val ignored =
      rep1("""(?mi)([^@]+|@[ \t\n\r]*comment)""".r) ^^ { case ss => Ignored(ss.mkString) }


    /* delimited string: outermost "..." or {...} and body with balanced {...} */

    private def delimited_depth(delim: Delimited): Parser[(String, Delimited)] =
      new Parser[(String, Delimited)]
      {
        require(if (delim.quoted) delim.depth > 0 else delim.depth >= 0)

        def apply(in: Input) =
        {
          val start = in.offset
          val end = in.source.length

          var i = start
          var q = delim.quoted
          var d = delim.depth
          var finished = false
          while (!finished && i < end) {
            val c = in.source.charAt(i)
            if (c == '"' && d == 0) { i += 1; d = 1; q = true }
            else if (c == '"' && d == 1) { i += 1; d = 0; q = false; finished = true }
            else if (c == '{') { i += 1; d += 1 }
            else if (c == '}' && d > 0) { i += 1; d -= 1; if (d == 0) finished = true }
            else if (d > 0) i += 1
            else finished = true
          }
          if (i == start) Failure("bad input", in)
          else
            Success((in.source.subSequence(start, i).toString,
              Delimited(q, d)), in.drop(i - start))
        }
      }.named("delimited_depth")

    private def delimited: Parser[String] =
      delimited_depth(Finished) ^? { case (x, delim) if delim == Finished => x }

    private def delimited_line(ctxt: Line_Context): Parser[(String, Line_Context)] =
    {
      ctxt match {
        case delim: Delimited => delimited_depth(delim)
        case _ => failure("")
      }
    }

    private val recover_delimited: Parser[String] =
      delimited_depth(Finished) ^^ (_._1)

    private val delimited_token =
      delimited ^^ token(Token.Kind.STRING) |
      recover_delimited ^^ token(Token.Kind.ERROR)


    /* other tokens */

    private val at = "@" ^^ keyword
    private val left_brace = "{" ^^ keyword
    private val right_brace = "}" ^^ keyword
    private val left_paren = "(" ^^ keyword
    private val right_paren = ")" ^^ keyword

    private val nat = "[0-9]+".r ^^ token(Token.Kind.NAT)

    private val ident =
      """[\x21-\x7f&&[^"#%'(),={}0-9]][\x21-\x7f&&[^"#%'(),={}]]*""".r ^^ token(Token.Kind.IDENT)


    /* chunks */

    private val item_start =
      at ~ spaces ~ ident ~ spaces ^^
        { case a ~ b ~ c ~ d => List(a) ::: b ::: List(c) ::: d }

    private val body_token = delimited_token | ("[=#,]".r ^^ keyword | (nat | (ident | space)))

    private val item =
      (item_start ~ left_brace ~ rep(body_token) ~ opt(right_brace) |
       item_start ~ left_paren ~ rep(body_token) ~ opt(right_paren)) ^^
        { case a ~ b ~ c ~ d => Item(a ::: List(b) ::: c ::: d.toList) }

    private val recover_item = "(?m)@[^@]+".r ^^ (s => Malformed(s))

    val chunks: Parser[List[Chunk]] = rep(ignored | (item | recover_item))
  }

  def parse(input: CharSequence): List[Chunk] =
  {
    val in: Reader[Char] = new CharSequenceReader(input)
    Parsers.parseAll(Parsers.chunks, in) match {
      case Parsers.Success(result, _) => result
      case _ => error("Unexpected failure of tokenizing input:\n" + input.toString)
    }
  }
}

