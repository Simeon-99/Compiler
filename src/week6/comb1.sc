// Parser Combinators: Simple Version
//====================================
//
// Call with
//
//  amm comb1.sc

 
//  Note, in the lectures I did not show the implicit type constraint
//  I : IsSeq, which means that the input type 'I' needs to be
//  a sequence. 

type IsSeq[A] = A => Seq[_]


abstract class Parser[I : IsSeq, T]{
  def parse(in: I): Set[(T, I)]

  def parse_all(in: I) : Set[T] =
    for ((hd, tl) <- parse(in); 
        if tl.isEmpty) yield hd
}

// parser combinators

// alternative parser
class AltParser[I : IsSeq, T](p: => Parser[I, T], 
                              q: => Parser[I, T]) extends Parser[I, T] {
  def parse(in: I) = p.parse(in) ++ q.parse(in)   
}

// sequence parser
class SeqParser[I : IsSeq, T, S](p: => Parser[I, T], 
                                 q: => Parser[I, S]) extends Parser[I, (T, S)] {
  def parse(in: I) = 
    for ((hd1, tl1) <- p.parse(in); 
         (hd2, tl2) <- q.parse(tl1)) yield ((hd1, hd2), tl2)
}

// map parser
class MapParser[I : IsSeq, T, S](p: => Parser[I, T], 
                                 f: T => S) extends Parser[I, S] {
  def parse(in: I) = for ((hd, tl) <- p.parse(in)) yield (f(hd), tl)
}



// an example of an atomic parser for characters
case class CharParser(c: Char) extends Parser[String, Char] {
  def parse(in: String) = 
    if (in != "" && in.head == c) Set((c, in.tail)) else Set()
}


// an atomic parser for parsing strings according to a regex
import scala.util.matching.Regex

case class RegexParser(reg: Regex) extends Parser[String, String] {
  def parse(in: String) = reg.findPrefixMatchOf(in)  match {
    case None => Set()
    case Some(m) => Set((m.matched, m.after.toString))  
  }
}

// atomic parsers for numbers and "verbatim" strings 
val NumParser = RegexParser("[0-9]+".r)
def StrParser(s: String) = RegexParser(Regex.quote(s).r)



// NumParserInt transforms a "string integer" into a propper Int
// (needs "new" because MapParser is not a case class)

val NumParserInt = new MapParser(NumParser, (s: String) => s.toInt)


// the following string interpolation allows us to write 
// StrParser(_some_string_) more conveniently as 
//
// p"<_some_string_>" 

implicit def parser_interpolation(sc: StringContext) = new {
  def p(args: Any*) = StrParser(sc.s(args:_*))
}
           

// more convenient syntax for parser combinators
implicit def ParserOps[I : IsSeq, T](p: Parser[I, T]) = new {
  def ||(q : => Parser[I, T]) = new AltParser[I, T](p, q)
  def ~[S] (q : => Parser[I, S]) = new SeqParser[I, T, S](p, q)
  def map[S](f: => T => S) = new MapParser[I, T, S](p, f)
}

// these implicits allow us to use an infix notation for
// sequences and alternatives; we also can write the usual
// map for a MapParser


// with this NumParserInt can now be written more conveniently
// as:

val NumParserInt2 = NumParser.map(_.toInt)


// A parser for palindromes (just returns them as string)
lazy val Pal : Parser[String, String] = {
  ((p"a" ~ Pal) ~ p"a").map{ case ((x, y), z) => s"$x$y$z" } || 
  (p"b" ~ Pal ~ p"b").map{ case ((x, y), z) => s"$x$y$z" } || 
  p"a" || p"b" || p""
}  

def foo(x: Int) = 
   println(s"$x + $x = ${x + x}")

// examples
Pal.parse_all("abaaaba")
Pal.parse("abaaaba")

println(Pal.parse("abaaaba"))
println("Palindrome: " + Pal.parse_all("abaaaba"))

// A parser for wellnested parentheses 
//
//   P ::= ( P ) P | epsilon
//
//   (transforms '(' -> '{' , ')' -> '}' )
lazy val P : Parser[String, String] = {
  (p"(" ~ P ~ p")" ~ P).map{ case (((_, x), _), y) => "{" + x + "}" + y } ||
  p""
}  

println(P.parse_all("(((()()))())"))
println(P.parse_all("(((()()))()))"))
println(P.parse_all(")("))
println(P.parse_all("()"))

// A parser for arithmetic expressions (Terms and Factors)

lazy val E: Parser[String, Int] = {
  (T ~ p"+" ~ E).map{ case ((x, _), z) => x + z } ||
  (T ~ p"-" ~ E).map{ case ((x, _), z) => x - z } || T }
lazy val T: Parser[String, Int] = {
  (F ~ p"*" ~ T).map{ case ((x, _), z) => x * z } || F }
lazy val F: Parser[String, Int] = {
  (p"(" ~ E ~ p")").map{ case ((_, y), _) => y } || NumParserInt }

println(E.parse_all("1+3+4"))
println(E.parse("1+3+4"))
println(E.parse_all("4*2+3"))
println(E.parse_all("4*(2+3)"))
println(E.parse_all("(4)*((2+3))"))
println(E.parse_all("4/2+3"))
println(E.parse("1 + 2 * 3"))
println(E.parse_all("(1+2)+3"))
println(E.parse_all("1+2+3"))


// with parser combinators (and other parsing algorithms)
// no left-recursion is allowed, otherwise the will loop

lazy val EL: Parser[String, Int] = 
  ((EL ~ p"+" ~ EL).map{ case ((x, y), z) => x + z} || 
   (EL ~ p"*" ~ EL).map{ case ((x, y), z) => x * z} ||
   (p"(" ~ EL ~ p")").map{ case ((x, y), z) => y} ||
   NumParserInt)

// this will run forever:
//println(EL.parse_all("1+2+3"))


// non-ambiguous vs ambiguous grammars

// ambiguous
lazy val S : Parser[String, String] =
  (p"1" ~ S ~ S).map{ case ((x, y), z) => x + y + z } || p""

println(time(S.parse("1" * 10)))
println(time(S.parse_all("1" * 10)))

// non-ambiguous
lazy val U : Parser[String, String] =
  (p"1" ~ U).map{ case (x, y) => x + y } || p""

println(time(U.parse("1" * 10)))
println(time(U.parse_all("1" * 10)))
println(U.parse("1" * 25))

U.parse("11")
U.parse("11111")
U.parse("11011")

U.parse_all("1" * 100)
U.parse_all("1" * 100 + "0")

// you can see the difference in second example
//S.parse_all("1" * 100)         // succeeds
//S.parse_all("1" * 100 + "0")   // fails


// A variant which counts how many 1s are parsed
lazy val UCount : Parser[String, Int] =
  (p"1" ~ UCount).map{ case (_, y) => y + 1 } || p"".map{ _ => 0 }

println(UCount.parse("11111"))
println(UCount.parse_all("11111"))

// Two single character parsers
lazy val One : Parser[String, String] = p"a"
lazy val Two : Parser[String, String] = p"b"

One.parse("a")
One.parse("aaa")

// note how the pairs nest to the left with sequence parsers
(One ~ One).parse("aaa")
(One ~ One ~ One).parse("aaa")
(One ~ One ~ One ~ One).parse("aaaa")

(One || Two).parse("aaa")



// a problem with the arithmetic expression parser: it 
// gets very slow with deeply nested parentheses

println("Runtime problem")
println(E.parse("1"))
println(E.parse("(1)"))
println(E.parse("((1))"))
//println(E.parse("(((1)))"))
//println(E.parse("((((1))))"))
//println(E.parse("((((((1))))))"))
//println(E.parse("(((((((1)))))))"))
//println(E.parse("((((((((1)))))))"))




// runs with amm2 and amm3
