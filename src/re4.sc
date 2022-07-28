// (ASIDE!) A version which attempts to move entire strings, 
// not just characters, under derivatives whenever possible
//    
// call the test cases with X = {1,2}
//
//   amm re4.sc testX
//
// or 
//
//   amm re4.sc all
//
// !! DO NOT USE THIS VERSION AS A STARTING POINT !!
// !! FOR CW1 & 2! USE re3.sc INSTEAD             !!


abstract class Rexp
case object ZERO extends Rexp
case object ONE extends Rexp
case class CHAR(c: Char) extends Rexp
case class ALT(r1: Rexp, r2: Rexp) extends Rexp 
case class SEQ(r1: Rexp, r2: Rexp) extends Rexp 
case class STAR(r: Rexp) extends Rexp 
case class NTIMES(r: Rexp, n: Int) extends Rexp 

// the nullable function: tests whether the regular 
// expression can recognise the empty string
def nullable (r: Rexp) : Boolean = r match {
  case ZERO => false
  case ONE => true
  case CHAR(_) => false
  case ALT(r1, r2) => nullable(r1) || nullable(r2)
  case SEQ(r1, r2) => nullable(r1) && nullable(r2)
  case STAR(_) => true
  case NTIMES(r, i) => if (i == 0) true else nullable(r)
}

// the derivative of a regular expression w.r.t. a character
def der (c: Char, r: Rexp) : Rexp = r match {
  case ZERO => ZERO
  case ONE => ZERO
  case CHAR(d) => if (c == d) ONE else ZERO
  case ALT(r1, r2) => ALT(der(c, r1), der(c, r2))
  case SEQ(r1, r2) => 
    if (nullable(r1)) ALT(SEQ(der(c, r1), r2), der(c, r2))
    else SEQ(der(c, r1), r2)
  case STAR(r1) => SEQ(der(c, r1), STAR(r1))
  case NTIMES(r1, i) => 
    if (i == 0) ZERO else SEQ(der(c, r1), NTIMES(r1, i - 1))
}

def simp(r: Rexp) : Rexp = r match {
  case ALT(r1, r2) => (simp(r1), simp(r2)) match {
    case (ZERO, r2s) => r2s
    case (r1s, ZERO) => r1s
    case (r1s, r2s) => if (r1s == r2s) r1s else ALT (r1s, r2s)
  }
  case SEQ(r1, r2) =>  (simp(r1), simp(r2)) match {
    case (ZERO, _) => ZERO
    case (_, ZERO) => ZERO
    case (ONE, r2s) => r2s
    case (r1s, ONE) => r1s
    case (r1s, r2s) => SEQ(r1s, r2s)
  }
  case r => r
}

// an example
val r = SEQ(SEQ(CHAR('x'), CHAR('y')), CHAR('z'))
der('x', r)
der('y', der('x', r))
der('z', der('y', der('x', r)))
simp(der('z', der('y', der('x', r))))

// *new*
// the derivative w.r.t. a string (iterates der)
def ders2(s: List[Char], r: Rexp) : Rexp = (s, r) match {
  case (Nil, r) => r
  case (s, ZERO) => ZERO
  case (s, ONE) => if (s == Nil) ONE else ZERO
  case (s, CHAR(c)) => if (s == List(c)) ONE else 
                       if (s == Nil) CHAR(c) else ZERO
  case (s, ALT(r1, r2)) => ALT(ders2(s, r1), ders2(s, r2))
  case (c::s, r) => ders2(s, simp(der(c, r)))
}

// the main matcher function
def matcher(r: Rexp, s: String) : Boolean = 
  nullable(ders2(s.toList, r))


// one or zero
def OPT(r: Rexp) = ALT(r, ONE)


// Test Cases

def EVIL1(n: Int) = SEQ(NTIMES(OPT(CHAR('a')), n), NTIMES(CHAR('a'), n))
val EVIL2 = SEQ(STAR(STAR(CHAR('a'))), CHAR('b'))

// for measuring time
def time_needed[T](i: Int, code: => T) = {
  val start = System.nanoTime()
  for (j <- 1 to i) code
  val end = System.nanoTime()
  (end - start) / (i * 1.0e9)
}


@arg(doc = "Test (a?{n}) (a{n})")
@main
def test1() = {
  for (i <- 0 to 11000 by 1000) {
    println(f"$i: ${time_needed(2, matcher(EVIL1(i), "a" * i))}%.5f")
  }
}

@arg(doc = "Test (a*)* b")
@main
def test2() = {
  for (i <- 0 to 7000000 by 500000) {
    println(f"$i: ${time_needed(2, matcher(EVIL2, "a" * i))}%.5f")
  }
} 

@arg(doc = "All tests.")
@main
def all() = { test1(); test2() } 



// the size of a regular expressions - for testing purposes 
def size(r: Rexp) : Int = r match {
  case ZERO => 1
  case ONE => 1
  case CHAR(_) => 1
  case ALT(r1, r2) => 1 + size(r1) + size(r2)
  case SEQ(r1, r2) => 1 + size(r1) + size(r2)
  case STAR(r) => 1 + size(r)
  case NTIMES(r, _) => 1 + size(r)
}


// string of a regular expressions - for testing purposes 
def string(r: Rexp) : String = r match {
  case ZERO => "0"
  case ONE => "1"
  case CHAR(c) => c.toString 
  case ALT(r1, r2) => s"(${string(r1)} + ${string(r2)})"
  case SEQ(CHAR(c), CHAR(d)) => s"${c}${d}"
  case SEQ(r1, r2) => s"(${string(r1)} ~ ${string(r2)})"
  case STAR(r) => s"(${string(r)})*"
  case NTIMES(r, n) =>  s"(${string(r)}){${n}}"
}


// further testcases:

// test: ("a" | "aa")*
val EVIL3 = STAR(ALT(CHAR('a'), SEQ(CHAR('a'), CHAR('a'))))

// test: ("" | "a" | "aa")*
val EVIL4 = STAR(ALT(ONE, ALT(CHAR('a'), SEQ(CHAR('a'), CHAR('a')))))




// runs with amm2 and amm3
