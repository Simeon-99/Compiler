abstract class Rexp
case object ZERO extends Rexp
case object ONE extends Rexp
case class CHAR(c: Char) extends Rexp
case class ALT(r1: Rexp, r2: Rexp) extends Rexp
case class SEQ(r1: Rexp, r2: Rexp) extends Rexp
case class STAR(r: Rexp) extends Rexp
case class NTIMES(r: Rexp, n: Int) extends Rexp
case class MULTI(s: List[Char]) extends Rexp




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
  case MULTI(s:List[Char]) =>
   nullable(CHAR(s.head))||nullable(MULTI(s.tail))
}

// the derivative of a regular expression w.r.t. a character
def der(c: Char, r: Rexp) : Rexp = r match {
  case ZERO => ZERO
  case ONE => ZERO
  case CHAR(d) => if (c == d) ONE else ZERO
  case ALT(r1, r2) => ALT(der(c, r1), der(c, r2))
  case SEQ(r1, r2) =>
    if (nullable(r1)) ALT(SEQ(der(c, r1), r2), der(c, r2))
    else SEQ(der(c, r1), r2)
  case STAR(r1) => SEQ(der(c, r1), STAR(r1))
  case NTIMES(r, i) =>
    if (i == 0) ZERO else SEQ(der(c, r), NTIMES(r, i - 1))
  case MULTI(s:List[Char]) =>
  if(s.isEmpty) ZERO
  else ALT(der(c,CHAR(s.head)),der(c,MULTI(s.tail)))
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




// the derivative w.r.t. a string (iterates der)
def ders(s: List[Char], r: Rexp) : Rexp = s match {
  case Nil => r
  case c::s => ders(s, simp(der(c, r)))
}


// the main matcher function
def matcher(r: Rexp, s: String) : Boolean =
  nullable(ders(s.toList, r))


// one or zero
def OPT(r: Rexp) = ALT(r, ONE)



// Test Cases
//val EVIL4 = STAR(ALT(ONE, ALT(CHAR('a'), SEQ(CHAR('a'), CHAR('a')))))
@arg(doc = "Test 1")
@main
def test1() = {
  val a = MULTI("".toList)
  println("Test 1"+a)

  println(matcher(a,"a"))
}

