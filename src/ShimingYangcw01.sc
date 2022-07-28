abstract class Rexp
case object ZERO extends Rexp
case object ONE extends Rexp
case class CHAR(c: Char) extends Rexp
case class ALT(r1: Rexp, r2: Rexp) extends Rexp
case class SEQ(r1: Rexp, r2: Rexp) extends Rexp
case class STAR(r: Rexp) extends Rexp
// r{n}
case class NTIMES(r: Rexp, n: Int) extends Rexp
//[c1, c2, . . . , cn]
case class RANGE(cs:List[Char]) extends Rexp
// r+
case class PLUS(r: Rexp) extends Rexp
//r ?
case class OPTIONAL(r:Rexp) extends Rexp
// ...m
case class UPTO(r:Rexp, m : Int) extends Rexp
// n...
case class FROM(r:Rexp, n : Int) extends Rexp
// n..m
case class BETWEEN(r:Rexp, n : Int, m :Int) extends Rexp
//∼ r
case class NOT(r:Rexp) extends Rexp

case class CFUN(f: Char => Boolean) extends Rexp

// the nullable function: tests whether the regular
// expression can recognise the empty string
def nullable (r: Rexp) : Boolean = r match {
  case ZERO => false
  case ONE => true
  case CHAR(_) => false
  case ALT(r1, r2) => nullable(r1) || nullable(r2)
  case SEQ(r1, r2) => nullable(r1) && nullable(r2)
  case STAR(_) => true
  //r{n}
  case NTIMES(r, i) => if (i == 0) true else nullable(r)
  //[c1, c2, . . . , cn]
  case RANGE(cs:List[Char]) => if(cs.isEmpty) true else false
  // r+
  case PLUS(r) => nullable(r)
  //r ?
  case OPTIONAL(r) => true
  // ...m
  case UPTO(r, m) => true
  // n ...
  case FROM(r, n) => if (n == 0) true else nullable(r)
  // {n..m}
  case BETWEEN(r,n,m) => if(n ==0 || m ==0) true else nullable(r)
  //∼ r
  case NOT(r) => !nullable(r)

  case CFUN(_) => false
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
  //r{n}
  case NTIMES(r, i) =>
    if (i == 0) ZERO else SEQ(der(c, r), NTIMES(r, i - 1))
  //[c1, c2, . . . , cn]
  case RANGE(cs:List[Char]) =>
    if (cs.contains(c)) ONE
    else ZERO
  // r+
  case PLUS(r) => SEQ(der(c, r), STAR(r))
  // r?
  case OPTIONAL(r) => der(c, r)
  // ...m
  case UPTO(r,m) =>
    if (m == 0) ZERO else SEQ(der(c, r), UPTO(r, m - 1))
  // n ...
  case FROM(r, n) =>
    if(n == 0) SEQ(der(c, r), STAR(r))
    else SEQ(der(c, r),FROM(r,n-1))
  // {n..m}
  case  BETWEEN(r,n,m) =>
    if ( m ==0 ) ZERO
    else if(n ==0 ){
      if (m == 0) ZERO else SEQ(der(c, r),  BETWEEN(r,0, m - 1))
    }
    else{
      SEQ(der(c, r), BETWEEN(r,n-1,m-1))
    }
  //∼ r
  case NOT(r:Rexp) => NOT(der(c, r))

  case CFUN(f) => if(f(c)) ONE else ZERO

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
@arg(doc = "Test Q3 form")
@main
def testform() = {
  println("Test Q3 form")
  val r1:Rexp = OPTIONAL(CHAR('a'))
  val r2:Rexp = NOT(CHAR('a'))
  val r3:Rexp = NTIMES(CHAR('a'),3)
  val r4:Rexp = NTIMES(OPTIONAL(CHAR('a')),3)
  val r5:Rexp = UPTO(CHAR('a'),3)
  val r6:Rexp = UPTO(OPTIONAL(CHAR('a')),3)
  val r7:Rexp = BETWEEN(CHAR('a'),3,5)
  val r8:Rexp = BETWEEN(OPTIONAL(CHAR('a')),3,5)
  val regexs:Array[Rexp] = Array(r1,r2,r3,r4,r5,r6,r7,r8)

  val s1:String = ""
  val s2:String = "a"
  val s3:String = "aa"
  val s4:String = "aaa"
  val s5:String = "aaaa"
  val s6:String = "aaaaa"
  val strings:Array[String] = Array(s1,s2,s3,s4,s5,s6)

  for (a <- 0 to 7){
    println("-------------")
    println("the regx is " + regexs(a))
    for(b <- 0 to 5){
      println("test result for " + strings(b) + " is " +matcher(regexs(a),strings(b)))
    }
    println("-------------")
    println("")
  }
}

@arg(doc = "Test Q4")
@main
def testQ4() = {
  println("Test Q4")

  val cs:List[Char] = List('a','b','v')
  println(matcher(CFUN(d => d == 'x'),"a"))
  println(matcher(CFUN(d => d == 'x'),"x"))
  println("======")
  println(matcher(CFUN(d => cs.contains(d)),"x"))
  println(matcher(CFUN(d => cs.contains(d)),"a"))
  println("======")
  println(matcher(CFUN(d => true),"c"))
  println(matcher(CFUN(d => true),""))

}

@arg(doc = "Test Q5")
@main
def testQ5() = {
  println("Test Q5")

  var list:List[Char] = List()
  for ( i <- 'a' to 'z'){
    list = list:+i
  }
  for ( i <- '0' to '9'){
    list = list:+i
  }
  list = list:+'_'
  list = list:+'.'
  list = list:+'-'
  val r1:Rexp = RANGE(list)
  val r2:Rexp = SEQ(PLUS(r1),CHAR('@'))
  val r3:Rexp = SEQ(r2,PLUS(RANGE(list filter (c => c  !='-'))))
  val r4:Rexp = SEQ(r3,CHAR('.'))
  val r:Rexp = SEQ(r4,BETWEEN(RANGE(list filter(c => (c>='a' && c<='z') || c =='.' )),2,6))

  println(matcher(r,"k21030405@kcl.ac.uk"))
}

@arg(doc = "Test Q6")
@main
def testQ6() = {
  println("Test Q6")
  //   / · ∗
  val r1:Rexp = SEQ(CHAR('/'),CHAR('*'))
  //   ∼ (ALL∗ *  /  ALL∗ )
  val r2:Rexp = NOT(SEQ(SEQ(SEQ(STAR(CFUN(d => true)),CHAR('*')),CHAR('/')),STAR(CFUN(d => true))))
  //  ∗ ·/
  val r3:Rexp = SEQ(CHAR('*'),CHAR('/'))

  val r:Rexp = SEQ(SEQ(r1,r2),r3)
  println(matcher(r,"/**/"))
  println(matcher(r,"/*foobar*/"))
  println(matcher(r,"/*test*/test*/"))
  println(matcher(r,"/*test/*test*/"))
}

@arg(doc = "Test Q7")
@main
def testQ7() = {
  println("Test Q7")

  val r1:Rexp = SEQ(SEQ(CHAR('a'),CHAR('a')),CHAR('a'))
  val r2:Rexp = SEQ(BETWEEN(CHAR('a'),19,19),OPTIONAL(CHAR('a')))

  var s1:String = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
  var s2:String = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
  var s3:String = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
  var stringarray :Array[String] = Array(s1,s2,s3)
  for( i <- stringarray){
    println("===============")
    println(matcher(PLUS(PLUS(r1)),i))
    println(matcher(PLUS(PLUS(r2)),i))
    println("===============")
    println()
  }

}

@arg(doc = "Test RANGE")
@main
def test1() = {
  println("Test 001")
//  println(matcher(RANGE("abc".toList),"b"))
//  println(matcher(RANGE("abc".toList),"x"))
//  println(matcher(RANGE("abc".toList),""))
  println(matcher(SEQ(STAR(CHAR('a')),CHAR('b')),"aab"))
}

@arg(doc = "Test PLUS")
@main
def test2() = {
  println("Test 002")
  var r1 = CHAR('a')
  var r2 = CHAR('b')
  var r3 = CHAR('c')
  var r4 = SEQ(r1,r2)

  println(matcher(PLUS(r4),"ab"))
  println(matcher(PLUS(r4),""))
  println(matcher(PLUS(r4),"ababab"))
  println(matcher(PLUS(r4),"abababx"))
}

@arg(doc = "Test OPTIONAL")
@main
def test3() = {
  println("Test 003")
  var r1 = CHAR('a')
  var r2 = CHAR('b')
  var r3 = CHAR('c')
  var r4 = SEQ(r1,r2)

  println(matcher(OPTIONAL(r4),"ab"))
  println(matcher(OPTIONAL(r4),"abab"))
  println(matcher(OPTIONAL(r4),""))
  println(matcher(OPTIONAL(r4),"x"))
}

@arg(doc = "Test UPTO")
@main
def test4() = {
  println("Test 004")
  var r1 = CHAR('a')
  var r2 = CHAR('b')
  var r3 = CHAR('c')
  var r4 = SEQ(r1,r2)

  println(matcher(UPTO(r4,3),"ababab"))
  println(matcher(UPTO(r4,3),"abababab"))
  println(matcher(UPTO(r4,3),""))
  println(matcher(UPTO(r4,3),"x"))
}

@arg(doc = "Test FROM")
@main
def test5() = {
  println("Test 005")
  var r1 = CHAR('a')
  var r2 = CHAR('b')
  var r3 = CHAR('c')
  var r4 = SEQ(r1,r2)
  println(matcher(FROM(r4,2),"ab"))
  println(matcher(FROM(r4,2),"abab"))
  println(matcher(FROM(r4,2),"abx"))
}

@arg(doc = "Test  BETWEEN")
@main
def test6() = {
  println("Test 006")
  var r1 = CHAR('a')
  var r2 = CHAR('b')
  var r3 = CHAR('c')
  var r4 = SEQ(r1,r2)
  println(matcher( BETWEEN(r4,1,2),"abab"))
  println(matcher( BETWEEN(r4,1,2),"ab"))
  println(matcher( BETWEEN(r4,1,2),""))
  println(matcher( BETWEEN(r4,1,2),"ababab"))
  println(matcher( BETWEEN(r4,1,2),"abababxc"))
}

@arg(doc = "Test NOT")
@main
def test7() = {
  println("Test 007")
  var r1 = CHAR('a')
  var r2 = CHAR('b')
  var r3 = CHAR('c')
  var r4 = SEQ(r1,r2)
  println(matcher(NOT(r4),"abd"))
  println(matcher(NOT(r4),"ab"))
}

@arg(doc = "All tests.")
@main
def all() = { testform();  testQ4();testQ5(); testQ6(); testQ7() ;
  test1(); test2(); test3(); test4(); test5(); test6(); test7()}

