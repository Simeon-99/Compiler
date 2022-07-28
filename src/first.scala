import java.util.Optional
import javax.swing.event.PopupMenuListener

class   first {

}
object first extends App {
//   var greet: String = "abcd";
//  var mylist: List[Char] = greet.toList
//  var a:Array[Int] = new Array[Int](5)
//  println(('a'+1).toChar)
//
//  var list:List[Int] = a.toList
//  list = 1+:list
//  list = 2+:list
//  println(list)
//  var arr :Array[Char] = new Array[Char](10)
//var list:List[Char] = List()
//  for ( i <- 'a' to 'z'){
//    list = list:+i
//  }
//  for ( i <- '0' to '9'){
//    list = list:+i
//  }
//  list = list:+'_'
//  list = list:+'.'
//  list = list:+'-'
//  println(list)
//  println(list filter  (c => c !='-'))
//  println(list filter  (c => c !='0'))
//  println (list filter(c => (c>='a' && c<='z') || c =='.' ))

  var s1:String = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
  var s2:String = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
  var s3:String = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
  println(s1.length)
  println(s2.length)
  println(s3.length)

}
object test {
  def factC(n:Int, ret: Int => Int): Int ={
    if(n ==0) ret(1)
    else factC(n-1,x => ret(x*n))
  }
  def hanoi(n: Int, a: Int, b: Int, c: Int):String ={
    if (n != 0){
      hanoi(n - 1, a, c, b);
      print(a);
      print('-'); print('>'); // prints out "->"
      print(b);
      print('\n');
      hanoi(n - 1, c, b, a)
    } else "finsih";
  }
  def main(args: Array[String]): Unit = {
//    def mkeps(r: Int) : String = r match {
//      case 1 => "yes"
//
//      case 2 => "no"
//
//      case _ =>"haha"
//
//    }
//    println(mkeps(1))
//    println(mkeps(3))
//    println(mkeps(2))
//    val x  = "\n".toCharArray
//    val s = "s"
//    val c = '0' +2
//    println(c.toInt)
//    val s1="HashSet(List(Assign(start,Num(1000)), Assign(x,Var(start)), Assign(y,Var(start)), Assign(z,Var(start)), While(Bop(<,Num(0),Var(x)),Lis\nt(While(Bop(<,Num(0),Var(y)),List(While(Bop(<,Num(0),Var(z)),List(Assign(z,Aop(-,Var(z),Num(1))))), Assign(z,Var(start)), Assign(y,Aop\n(-,Var(y),Num(1))))), Assign(y,Var(start)), Assign(x,Aop(-,Var(x),Num(1)))))))"
//    val m1 = List((2, 3), (5, 7),(9,8 ))
////    val result = m1.flatMap(x => x+2)mkString("*","-","+")
//    val result1 = m1.mkString("*")
//
//    val m2 = List((2, 3), (5, 7),(2,8) )
//    val str = ""
//    println(str+"===============")
//    for(x <- m2){
//      x._1 match {
//        case 3 =>  println(2)
//        case 2 =>   println(5)
//        case _ =>
//      }
//    }
//    println(str+"===============")
//    var ts = Map[String, String]("1" -> "2")
//    var tp = ts +("s"->"ty")
//     tp = ts +("s"->"ty1")
//    println(tp)
//    val lst = "List('a', 'b', 'c')"
//    println(lst.length)
//    println(lst.length())
    val s1 = "\ndeclare i32 @printf(i8*, ...)\n\n@.str_nl = private constant [2 x i8] c\"\\0A\\00\"\n@.str_star = private constant [2 x i8] c\"*\\00\"\n@.str_space = private constant [2 x i8] c\" \\00\"\n\ndefine void @new_line() #0 {\n  %t0 = getelementptr [2 x i8], [2 x i8]* @.str_nl, i32 0, i32 0\n  %1 = call i32 (i8*, ...) @printf(i8* %t0)\n  ret void\n}\n\ndefine void @print_star() #0 {\n  %t0 = getelementptr [2 x i8], [2 x i8]* @.str_star, i32 0, i32 0\n  %1 = call i32 (i8*, ...) @printf(i8* %t0)\n  ret void\n}\n\ndefine void @print_space() #0 {\n  %t0 = getelementptr [2 x i8], [2 x i8]* @.str_space, i32 0, i32 0\n  %1 = call i32 (i8*, ...) @printf(i8* %t0)\n  ret void\n}\n\ndefine void @skip() #0 {\n  ret void\n}\n\n@.str = private constant [3 x i8] c\"%d\\00\"\n@.str_char = private constant [3 x i8] c\"%c\\00\"\n\ndefine void @print_int(i32 %x) {\n   %t0 = getelementptr [3 x i8], [3 x i8]* @.str, i32 0, i32 0\n   call i32 (i8*, ...) @printf(i8* %t0, i32 %x)\n   ret void\n}\n\ndefine void @print_char(i32 %x) {\n   %t0 = getelementptr [3 x i8], [3 x i8]* @.str_char, i32 0, i32 0\n   call i32 (i8*, ...) @printf(i8* %t0, i32 %x)\n   ret void\n}\n\n; END OF BUILD-IN FUNCTIONS (prelude)\n\n@Ymin = global double -1.3\n\n@Ymax = global double 1.3\n\n@Ystep = global double 0.05\n\n@Xmin = global double -2.1\n\n@Xmax = global double 1.1\n\n@Xstep = global double 0.02\n\n@Maxiters = global i32 1000\n\ndefine void @m_iter (i32 %m, double %x, double %y, double %zr, double %zi) {\n   %tmp_1 = load i32, i32* @Maxiters\n   %tmp_0 = icmp sle i32  %tmp_1, %m\n   br i1 %tmp_0, label %if_branch_19, label %else_branch_20\n\nif_branch_19:\n   call void @print_char (i32 32)\n   ret void\n\nelse_branch_20:\n   %tmp_5 = fmul double  %zi, %zi\n   %tmp_6 = fmul double  %zr, %zr\n   %tmp_4 = fadd double  %tmp_5, %tmp_6\n   %tmp_3 = fcmp ole double  4.0, %tmp_4\n   br i1 %tmp_3, label %if_branch_21, label %else_branch_22\n\nif_branch_21:\n   %tmp_8 = srem i32  %m, 10\n   %tmp_7 = add i32  48, %tmp_8\n   call void @print_char (i32 %tmp_7)\n   ret void\n\nelse_branch_22:\n   %tmp_10 = add i32  %m, 1\n   %tmp_13 = fmul double  %zr, %zr\n   %tmp_14 = fmul double  %zi, %zi\n   %tmp_12 = fsub double  %tmp_13, %tmp_14\n   %tmp_11 = fadd double  %x, %tmp_12\n   %tmp_17 = fmul double  %zr, %zi\n   %tmp_16 = fmul double  2.0, %tmp_17\n   %tmp_15 = fadd double  %tmp_16, %y\n   call void @m_iter (i32 %tmp_10, double %x, double %y, double %tmp_11, double %tmp_15)\n   ret void\n}\n\ndefine void @x_iter (double %x, double %y) {\n   %tmp_24 = load double, double* @Xmax\n   %tmp_23 = fcmp ole double  %x, %tmp_24\n   br i1 %tmp_23, label %if_branch_30, label %else_branch_31\n\nif_branch_30:\n   call void @m_iter (i32 0, double %x, double %y, double 0.0, double 0.0)\n   %tmp_27 = load double, double* @Xstep\n   %tmp_26 = fadd double  %x, %tmp_27\n   call void @x_iter (double %tmp_26, double %y)\n   ret void\n\nelse_branch_31:\n   call void @skip ()\n   ret void\n}\n\ndefine void @y_iter (double %y) {\n   %tmp_33 = load double, double* @Ymax\n   %tmp_32 = fcmp ole double  %y, %tmp_33\n   br i1 %tmp_32, label %if_branch_41, label %else_branch_42\n\nif_branch_41:\n   %tmp_34 = load double, double* @Xmin\n   call void @x_iter (double %tmp_34, double %y)\n   call void @print_char (i32 10)\n   %tmp_38 = load double, double* @Ystep\n   %tmp_37 = fadd double  %y, %tmp_38\n   call void @y_iter (double %tmp_37)\n   ret void\n\nelse_branch_42:\n   call void @skip ()\n   ret void\n}\n\ndefine i32 @main() {\n   %tmp_43 = load double, double* @Ymin\n   call void @y_iter (double %tmp_43)\n   ret i32 0\n}"
    val s2 = "\ndeclare i32 @printf(i8*, ...)\n\n@.str_nl = private constant [2 x i8] c\"\\0A\\00\"\n@.str_star = private constant [2 x i8] c\"*\\00\"\n@.str_space = private constant [2 x i8] c\" \\00\"\n\ndefine void @new_line() #0 {\n  %t0 = getelementptr [2 x i8], [2 x i8]* @.str_nl, i32 0, i32 0\n  %1 = call i32 (i8*, ...) @printf(i8* %t0)\n  ret void\n}\n\ndefine void @print_star() #0 {\n  %t0 = getelementptr [2 x i8], [2 x i8]* @.str_star, i32 0, i32 0\n  %1 = call i32 (i8*, ...) @printf(i8* %t0)\n  ret void\n}\n\ndefine void @print_space() #0 {\n  %t0 = getelementptr [2 x i8], [2 x i8]* @.str_space, i32 0, i32 0\n  %1 = call i32 (i8*, ...) @printf(i8* %t0)\n  ret void\n}\n\ndefine void @skip() #0 {\n  ret void\n}\n\n@.str = private constant [3 x i8] c\"%d\\00\"\n@.str_char = private constant [3 x i8] c\"%c\\00\"\n\ndefine void @print_int(i32 %x) {\n   %t0 = getelementptr [3 x i8], [3 x i8]* @.str, i32 0, i32 0\n   call i32 (i8*, ...) @printf(i8* %t0, i32 %x)\n   ret void\n}\n\ndefine void @print_char(i32 %x) {\n   %t0 = getelementptr [3 x i8], [3 x i8]* @.str_char, i32 0, i32 0\n   call i32 (i8*, ...) @printf(i8* %t0, i32 %x)\n   ret void\n}\n\n; END OF BUILD-IN FUNCTIONS (prelude)\n\n@Ymin = global double -1.3\n\n@Ymax = global double 1.3\n\n@Ystep = global double 0.05\n\n@Xmin = global double -2.1\n\n@Xmax = global double 1.1\n\n@Xstep = global double 0.02\n\n@Maxiters = global i32 1000\n\ndefine void @m_iter (i32 %m, double %x, double %y, double %zr, double %zi) {\n   %tmp_1 = load i32, i32* @Maxiters\n   %tmp_0 = icmp sle i32  %tmp_1, %m\n   br i1 %tmp_0, label %if_branch_19, label %else_branch_20\n\nif_branch_19:\n   call void @print_char (i32 32)\n   ret void\n\nelse_branch_20:\n   %tmp_5 = fmul double  %zi, %zi\n   %tmp_6 = fmul double  %zr, %zr\n   %tmp_4 = fadd double  %tmp_5, %tmp_6\n   %tmp_3 = fcmp ole double  4.0, %tmp_4\n   br i1 %tmp_3, label %if_branch_21, label %else_branch_22\n\nif_branch_21:\n   %tmp_8 = srem i32  %m, 10\n   %tmp_7 = add i32  48, %tmp_8\n   call void @print_char (i32 %tmp_7)\n   ret void\n\nelse_branch_22:\n   %tmp_10 = add i32  %m, 1\n   %tmp_13 = fmul double  %zr, %zr\n   %tmp_14 = fmul double  %zi, %zi\n   %tmp_12 = fsub double  %tmp_13, %tmp_14\n   %tmp_11 = fadd double  %x, %tmp_12\n   %tmp_17 = fmul double  %zr, %zi\n   %tmp_16 = fmul double  2.0, %tmp_17\n   %tmp_15 = fadd double  %tmp_16, %y\n   call void @m_iter (i32 %tmp_10, double %x, double %y, double %tmp_11, double %tmp_15)\n   ret void\n}\n\ndefine void @x_iter (double %x, double %y) {\n   %tmp_24 = load double, double* @Xmax\n   %tmp_23 = fcmp ole double  %x, %tmp_24\n   br i1 %tmp_23, label %if_branch_30, label %else_branch_31\n\nif_branch_30:\n   call void @m_iter (i32 0, double %x, double %y, double 0.0, double 0.0)\n   %tmp_27 = load double, double* @Xstep\n   %tmp_26 = fadd double  %x, %tmp_27\n   call void @x_iter (double %tmp_26, double %y)\n   ret void\n\nelse_branch_31:\n   call void @skip ()\n   ret void\n}\n\ndefine void @y_iter (double %y) {\n   %tmp_33 = load double, double* @Ymax\n   %tmp_32 = fcmp ole double  %y, %tmp_33\n   br i1 %tmp_32, label %if_branch_41, label %else_branch_42\n\nif_branch_41:\n   %tmp_34 = load double, double* @Xmin\n   call void @x_iter (double %tmp_34, double %y)\n   call void @print_char (i32 10)\n   %tmp_38 = load double, double* @Ystep\n   %tmp_37 = fadd double  %y, %tmp_38\n   call void @y_iter (double %tmp_37)\n   ret void\n\nelse_branch_42:\n   call void @skip ()\n   ret void\n}\n\ndefine i32 @main() {\n   %tmp_43 = load double, double* @Ymin\n   call void @y_iter (double %tmp_43)\n   ret i32 0\n}"
    print(s1.equals(s2))



    // Displays output
//    println(result)
//    println(result)

  }
//def testf(n:Int)  =  n+1
//def testF(n:Int)  =  n+2

}
