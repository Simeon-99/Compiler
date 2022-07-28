.class public test.test
.super java/lang/Object

.method public static write(I)V 
    .limit locals 1 
    .limit stack 2
    getstatic java/lang/System/out Ljava/io/PrintStream; 
    iload 0 
    invokevirtual java/io/PrintStream/println(I)V 
    return 
.end method

.method public static main([Ljava/lang/String;)V
   .limit locals 200
   .limit stack 200

   ; 1 + ((2 * 3) + (4 - 3))
   ldc 5       
   ldc 6
   ldc 7
   imul
   ldc 4
   ldc 3
   isub
   iadd
   iadd
   invokestatic test/test/write(I)V 
   return
.end method
