def fact(n) =
  (if n == 0 then 1 else n * fact(n - 1));

def facT(n, acc) =
  if n == 0 then acc else facT(n - 1, n * acc);

def facTi(n) = facT(n, 1);

//fact(10)
//facTi(10)

write(fact(6)); facTi(6)

// a simple factorial program
// (including a tail recursive version)
