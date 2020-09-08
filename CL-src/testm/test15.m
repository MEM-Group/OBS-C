
fun foo(a,b) {
   let t = a. 1 + b. 2
};

fun main() {
  let a = #allocate(3,1,2,4);
  let b = REF(a);
  let c = REF(a);
  a. 2 := 7;
  (*b). 1 := 6
    };

call(main,)

