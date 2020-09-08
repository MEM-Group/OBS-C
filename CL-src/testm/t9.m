
fun foo(x,y){
  let t = if x > y then x else y ;
  t
    };

fun main() {
  let x = #allocate(2,4,5) ;
  let re = call(foo, x. 0, x. 1);
  assert(re == 5)
    };

call(main,)
