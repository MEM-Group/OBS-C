
fun foo(x){

  let y = #allocate(2,1,2);
  {
    let z = REF(y);
    (*z). 1 := 4
  }
  
};

call(foo,4)
