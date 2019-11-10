(* introduction to ML *)
fun curry f x y = f (x, y);
fun uncurry f(x,y) = f x y;

(* Signatures *)
fun steve (f, g) x = f(g(x));
fun tony (f:'a->string) = fn (x:'a, n:int) => n;
fun thor (x:''a) = fn (y:''a) => fn (z:''a) => x;
fun clint (x:real) (y:real) (f:real->'a) = f(x);
fun bruce (a:'a, b:'b) (c:'c) (d:'d, e:'e) = ((a, c), (b, e), d);
fun natasha (x:unit) (y:int) (z:unit) = y;