(* Question 1*)
datatype vector = Vec of real list;
datatype matrix = Mat of real list list;
(* Part 1*)
local
	
	fun add ([]:real list,[]:real list)=[]
	|add ((x::xs),(y::ys))=((x+y)::add(xs,ys))
in
	fun ++ (Vec a,Vec b)=Vec(add(a,b))
end;
infix ++;
(* Part 2*)
local 
	fun mult ( [], [])=0.0
	|mult ( (x::xs), (y::ys))=((x*y)+mult( xs, ys));
in
	fun ** (Vec a, Vec b)=( mult(a,b))
end;
infix **;
(* Part 3*)
local
	fun scalarmult a []:real list=[]:real list
	| scalarmult a (x::xs):real list=((a*x)::((scalarmult a) xs))
in
	fun scalar_mult a=fn (Vec x)=>Vec ((scalarmult a) x)
end;
(* Part 4*)
fun normalize (Vec a)=scalar_mult (1.0/Math.sqrt(((Vec a)**(Vec a))))  (Vec a);