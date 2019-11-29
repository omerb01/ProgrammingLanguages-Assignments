(* Question 1 *)

fun implode(x:char list)=  foldr op^ "" (map (str) (x));
fun translate(x)=fn (s) =>concat( map(x) (explode(s)));
fun enumerate(x:'a list)=List.tabulate(length(x),fn a=>(a,List.nth(x,a)));
fun slice (array:'a list)=fn (start,fin,interval)=>map(fn (n,a) =>a) (List.filter( fn (n,a) => (n-start) mod interval=0 andalso n<=fin andalso start<=n) (enumerate(array)));
fun allholds_notholds (allholds:('a->bool) list)(notholds:('a->bool) list)(arr:'a list) =foldl (fn(f,y)=>(List.filter (fn x=>not(f(x))) y)) (foldl (fn ( x, y) =>List.filter x y) arr allholds) notholds;

(* Question 2 *)

fun interleave input = 
	if (null (hd input)) then
		[]
	else
		(map hd input) @ (interleave (map tl input));

fun valAt input i =
	if i=0 then
		hd input
	else
		valAt (tl input) (i-1);

fun valAt (x::xs) 0 = x
	| valAt (x::xs) i =
	valAt xs (i-1);

fun sumAtIndices input [] = 0
	| sumAtIndices input (i::is) =
	(valAt input i) + (sumAtIndices input is);

fun removeDupes [] = []
	| removeDupes (x::xs) =
	let
		fun pred n = (n<>x);
	in
		x :: (removeDupes (List.filter pred xs))
	end;
