datatype calcTree =
    Lf
    | Poly of (int * int) * calcTree * calcTree;

(* question 1.1*)
fun mapTree f Lf=Lf
  | mapTree f (Poly(p,t1,t2))=Poly(f(p),mapTree f t1,mapTree f t2);
 (* question 1.2*)
local
	fun makeList Lf = []
	|   makeList (Poly(p,t1,t2)) = ((makeList t1)@[p]@(makeList t2))
	
	fun makeTree [] = Lf 
		|makeTree (x::xs) = Poly(x,Lf,makeTree xs)
in
	fun filterTree predict tree=makeTree(List.filter predict (makeList( tree))) 
end;
(* question 1.3 *)
local
(*fun cmp(_,k1) (_,k2)=Int.compare (k1,k2)*)
fun isZero (a,n) = if (a=0) then false else true
in
fun insert  (a,n) Lf=Poly((a,n),Lf,Lf)
    |   insert (a,n) (Poly((a1,n1),t1,t2)) =
        case Int.compare(n,n1) of
			EQUAL => filterTree isZero (Poly((a+a1,n),t1,t2))
			| GREATER => Poly(((a1,n1),t1,insert (a,n) t2))
			| LESS => Poly((a1,n1),insert (a,n) t1,t2)
end;

(* question 1.4*)
local
	fun isZero (a,n) = if (a=0) then false else true
    fun derivative (a,0)=(0,0)
    |   derivative (a,n)=(a*n,n-1)
in
	fun divTree (Poly((a,n),t1,t2))=filterTree isZero (mapTree derivative (Poly((a,n),t1,t2)))
end; 
(*question 1.5 *)
local
	fun intPow(a,0)=1
	| intPow (a,n)=a*intPow(a,n-1)
in
	fun evalTree x Lf=0
	| evalTree x (Poly((a,n),t1,t2))=a*(intPow(x,n))+evalTree x t1 +evalTree x t2
end;
