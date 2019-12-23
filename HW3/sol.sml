(* PART 1 *)

datatype vector = Vec of real list;
datatype matrix = Mat of real list list;

local
	
	fun add ([]:real list,[]:real list)=[]
	|add ((x::xs),(y::ys))=((x+y)::add(xs,ys))
in
	fun ++ (Vec a,Vec b)=Vec(add(a,b))
end;
infix ++;

local 
	fun mult ( [], [])=0.0
	|mult ( (x::xs), (y::ys))=((x*y)+mult( xs, ys));
in
	fun ** (Vec a, Vec b)=( mult(a,b))
end;
infix **;

local
	fun scalarmult a []:real list=[]:real list
	| scalarmult a (x::xs):real list=((a*x)::((scalarmult a) xs))
in
	fun scalar_mult a=fn (Vec x)=>Vec ((scalarmult a) x)
end;

fun normalize (Vec a)=scalar_mult (1.0/Math.sqrt(((Vec a)**(Vec a))))  (Vec a);

(* PART 2 *)

fun unique [] = []
    | unique (x::xs) =
    let
        fun pred n = (n<>x);
    in
        x :: (unique (List.filter pred xs))
    end;

fun max comparator (x::xs) =
    let
        fun getGreater (x,y) = if comparator (x,y) then x else y;
    in
        foldl getGreater x xs
    end;

fun min comparator (x::xs) =
    let
        fun getLower (x,y) = if comparator (x,y) then y else x;
    in
        foldl getLower x xs
    end;

fun sequence inputList f =
    let
        fun firstSequence [] = []
            | firstSequence [x] = [x]
            | firstSequence (x1::x2::xs) =
            if (f x1) = x2 then x1 :: firstSequence (x2::xs) else [x1];
        fun subLists [] = [[]]
            | subLists (x::xs) = (x::xs) :: (subLists xs);
        fun isLonger (l1,l2) = (List.length l1) > (List.length l2);
    in
        max isLonger (List.map firstSequence (subLists inputList))
    end;

fun histogram inputList =
    let
        fun count x l = List.length (List.filter (fn y => x=y) l);
        fun countList [] = []
            | countList (x::xs) = (x,count x inputList) :: countList xs;
    in
        unique (countList inputList)
    end;

fun common inputList =
    let
        fun isCommoner ((x,c1),(y,c2)) = c1 > c2;
        fun getFirst (x,y) = x;
    in
        getFirst (max isCommoner (histogram inputList))
    end;
