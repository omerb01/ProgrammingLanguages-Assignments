datatype 'a seq = Nil | Cons of 'a * (unit-> 'a seq);

exception EmptySeq;
fun head(Cons(x,_)) = x | head Nil = raise EmptySeq;
fun tail(Cons(_,xf)) = xf() | tail Nil = raise EmptySeq;

datatype direction = Back | Forward;
datatype 'a bseq =   bNil
                | bCons of 'a * (direction -> 'a bseq);

fun bHead(bCons(x,_)) = x | bHead bNil = raise EmptySeq;
fun bForward(bCons(_,xf)) = xf(Forward) | bForward bNil = raise EmptySeq;
fun bBack(bCons(_,xf)) = xf(Back) | bBack bNil = raise EmptySeq;

fun intbseq x =
	let
		fun move Forward = intbseq (x+1)
			| move Back = intbseq (x-1);
	in
		bCons (x, move)
	end;

intbseq 2;
bForward(it);
bForward(it);
bBack(it);
bBack(it);
bBack(it);
bBack(it);
bBack(it);

fun bmap f bseq =
	let
		val x = bHead bseq;
		fun xf Forward = bForward bseq | xf Back = bBack bseq;
		fun move direction = bmap f (xf (direction));
	in
		bCons (f (x), move)
	end;

bmap (fn x =>x*x) (intbseq 2);
bForward(it);
bForward(it);
bBack(it);
bBack(it);
bBack(it);
bBack(it);
bBack(it);
bBack(it);

fun bfilter pred d bseq =
	let
		val x = bHead bseq;
		fun xf Forward = bForward bseq | xf Back = bBack bseq;
		fun move d = bfilter pred d (xf d);
	in
		if pred (x) then bCons(x, move) else bfilter pred d (xf d)
	end;

bfilter (fn x => x mod 2 = 0) Back (intbseq 2);
bForward(it);
bForward(it);
bBack(it);
bBack(it);
bBack(it);
bBack(it);
bfilter (fn x => x mod 2 = 0) Back (intbseq 1);
bfilter (fn x => x mod 2 = 0) Forward (intbseq 1);

fun seq2bseq (Cons(x,xf)) (Cons(y,yf)) =
	let
		fun move_rev () = Cons(x,xf);
		fun move_reg () = Cons(y,yf);
		fun move Forward = seq2bseq (Cons(y,move_rev)) (yf())
			| move Back = seq2bseq (xf()) (Cons(x,move_reg));
	in
		bCons(y, move)
	end;

fun from(x) = Cons(x, fn()=>from(x+1));
fun downfrom(x) = Cons(x, fn()=>downfrom(x-1));
seq2bseq (downfrom ~2) (from 0);
bForward(it);
bBack(it);
bBack(it);



















