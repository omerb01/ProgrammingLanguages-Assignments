datatype Atom =
   SYMBOL of string | NUMBER of int | NIL;
datatype SExp =
   ATOM of Atom | CONS of (SExp * SExp);
 (* question 2.1 *)
local 
	fun printSpaces 0=""
	| printSpaces i="  "^printSpaces(i-1)
	fun SwithIndentations ((ATOM(NIL)),i)=""
	| SwithIndentations ((ATOM(NUMBER (n))),i)=Int.toString(n)
	| SwithIndentations ((ATOM(SYMBOL (id))),i)=id
	| SwithIndentations ((CONS(x,(ATOM(NIL))),i))=SwithIndentations(x,i)^"\n"
	| SwithIndentations ((CONS(x,y)),i)=SwithIndentations(x,i)^"\n"^(printSpaces (i))^SwithIndentations(y,i+1);

in
	fun withIndentations (ATOM(NIL))=""
	| withIndentations (ATOM(NUMBER (n)))=Int.toString(n)
	| withIndentations (ATOM(SYMBOL (id)))=id
	| withIndentations (CONS(x,(ATOM(NIL))))=withIndentations(x)^"\n"
	| withIndentations (CONS(x,y))=SwithIndentations(x,0)^"\n"^(printSpaces 1)^SwithIndentations(y,1);
end;

val exp2 = CONS(ATOM(SYMBOL("a")), CONS(ATOM(SYMBOL("b")), CONS( CONS(ATOM(SYMBOL("c")),CONS(ATOM(SYMBOL("d")),ATOM(NIL))),ATOM(NIL))));
print(withIndentations exp2  );
val exp1=CONS(CONS(ATOM(SYMBOL("A")),ATOM(SYMBOL("B"))),CONS(ATOM(SYMBOL("C")),ATOM(SYMBOL("D"))));
val exp1=(CONS(ATOM(SYMBOL("A")),(CONS(ATOM(SYMBOL("B")),CONS(ATOM(SYMBOL("C")),ATOM(SYMBOL("D")))))));
 (* question 2.2 *)
fun   asFunctional (ATOM(NIL))=""
	| asFunctional (ATOM(NUMBER(n)))=Int.toString(n)
	| asFunctional (ATOM(SYMBOL(id)))=id
	| asFunctional (CONS(x,(ATOM(NIL))))=(asFunctional x)
	| asFunctional (CONS(x,CONS(CONS(y,z),w))) = (asFunctional x)^","^(asFunctional (CONS(y,z)))^(asFunctional w) 
	| asFunctional (CONS(x,y))=(asFunctional x)^"("^(asFunctional y)^")";
	
(* question 2.3*)

	
	