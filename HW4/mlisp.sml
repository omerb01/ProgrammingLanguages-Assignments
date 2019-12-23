datatype Atom =
   SYMBOL of string | NUMBER of int | NIL;
datatype SExp =
   ATOM of Atom | CONS of (SExp * SExp);
local 
	fun printSpaces 0=""
	| printSpaces i="  "^printSpaces(i-1)
	fun SwithIndentations ((ATOM(NIL)),i)=""
	| SwithIndentations ((ATOM(NUMBER (n))),i)=Int.toString(n)
	| SwithIndentations ((ATOM(SYMBOL (id))),i)=id
	| SwithIndentations ((CONS(x,y)),i)=SwithIndentations(x,i)^"\n"^(printSpaces (i+1))^SwithIndentations(y,i+1);

in
	fun withIndentations (ATOM(NIL))=""
	| withIndentations (ATOM(NUMBER (n)))=Int.toString(n)
	| withIndentations (ATOM(SYMBOL (id)))=id
	| withIndentations (CONS(x,y))=SwithIndentations(x,0)^"\n"^(printSpaces 1)^SwithIndentations(y,1);
end;
val exp2 = CONS(ATOM(SYMBOL("a")), CONS(ATOM(SYMBOL("b")), CONS( CONS(ATOM(SYMBOL("c")),CONS(ATOM(SYMBOL("d")),ATOM(NIL))),ATOM(NIL))));
print(withIndentations exp2  );