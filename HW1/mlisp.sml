(* Part 1 *)
fun isPascalInteger (str:string) = 
	if size(str)=0 then
		true
	else 
		if Char.isDigit(String.sub(str, size(str)-1)) then
			isPascalInteger(substring(str, 0, size(str)-1))
		else
			false

(* Part 2 *)
fun isLeft (c:char) = Char.compare(c, #"(")=EQUAL;
fun isRight (c:char) = Char.compare(c, #")")=EQUAL;
fun step1 (str:string) = String.concatWith " ( " (String.fields isLeft str);
fun step2 (str:string) =String.concatWith " ) " (String.fields isRight str);
fun step3 (str:string) = String.tokens Char.isSpace str;
fun tokenize (program:string) = step3(step2(step1(program)));
