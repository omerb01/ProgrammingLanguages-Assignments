exception MlispError;

local
	fun getFirstElement ((CONS(exp1,exp2)) ,env)= exp1
	|   getFirstElement (_) = raise MlispError;
	
	fun getSecondElement ((CONS(exp1,exp2)),env)= exp2
	|   getSecondElement (_) = raise MlispError;
	

	fun add ((ATOM (NUMBER (x))),env1) (ATOM (NUMBER (y)),env2)= (ATOM (NUMBER(x+y)))
		| add _ _=raise MlispError;
	fun sub ((ATOM (NUMBER (x))),env1) (ATOM (NUMBER (y)),env2)= (ATOM (NUMBER(x-y)))
		| sub _ _=raise MlispError;
	fun mul ((ATOM (NUMBER (x))),env1) (ATOM (NUMBER (y)),env2)= (ATOM (NUMBER(x*y)))
		| mul _ _=raise MlispError;
	fun divr ((ATOM (NUMBER (x))),env1) (ATOM (NUMBER (0)),env2)=raise MlispError (*checking if dividing by zero*)
		|divr ((ATOM (NUMBER x)),env1) (ATOM (NUMBER y),env2)= (ATOM (NUMBER(x div y)))
		|divr _ _=raise MlispError;
	fun consize (x,env1) (y,env2)= (CONS(x,y));

	fun atomToString (ATOM(SYMBOL(x))) = x
	|   atomToString _ = raise MlispError;

	fun numToAtom x = (ATOM(NUMBER(x)));
	fun makeEnv parameters values env =
	let
		fun InnerBuildEnv (ATOM(SYMBOL(x))) (ATOM(NUMBER(y))) env = assoc (x, numToAtom(y)) env (*simple case*)
		|   InnerBuildEnv x (CONS(y,ATOM(NIL))) env = InnerBuildEnv x y env                         (*need to remove nil from values*)
		|   InnerBuildEnv (CONS(x,ATOM(NIL))) y env = InnerBuildEnv x y env                         (*need to remove nil from parameters*)
		|   InnerBuildEnv (CONS(x,z)) (CONS(y,w)) env = InnerBuildEnv z w (assoc (atomToString(x), z) env) (*main recursion*)
		|   InnerBuildEnv _ _ _ = raise MlispError
	in
		InnerBuildEnv parameters values env
	end;
	fun aux_eval((ATOM(NIL)),env)=(ATOM(NIL), env)
		| aux_eval((ATOM(NUMBER(x))),env)=((ATOM(NUMBER(x))),env)
		| aux_eval((ATOM(SYMBOL(x))),env)=if (x= "nil") then (ATOM(NIL), env) else ((find (x) (env)),env)
		| aux_eval ((CONS(exp,ATOM(NIL))) ,env)   = (aux_eval (exp, env))
		| aux_eval ((CONS((ATOM(SYMBOL "+")),(CONS(x,y)))),env)=(add (aux_eval (x ,env)) (aux_eval (y ,env)), (env))	
		| aux_eval ((CONS((ATOM(SYMBOL "-")),(CONS(x,y)))),env)=(sub (aux_eval (x ,env)) (aux_eval (y ,env)), (env))
		| aux_eval ((CONS((ATOM(SYMBOL "*")),(CONS(x,y)))),env)=(mul (aux_eval (x ,env)) (aux_eval (y ,env)), (env))
		| aux_eval ((CONS((ATOM(SYMBOL "div")),(CONS(x,y)))),env)=(divr (aux_eval (x ,env)) (aux_eval (y ,env)), (env))
		| aux_eval ((CONS((ATOM(SYMBOL "cons")),(CONS(x,y)))),env)=(consize (aux_eval (x ,env)) (aux_eval (y ,env)), (env))
		(*| aux_eval ((CONS((ATOM(SYMBOL "cond")),x)),env)=condEval*)
		| aux_eval ((CONS((ATOM (SYMBOL "car")),(CONS(x,_)))),env)= (getFirstElement (aux_eval (x,env)),(env))
		| aux_eval ((CONS((ATOM (SYMBOL "cdr")),x)),env)= cdrEval(x,env)
		| aux_eval ((CONS(ATOM(SYMBOL("define")), exp)),env)= ((defineEval (exp, env)))		
		| aux_eval ((CONS(ATOM(SYMBOL(x)),( exp))),env)= (symbolEval ((CONS(ATOM(SYMBOL(x)), exp)), env))
		| aux_eval (_,_) = raise MlispError
	and cdrEval(CONS(CONS(ATOM(SYMBOL("cons")),CONS(_, CONS(y, ATOM(NIL)))), ATOM(NIL)) ,env) = (aux_eval (y,env))
	|   cdrEval (_,_) = raise MlispError	
	and defineEval (CONS(ATOM(SYMBOL(x)),CONS(exp1,CONS(exp2,ATOM(NIL)))) , env) = (ATOM(NIL), (assoc (x, (CONS(exp1,exp2))) env))
	|   defineEval (CONS(ATOM(SYMBOL(x)),CONS(exp1,ATOM(NIL))) , env) = (ATOM(NIL), (assoc (x, (#1 (aux_eval(exp1, env)))) env))
	|   defineEval (_, _) = raise MlispError

	and symbolEval ((CONS(ATOM(SYMBOL(func)), args)) ,env) =
		let
			val newEnv=makeEnv (getFirstElement((find func env),env)) (#1 (aux_eval (CONS(ATOM(SYMBOL("cons")), args), env))) env;
		in
			(#1 (aux_eval (getSecondElement((find func newEnv),newEnv),newEnv)), env)
		end
		|  symbolEval (_,_) = raise MlispError
in
	fun eval (s:SExp) (env:SExp AssociationList):SExp*SExp AssociationList = aux_eval(s , env) handle _ => raise MlispError;
end;