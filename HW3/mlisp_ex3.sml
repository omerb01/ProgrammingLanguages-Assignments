exception Undefined;
type ('a) AssociationEntry = (string * 'a);
type ('a) AssociationList = 'a AssociationEntry list;

fun assoc (entry:('a AssociationEntry)) (env:('a AssociationList)) = ((entry :: env):('a AssociationList));

fun find str [] = raise Undefined
	| find str ((entry:('a AssociationEntry))::(entries:('a AssociationList))) =
		let
			fun getVar (var,value) = var;
			fun getValue (var,value) = value;
		in
			if (getVar entry)=str then (getValue entry) else find str entries
		end;
