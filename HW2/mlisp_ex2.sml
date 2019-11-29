(* Question 3 *)

exception Undefined;
fun initEnv()=fn str:string => raise Undefined;
fun define str1 env a=fn str2:string =>if (str2<=str1 andalso str1<=str2)  then a else env(str2);
