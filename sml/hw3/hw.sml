val only_capitals = List.filter (fn s => Char.isUpper (String.sub(s,0)))

val longest_string1 = 
    List.foldl (fn (s,sofar) => if String.size s > String.size sofar
				then s
				else sofar) 
	       ""

val longest_string2 = 
    List.foldl (fn (s,sofar) => if String.size s >= String.size sofar
				then s
				else sofar) 
	       ""

fun longest_string_helper f = 
    List.foldl (fn (s,sofar) => if f(String.size s,String.size sofar)
				then s
				else sofar) 
	       ""

(* but let's require val-bindings for these last 4 *)

val longest_string3 = longest_string_helper (fn (x,y) => x > y) (* or op> *)
val longest_string4 = longest_string_helper (fn (x,y) => x >= y) (* or op>= *)

val longest_string3b = longest_string_helper op>
val longest_string4b = longest_string_helper op>=

val longest_capitalized = longest_string1 o only_capitals

val rev_string = String.implode o rev o String.explode

(* here they write their own higher-order functions *)
fun first_answer f xs =
    case xs of
	[] => raise NoAnswer
      | x::xs' => case f x of NONE => first_answer f xs'
			    | SOME y => y

fun all_answers f xs =
    let fun loop (acc,xs) =
	    case xs of
		[] => SOME acc
	      | x::xs' => case f x of 
			      NONE => NONE
			    | SOME y => loop((y @ acc), xs')
    in loop ([],xs) end

(* here they use my higher-order function *)
fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

val count_wildcards = g (fn () => 1) (fn _ => 0)
val count_wild_and_variable_lengths = g (fn () => 1) String.size
(* this is the only one that uses a free variable, but also a couple
   later uses with the pattern matching *)
fun count_some_var (x,p) = g (fn () => 0) (fn s => if s = x then 1 else 0) p

(* here they do something useful for implementing languages *)
fun check_pat pat = 
    let fun get_vars pat =
	    case pat of
		Variable s => [s]
	      | TupleP ps  => List.foldl (fn (p,vs) => get_vars p @ vs) [] ps
	      | ConstructorP(_,p) => get_vars p
	      | _          => []
	fun unique xs = (* note: could also use count_some_var *)
	    case xs of
		[]     => true
	      | x::xs' => (not (List.exists (fn y => y=x) xs'))
			  andalso unique xs'
    in 
	unique (get_vars pat)
    end

fun match (valu,pat) =
    case (valu,pat) of
	(_,Wildcard)    => SOME []
      | (_,Variable(s)) => SOME [(s,valu)]
      | (Unit,UnitP)    => SOME []
      | (Const i, ConstP j)    => if i=j then SOME [] else NONE
      | (Tuple(vs),TupleP(ps)) => if length vs = length ps
				  then all_answers match (ListPair.zip(vs,ps))
				  else NONE
      | (Constructor(s1,v), ConstructorP(s2,p)) => if s1=s2
						   then match(v,p)
						   else NONE
      | _ => NONE

fun first_match valu patlst =
    SOME (first_answer (fn pat => match (valu,pat)) patlst)
    handle NoAnswer => NONE

(**** for the challenge problem ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

fun uncurry f (a,b) = f a b

(* turn a list of options into an option of a list *)
fun optL [] acc = SOME acc
  | optL ((SOME x)::r) acc = optL r (acc@[x])
  | optL (NONE::r) acc = NONE

fun optList x = optL x []

(* reduce operator on types *)
fun unify Anything x = SOME x
  | unify x Anything = SOME x
  | unify UnitT UnitT = SOME UnitT
  | unify IntT IntT = SOME IntT
  | unify (Datatype s) (Datatype t) = if s=t then SOME (Datatype t) else NONE
  | unify (TupleT t1) (TupleT t2) = if (length t1) <> (length t2) then NONE
				    else let 
					    val t = optList (List.map (uncurry unify) (ListPair.zip (t1,t2)))
					in
					    case t of
						SOME l => SOME (TupleT l)
					      | NONE => NONE
					end
  | unify _ _ = NONE

fun optunify (SOME p) (SOME p2) = unify p p2
  | optunify _ _ = NONE					     

fun typecheck_patterns (bindings,pats) = let
    fun matches [] s (SOME t) = NONE
      | matches ((cn,tn,ct)::bs) s (SOME t) = if cn=s andalso (ct=t orelse ct=Anything)
				       then SOME (Datatype tn)
				       else matches bs s (SOME t)
      | matches _ _ NONE = NONE
    fun getType (Wildcard) = SOME Anything
      | getType (Variable s) = SOME Anything
      | getType (UnitP) = SOME UnitT
      | getType (ConstP i) = SOME IntT
      | getType (TupleP pl) = (case optList (List.map getType pl) of
				  SOME l => SOME (TupleT l)
				| NONE => NONE)
      | getType (ConstructorP (s,p)) = matches bindings s (getType p)
in
    List.foldl (uncurry optunify) (SOME Anything) (List.map getType pats)
end