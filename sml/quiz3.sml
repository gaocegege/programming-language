(*exception E
val x = 1
fun f x = if x=2 then raise E else 14
val x = 2
val ans = ( ( f x ) + 4 ) handle E => 9

fun majority f lst =
	let
		fun helper acc lstlst =
			case lstlst of
				[] => acc
				| x::xs => case f(x) of
					true => helper (1 + acc) lstlst
					| else => helper acc lstlst
	in
	end*)

fun fold f acc xs =
   case xs of 
     []    => acc
   | x::xs => fold f (f(x, acc)) xs

fun foldr f acc xs =
	case xs of
		[] => acc
		| x::xs => f(x, (foldr f acc xs))