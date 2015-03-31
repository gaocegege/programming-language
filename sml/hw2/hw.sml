fun all_except_option (str, lst) = 
	case lst of 
		nil => NONE
	|	ele::lst => 
			if str = ele then SOME lst
			else case all_except_option(str, lst) of 
				NONE => NONE
			|	SOME e => SOME (ele::e)

fun get_substitutions1 (lstlst, str) =
	case lstlst of
		nil => []
		| ele::lst => 
			case all_except_option(str, ele) of
				NONE => get_substitutions1(lst, str)
				| SOME e => e@get_substitutions1(lst, str)

fun get_substitutions2 (lstlst, str) =
	let
		fun helper (lst_lst, lst) =
			case lst_lst of
				nil => lst
				| ele::rest => case all_except_option(str, ele) of
					NONE => helper(rest, lst)
					| SOME e => helper(rest, lst @ e)
	in
		helper(lstlst, [])
	end

fun similar_names (lstlst, {first=f, middle=m, last=l}) =
	let
		fun helper (lst) =
			case lst of
				nil => []
				| ele::rest => {first=ele, middle=m, last=l}::helper(rest)
	in
		helper(get_substitutions1(lstlst, f)@[f])
	end

fun card_color (s, n) =
	case s of
		Diamonds => Red
		| Hearts => Red
		| _ => Black

fun card_value (s, n) =
	case n of
		Ace => 11
		| Num i => i
		| _ => 10

fun remove_card (cs, c, e) =
	case cs of
		nil => raise e
		| ele::rest => 
			if ele = c then rest
			else ele::remove_card(cs, c, e)

fun all_same_color (lst: card list) =
	case lst of
		nil => true
		| ele::nil => true
		| ele1::ele2::rest => if card_color(ele1) = card_color(ele2) then all_same_color(ele2::rest)
						else false

fun sum_cards (lst) =
	let
		fun sum (lst) =
			case lst of
				nil => 0
				| ele::rest => card_value(ele) + sum(rest)
	in
		sum(lst)
	end

fun score (lst, goal) =
	let
		val sum = sum_cards(lst)
		val s = if sum > goal then 3 * (sum - goal) else goal - sum
	in
		if all_same_color(lst)
		then s div 2
		else s
	end

fun officiate (card_list, move_list, goal) =
	let
		fun helper (cards, moves, helds) =
			case moves of
				nil => score(helds, goal)
				| ele::rest => case ele of
					Discard e => helper(cards, rest, remove_card(helds, e, IllegalMove))
					| Draw => case cards of
						nil => score(helds, goal)
						| eleCard::restCards => 
						let
							val newHelds = eleCard::helds
						in
							if sum_cards(newHelds) >= goal
							then score(newHelds, goal) 
							else helper(restCards, rest, newHelds)
						end
	in
		helper(card_list, move_list, [])
	end

fun score_challenge(cards, goal) =
	let 
		fun card_value_new(card, value) = 
			case card of
			(_, Num v) => v
			|(_, Ace) => value
			|_ => 10

		fun sum_cards_new(lst, value) =
			let
				fun sum (lst) =
					case lst of
					nil => 0
					| ele::rest => card_value_new(ele, value) + sum(rest)
			in
				sum(lst)
			end

		fun score_new(value) =
			let 
				val buf = sum_cards_new(cards, value)
			in 
				if buf > goal then 3 * (buf - goal) else goal - buf
			end 

		val score1 = score_new(1)
		val score11 = score_new(11)
		val realScore = if score1 < score11 then score1 else score11

		fun score_new_real r =
			if all_same_color(cards) 
			then r div 2
			else r
	in
		score_new_real(realScore)
	end

fun officiate_challenge (card_list, move_list, goal) =
	let
		fun card_value_new(card, value) = 
			case card of
			(_, Num v) => v
			|(_, Ace) => value
			|(_,_) => 10

		fun sum_cards_new(lst, value) =
			let
				fun sum (lst) =
					case lst of
					nil => 0
					| ele::rest => card_value_new(ele, value) + sum(rest)
			in
				sum(lst)
			end

		fun helper (cards, moves, helds) =
			case moves of
				nil => score_challenge(helds, goal)
				| ele::rest => case ele of
					Discard e => helper(cards, rest, remove_card(helds, e, IllegalMove))
					| Draw => case cards of
						nil => score_challenge(helds, goal)
						| eleCard::restCards => 
						let
							val newHelds = eleCard::helds
						in
							if sum_cards_new(newHelds, 1) >= goal andalso sum_cards_new(newHelds, 11) >= goal
							then score_challenge(newHelds, goal) 
							else helper(restCards, rest, newHelds)
						end
	in
		helper(card_list, move_list, [])
	end

fun careful_player (card_list, goal) =
	let
		fun possible_to_discard(held_card,check_list,card) =
			case check_list of
				nil => (false, (Diamonds, Num 1)) 
				|ele::rest => if score(card::remove_card(held_card, ele, IllegalMove), goal) = 0
							then (true, ele)
							else possible_to_discard(held_card, rest, card)

		fun make_move(held_list,card_list,move_list) =
			if score(held_list,goal) = 0
			then move_list
			else  
				case card_list of
				nil => move_list
				|x::xs => 	case held_list of 
							nil => make_move(x::held_list,xs,move_list@[Draw])
							|y::ys => case  possible_to_discard(held_list,held_list,x) of
									(true,x1) => move_list@[(Discard x1),Draw]
									|_ => if ((goal - sum_cards(held_list) > 10) orelse
											(sum_cards(x::held_list) <= goal))
											then make_move(x::held_list,xs,move_list@[Draw])
											else make_move(x::held_list,xs,move_list@[Discard y])
	in
		make_move([],card_list,[])
	end