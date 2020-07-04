(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2;

fun rev lst =
    let fun aux(lst,acc) =
        case lst of 
        [] => acc
    |   x::xs => aux(xs, x::acc)
    in aux(lst,[])
    end

(* put your solutions for problem 1 here *)

fun all_except_option(input_string, available_strings) = 
    let fun aux(input_string, available_strings, acc_list, acc_bool) = 
        case available_strings of 
            [] => let val rev_list = rev(acc_list)
                  in if acc_bool then SOME rev_list else NONE
                  end            
          | x::xs' => if same_string(x, input_string) then aux(input_string, xs', acc_list, true) else aux(input_string, xs', x::acc_list, acc_bool)
    in aux(input_string, available_strings, [], false)
    end;

fun get_substitutions1(list_aliases, name) = 
    case list_aliases of
        [] => []
      | x::xs => let val result = all_except_option(name, x);
                in case result of 
                    NONE => get_substitutions1(xs, name)
                |   SOME r => r @ get_substitutions1(xs, name)
                 end;

fun get_substitutions2(list_aliases, name) = 
    let fun aux(list_aliases, name, acc) = 
        case list_aliases of
            [] => acc
          | x::xs => let val result = all_except_option(name, x);
                     in case result of
                        NONE => aux(xs, name, acc)
                      | SOME r => aux(xs, name, acc @ r)
                     end
    in aux(list_aliases, name, [])
    end;

fun similar_names(list_aliases, full_name) = 
    let val {first=first_name, middle=middle_name, last=last_name} = full_name;
        val other_aliases = get_substitutions2(list_aliases, first_name)
        fun iterate_names(aliases, full_name) = 
            case aliases of 
                [] => []
            | x::xs => {first=x, middle=middle_name, last=last_name}::iterate_names(xs, full_name)
    in iterate_names(first_name::other_aliases, full_name)
    end;

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color(c) = 
    case c of
        (Clubs, _) => Black
      | (Spades, _) => Black
      | (Hearts, _) => Red
      | (Diamonds, _) => Red;

fun card_colors(cs) =
    case cs of
        [] => []
      | x::xs => card_color(x)::card_colors(xs);

fun card_value(c) =
    case c of
        (_, Jack) => 10
      | (_, Queen) => 10
      | (_, King) => 10
      | (_, Ace) => 11
      | (_, Num r) => r;


fun remove_card(cs, c, e) =
    let fun aux(remain_cs, c, e, checked_cs) =
            case remain_cs of 
                [] => raise e
              | x::xs => let val new_checked_cs = rev(x::checked_cs);
                         in if c = x then checked_cs @ xs else aux(xs, c, e, new_checked_cs)
                         end 
    in aux(cs, c, e ,[])
    end;

fun all_same_color(cs) =
    case cs of
        [] => true
      | x::[] => true
      | x::y::[] => card_color(x) = card_color(y)
      | x::y::ys => if card_color(x) = card_color(y) then all_same_color(y::ys) else false;

fun sum_cards(cs) = 
    let fun aux(cs, acc) = 
        case cs of 
            [] => acc
          | x::xs => aux(xs, card_value(x) + acc)
    in aux(cs, 0)
    end;

fun score(held_cs, goal) = 
    let 
        val held_sum = sum_cards(held_cs);
        val held_same_color = all_same_color(held_cs);
        val prelim_score = if held_sum > goal then 3 * (held_sum - goal) else (goal - held_sum)
    in if held_same_color then prelim_score div 2 else prelim_score
    end;


fun officiate(decks, moves, goal) = 
    let fun aux(decks, moves, goal, held_cards) = 
        case moves of
            [] => score(held_cards, goal) 
          | Draw::xs => (case decks of 
                            [] => score(held_cards, goal)
                         |  y::ys => aux(ys, xs, goal, y::held_cards))
          | Discard(z)::zs => let val remain_held_cards = remove_card(held_cards, z, IllegalMove);
                             in aux(decks, zs, goal, remain_held_cards) 
                             end
    in aux(decks, moves, goal, [])
    end;