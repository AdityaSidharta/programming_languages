(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

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

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

fun only_capitals(input_strings) = let 
                                    fun f x = Char.isUpper(String.sub(x, 0));
                                   in List.filter f input_strings
                                   end

fun longest_string1(input_strings) = let fun f (x1, acc) = if String.size x1 > String.size acc then x1 else acc
                                     in List.foldl f "" input_strings
                                     end

fun longest_string2(input_strings) = let fun f (x1, acc) = if String.size x1 >= String.size acc then x1 else acc
                                     in List.foldl f "" input_strings
                                     end

fun longest_string_helper f input_strings = let fun g(x1, acc) = if f(String.size x1, String.size acc) then x1 else acc
                                            in List.foldl g "" input_strings
                                            end

val longest_string3 = longest_string_helper (fn (x,y) => x > y)

val longest_string4 = longest_string_helper (fn (x,y) => x >= y)

val longest_capitalized = longest_string1 o only_capitals

val rev_string = String.implode o List.rev o String.explode

fun first_answer f input_list = case input_list of
                                    [] => raise NoAnswer
                                  | x::xs => (case f x of 
                                                NONE => first_answer f xs
                                              | SOME v => v)

fun all_answers f input_list = let fun aux(f, input_list, acc, indicator) = 
                                        case input_list of 
                                            [] => SOME acc
                                          | x::xs => (let val indiv_result = f(x);
                                                          val new_indicator = indicator orelse Bool.not(isSome indiv_result);
                                                     in if new_indicator then NONE else aux(f, xs, acc @ valOf(indiv_result), new_indicator)
                                                     end )
                               in aux(f, input_list, [], false)
                               end

val count_wildcards = g (fn _ => 1) (fn _ => 0) 

val count_wild_and_variable_lengths = g (fn _ => 1) (fn x => String.size x)

fun count_some_var(input_string, input_pattern) = g (fn _ => 0) (fn x => if x = input_string then 1 else 0) input_pattern

fun get_all_vars(input_pattern) = case input_pattern of
                                      Variable x => [x]
                                    | TupleP ps => List.foldl (fn (p1, acc) => get_all_vars(p1) @ acc) [] ps
                                    | ConstructorP (_, p) => get_all_vars(p)
                                    | _ => []

fun is_unique(input_list) = let fun aux(input_list, acc) = 
                                if Bool.not(acc) then false else
                                case input_list of
                                    [] => true
                                  | x::xs => aux(xs, Bool.not(List.exists (fn y => x = y) xs))
                            in aux(input_list, true)
                            end

val check_pat = is_unique o get_all_vars

fun match(v, p) = 
    case (v, p) of 
        (_, Wildcard) => SOME []
      | (_, Variable s) => SOME [(s, v)]
      | (Unit, UnitP) => SOME []
      | (_, UnitP) => NONE
      | (Const x, ConstP y) => if x = y then SOME [] else NONE
      | (_, ConstP x) => NONE
      | (Tuple vs, TupleP ps) => let val pairs = ListPair.zip(vs, ps);
                                     val result = all_answers (fn (v1,p1) => match(v1, p1)) pairs;
                                in if List.length(vs) = List.length(ps) then result else NONE
                                end  
      | (_, TupleP ps) => NONE
      | (Constructor (s1, vi), ConstructorP (s2, pi)) => if s1 = s2 then match(vi, pi) else NONE
      | (_, ConstructorP (s2, p)) => NONE

fun first_match v ps = let fun partialmatch(input_ps) = match(v, input_ps)
                         in SOME (first_answer partialmatch ps) handle NoAnswer => NONE
                         end