(* Homework2 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

use "hw_2.sml";

val test1 = all_except_option ("string", ["string"]) = SOME []
val test1a = all_except_option ("string", ["world", "string", "hello"]) = SOME ["world", "hello"]
val test1b = all_except_option ("string", ["string", "world", "hello"]) = SOME ["world", "hello"]
val test1c = all_except_option ("string", ["world", "hello", "string"]) = SOME ["world", "hello"]
val test1d = all_except_option ("string", ["string", "hello"]) = SOME ["hello"]
val test1e = all_except_option ("string", ["strings", "hello"]) = NONE
val test1f = all_except_option ("string", []) = NONE

val test2 = get_substitutions1 ([["foo"],["there"]], "foo") = []
val test2a = get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],"Fred") = ["Fredrick","Freddie","F"]
val test2b = get_substitutions1([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],"Jeff") = ["Jeffrey","Geoff","Jeffrey"]

val test3 = get_substitutions2 ([["foo"],["there"]], "foo") = []
val test3a = get_substitutions2([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],"Fred") = ["Fredrick","Freddie","F"]
val test3b = get_substitutions2([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],"Jeff") = ["Jeffrey","Geoff","Jeffrey"]

val test4 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
	     {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]

val test5 = card_color (Clubs, Num 2) = Black

val test6 = card_value (Clubs, Num 2) = 2
val test6a = card_value (Clubs, Jack) = 10
val test6b = card_value (Clubs, Ace) = 11

val test7 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []

val test8 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true
val test8a = all_same_color [(Spades, Ace), (Hearts, Ace), (Diamonds, King)] = false
val test8b = all_same_color [(Hearts, King), (Hearts, Ace), (Diamonds, King)] = true
val test8c = all_same_color [(Hearts, Ace)] = true

val test9 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4

val test10 = score ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4
val test10a = score ([(Hearts, Num 2),(Diamonds, Num 4)],10) = 2
val test10b = score ([(Hearts, Ace),(Clubs, Num 4)],10) = 15
val test10c = score ([(Hearts, Ace),(Diamonds, Num 4)],10) = 7


val test11 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6