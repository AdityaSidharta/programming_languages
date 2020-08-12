(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test1 = only_capitals ["A","B","C"] = ["A","B","C"]
val test1a = only_capitals ["A","B","hello", "C"] = ["A","B","C"]
val test1b = only_capitals [] = []

val test2 = longest_string1 ["A","bc","C"] = "bc"
val test2a = longest_string1 [] = ""
val test2b = longest_string1 ["A","bc","C", "CD"] = "bc"

val test3 = longest_string2 ["A","bc","C"] = "bc"
val test3a = longest_string2 [] = ""
val test3b = longest_string2 ["A","bc","C", "CD"] = "CD"

val test4a = longest_string3 ["A","bc","C"] = "bc"
val test4aa = longest_string3 ["A","bc","C"] = "bc"
val test4ab = longest_string3 [] = ""
val test4ac = longest_string3 ["A","bc","C", "CD"] = "bc"

val test4b = longest_string4 ["A","B","C"] = "C"
val test4ba = longest_string4 ["A","bc","C"] = "bc"
val test4bb = longest_string4 [] = ""
val test4bc = longest_string4 ["A","bc","C", "CD"] = "CD"

val test5 = longest_capitalized ["A","bc","C"] = "A"
val test5a = longest_capitalized [] = ""
val test5b = longest_capitalized ["a","bc","c"] = ""
val test5c = longest_capitalized ["AC","bca","CA"] = "AC"

val test6 = rev_string "abc" = "cba"
val test6a = rev_string "" = ""
val test6b = rev_string "c" = "c"
val test6c = rev_string "Abc" = "cbA"

val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4

val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val test8a = all_answers (fn x => if x > 1 then SOME [x] else NONE) [2,3,4,5,6,7] = SOME [2,3,4,5,6,7]
val test8b = all_answers (fn x => if x > 1 then SOME [x] else NONE) [] = SOME []

val test9a = count_wildcards Wildcard = 1
val test9ab = count_wildcards (Variable "Hello") = 0
val test9ac = count_wildcards (TupleP [Wildcard, Variable "Hello", Wildcard]) = 2

val test9b = count_wild_and_variable_lengths (Variable("a")) = 1
val test9ba = count_wild_and_variable_lengths Wildcard = 1
val test9bb = count_wild_and_variable_lengths (Variable "Hello") = 5
val test9bc = count_wild_and_variable_lengths (TupleP [Wildcard, Variable "Hello", Wildcard]) = 7

val test9c = count_some_var ("x", Variable("x")) = 1
val test9ca = count_some_var ("x", TupleP [Wildcard, Variable "Hello", Wildcard]) = 0
val test9cb = count_some_var ("x", TupleP [Variable("x"), Variable "Hello", Variable("x")]) = 2

val test10 = check_pat (Variable("x")) = true
val test10a = check_pat (TupleP [Variable("x"), Variable "Hello", Variable("x")]) = false

val test11 = match (Const(1), UnitP) = NONE

val test12 = first_match Unit [UnitP] = SOME []
