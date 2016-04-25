let my_subset_test0 =
subset [1; 2; 3] [1; 1; 1; 2; 2; 2; 3; 3; 3; 4; 4; 5; 5; 5];;

let my_equal_sets_test0 =
equal_sets [1; 2; 3] [1; 2; 3];;

let my_set_union_test0 =
(set_union [1; 2; 3] [3; 4; 5]) = [1; 2; 3; 4; 5];;

let my_set_intersection_test0 =
(set_intersection [1; 2; 3] [3; 4; 5]) = [3];;

let my_set_diff_test0 =
(set_diff [1; 2; 3] [1; 2]) = [3];;

let my_computed_fixed_point_test0 =
computed_fixed_point (=) (fun x -> x/x) 100 = 1;;
  
let my_computed_periodic_point_test0 =
computed_periodic_point (=) ( fun x -> x /2 ) 2 100 = 0;;

type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal;;

let my_filter_blind_alleys_test0 = 
let rules = ["Squirtle", [N "Bulbasaur"; T "Water Gun"; N "Pikachu"];
"Bulbasaur", [T "Razor Leaf"];
"Pikachu", [T "Electrick Shock"; T "Quick Attack"]] in
let grammar = "Squirtle", rules in 
filter_blind_alleys grammar = grammar
;;
