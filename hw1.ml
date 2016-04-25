type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal;;

let subset a b =
List.fold_left (fun boolean element ->
if boolean
then List.mem element b
else false)
true a;;

let equal_sets a b =
subset a b && subset b a;;


let set_union a b =
a@b;;

let set_intersection a b =
List.fold_left (fun inter element ->
let boolean = List.mem element inter in
if not boolean && List.mem element b
then element::inter
else inter)
[] a;;

let set_diff a b =
List.fold_left (fun diff element ->
let inside_diff = List.mem element diff in
let inside_b = List.mem element b in
if not inside_diff && not inside_b
then element::diff
else diff)
[] a;;

let rec computed_fixed_point eq f x =
let y = f x in
if eq y x
then x else computed_fixed_point eq f y;;

let rec counter f p x =
if p > 0
then let y = f x in
let q = p-1 in
counter f q y
else x;;

let rec computed_periodic_point eq f p x =
let y = counter f p x in
if eq y x
then x
else computed_periodic_point eq f p (f x);;

let same_size a b =
List.length a = List.length b;;

let is_terminal nonterminal_list symbol_list =
let non_term_symbols = List.fold_left ( fun non_terms sym ->
match sym with
| T x -> non_terms
| N x -> x::non_terms) [] symbol_list in
subset non_term_symbols nonterminal_list;;


let same_size a b =
List.length a = List.length b;;


let grab_terminated rules terminal_symbols =
let start_list, sym_list_list = List.split rules in
List.fold_left2 ( fun terminal_list start sym_list ->
if ( is_terminal terminal_list sym_list ) && not (List.mem start terminal_list)
then start::terminal_list
else terminal_list) terminal_symbols start_list sym_list_list


let filter_blind_alleys g =
let start_grammar, rules = g in
let grab_terminated_symbols = grab_terminated rules in
let terminal_symbols = computed_fixed_point same_size grab_terminated_symbols [] in
start_grammar, List.fold_right ( fun rule valid_rules ->
let start, _ = rule in
if List.mem start terminal_symbols
then rule::valid_rules
else valid_rules 
) rules [];;
