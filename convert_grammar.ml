type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal;;

let is_alternative value term_list rule =
	let lhs, rhs = rule in
	if value == lhs then rhs::term_list else term_list;;

let rule_function rule_list value =
	let value_is_alternative = is_alternative value in
	List.fold_left value_is_alternative [] rule_list;;


let convert_grammar gram1 =
	let start, rule_list = gram1 in
	let production_function = rule_function rule_list in
	start, production_function;;