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

let rec check_symbol start_symbol production_function derivation accept fragment rhs =
  match rhs with
  | [] -> Some(derivation, fragment)
  | first::last -> 
  (match first with 
  | T sym -> (match fragment with | [] -> None | frag::ment -> 
    (if frag = sym
    then (check_symbol start_symbol production_function derivation accept ment last)
    else None))
  (* if first symbol is *)
  | N sym ->(match check_list sym production_function derivation accept fragment (production_function sym) with
            | None -> None
            | Some(var) -> (let deriv, suffix = var in check_symbol start_symbol production_function deriv accept suffix last)))


      and check_list start_symbol production_function derivation accept fragment alternative_list =
			match alternative_list with
				| [] -> None
				(* If list not empty check head's symbol *)
				| rhs::tail -> (match check_symbol start_symbol production_function (derivation@[(start_symbol, rhs)]) accept fragment rhs with 
								| None -> check_list start_symbol production_function derivation accept fragment tail
								| Some(a) -> let deriv, suffix = a in (match accept deriv suffix with
                  | Some(b) -> Some(b)
                  | None -> check_list start_symbol production_function derivation accept fragment tail))

        and matcher start_symbol production_function derivation accept fragment =
        (* check entire alternative list of start symbol *)
        check_list start_symbol production_function derivation accept fragment (production_function start_symbol);;



let parse_prefix grammar =
	let start_symbol, production_function = grammar in
	matcher start_symbol production_function []
;;