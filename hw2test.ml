type awksub_nonterminals =
  | Expr | Term | Lvalue | Incrop | Binop | Num

let accept_empty_suffix derivation = function
   | [] -> Some (derivation, [])
   | _ -> None;;



let awkish_grammar =
  (Expr,
   function
     | Expr ->
         [[N Term; N Binop; N Expr];
          [N Term]]
     | Term ->
	 [[N Num];
	  [N Lvalue];
	  [N Incrop; N Lvalue];
	  [N Lvalue; N Incrop];
	  [T"("; N Expr; T")"]]
     | Lvalue ->
	 [[T"$"; N Expr]]
     | Incrop ->
	 [[T"++"];
	  [T"--"]]
     | Binop ->
	 [[T"+"];
	  [T"-"]]
     | Num ->
	 [[T"0"]; [T"1"]; [T"2"]; [T"3"]; [T"4"];
	  [T"5"]; [T"6"]; [T"7"]; [T"8"]; [T"9"]])



let test_1 =
(parse_prefix awkish_grammar accept_empty_suffix ["("; "8"; "+"; "7"; ")"; "-"; "5"]) =
Some([Expr, [N Term; N Binop; N Expr]; N Term, [T"("; N Expr; T")"]; Expr, [N Term; N Binop; N Expr];
Term, [N Num]; Num, [T"8"]; Binop, [T"+"]; Expr, [N Term]; Term, [N Num]; Num, [T"7"]; Binop; [T "-"]; Expr, [N Term]; Term, [N Num]; Num, [T "5"]], [])



 let test_2 =
 (parse_prefix awkish_grammar accept_empty_suffix ["("; "$"; "9"; "--"; "+"; "9"; ")"]) =
 Some(Expr, [N Term]; Term, [T"("; N Expr; T ")"]; Expr, [N Term; N Binop; N Expr]; Term, [N Lvalue; N Incrop]; Lvalue, [T"$"; N Expr];
Expr, [N Term]; Term, [N Num]; Num, [T"9"]; Incrop, [T"--"]; Binop, [T"+"]; Expr, [N Term]; Term, [N Num]; Num, [T "9"], [])