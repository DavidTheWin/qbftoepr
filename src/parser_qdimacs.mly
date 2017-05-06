%{
  open Qbf
  open Printf

  let quant_level_counter = ref 0
  let clauses = ref []
  let quantified_variables = ref []
  let quantifiers_seen = ref []
  let parse_error s = print_endline s

  (* If the new quantifier is the same as the last one read in raise InvalidQDIMACS
   * Compares the new against the head of the list, if it isn't the same as the
   * previous it pushes the new to the head of the list because only the last
   * quantifier seen is relevant and getting the head of the list is easy *)
  let check_quantifier_alternated quantifier =
    if (List.length !quantifiers_seen = 0) 
       || (quantifier != (List.hd !quantifiers_seen))
    then quantifiers_seen := quantifier :: !quantifiers_seen
    else raise (InvalidQDIMACS "Contiguous quantifiers must be different")
%}

%token Zero
%token <string> Comment Problem
%token <string> Existential Universal 
%token EOL
%token EOF
%token <int> Num
%start main
%type <Qbf.qbf> main
%%

main:
    /* empty */ { {qbf_matrix = []; qbf_prefix = []} }
  | main line   { {qbf_matrix = !clauses; qbf_prefix = !quantified_variables} }
;

line :
    comment_line {}
  | problem_line {}
  | quant_set    { quantified_variables := !quantified_variables @ $1 }
  | clause       { clauses := !clauses @ [$1] }
;

comment_line:
  Comment EOL {}
;

problem_line:
  Problem EOL {}
;

quant_set:
  quantifier atom_set Zero EOL 
  {
    (* Check if the atoms just read were all positive integers
     * Update the quantification level
     * Find the right quantifier to use
     * Check the quantifier has alternated
     * Build a list of quantified_variables *)
    check_if_atoms $2;
    incr quant_level_counter;
    let quant = 
      match $1 with 
          "e" -> E 
        | "a" -> A 
        | _ -> raise (InvalidQDIMACS "Quantifier must be 'e' or 'a'") in
      check_quantifier_alternated quant;
      build_quantified_variables $2 quant !quant_level_counter
  }
;

quantifier:
    Existential {$1}
  | Universal {$1}
;

atom_set:
    atom_set Num {$1 @ [$2]}
  | Num {[$1]}
;

clause:
  clause_list Zero EOL {$1}
;

// After this whether the value was negative/positive doesn't matter so remove
// it using abs
clause_list:
  clause_list Num { $1 @ [ {sign = (if $2 < 0 then Neg else Pos); atom = (abs $2)} ] }
  | Num { [ {sign = (if $1 < 0 then Neg else Pos); atom = (abs $1)} ] }
;
