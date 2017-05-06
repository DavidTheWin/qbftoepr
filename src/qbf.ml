(* QBF types used for reading input from QDIMACS and validating it *)

open Printf
exception InvalidQDIMACS of string

type atom = int 
type sign = Pos | Neg
type literal = {sign: sign; atom: atom}
type clause = literal list

type quant_level = int
type quantifier = E | A
type quantified_variable =
  {quant_level: quant_level;
   quantifier: quantifier;
   variable: atom}

type prefix = quantified_variable list
type matrix = clause list
type qbf = {qbf_prefix: prefix; qbf_matrix: matrix}

type dep_scheme = Trivial | Standard

let compare_atoms atom_one atom_two = atom_one - atom_two

let atoms_of_quantified_variables quantified_variables =
  List.map (fun var -> var.variable) quantified_variables

(* Build a list of the quantified_variables seen when reading the QDIMACS *)
let rec build_quantified_variables atom_list quantifier quant_level =
    match atom_list with
      [] -> []
    | [atom] -> [{quant_level = quant_level; quantifier = quantifier; variable = atom}]
    | atom::tail ->
       {quant_level = quant_level; quantifier = quantifier; variable = atom}
       :: build_quantified_variables tail quantifier quant_level

(* String representations *)
    
let quantifier_to_string quantifier =
  match quantifier with
    E -> "E"
  | A -> "A"

let quantified_variable_to_string quantified_variable =
  sprintf "%s X%d" 
    (quantifier_to_string quantified_variable.quantifier)
    quantified_variable.variable

let prefix_to_string prefix =
  String.concat " " (List.map quantified_variable_to_string prefix)

let literal_to_string literal =
  (match literal.sign with Neg -> "~" | Pos -> "")
    ^ sprintf "X%d" literal.atom

let rec clause_to_string clause =
  match clause with
    [] -> ""
  | [literal] -> literal_to_string literal
  | literal::tail -> sprintf "%s ∨ %s" (literal_to_string literal) (clause_to_string tail)

let matrix_to_string matrix =
  String.concat "\n" (List.map clause_to_string matrix)

let qbf_to_string qbf =
  sprintf "%s\n%s" (prefix_to_string qbf.qbf_prefix) (matrix_to_string qbf.qbf_matrix)

let print_qbf qbf =
  printf "%s" (qbf_to_string qbf)

(* General dependency scheme computation *)

let all_existential_variables prefix =
  List.filter (fun var -> var.quantifier = E) prefix

(* Trivial dependency scheme computation *)

let quantified_variable_for_atom atom prefix =
  List.find (fun var -> var.variable = atom) prefix

let universally_quantified_before quant_level next =
  next.quantifier = A && next.quant_level < quant_level

let universally_quantified_below_quant_level prefix quant_level =
  List.filter (universally_quantified_before quant_level) prefix

let universally_quantified_before_variable prefix var_one atom_two =
  let quant_var_two = quantified_variable_for_atom atom_two prefix in
  universally_quantified_before var_one.quant_level quant_var_two

(* Standard dependency scheme computation *)
let clause_contains_atom atom clause =
  List.exists (fun literal -> literal.atom = atom) clause

let clauses_containing_atom atom matrix =
  List.filter (clause_contains_atom atom) matrix

let atoms_of_clause clause =
  List.map (fun literal -> literal.atom) clause

let atoms_of_clauses clauses =
  List.flatten (List.map atoms_of_clause clauses)

let std_dep_scheme_for_variable qbf var =
  let all_potential_atoms =
   List.sort_uniq
     compare_atoms
     (atoms_of_clauses (clauses_containing_atom var.variable qbf.qbf_matrix)) in
  [var.variable,
   List.filter
     (universally_quantified_before_variable qbf.qbf_prefix var)
     all_potential_atoms]

let trivial_dep_scheme_for_variable prefix var =
  [var.variable,
   atoms_of_quantified_variables
    (universally_quantified_below_quant_level prefix var.quant_level)]

let build_dep_scheme qbf dep_scheme =
  let all_existential = all_existential_variables qbf.qbf_prefix in
  match dep_scheme with
    Trivial -> List.flatten (List.map (trivial_dep_scheme_for_variable qbf.qbf_prefix) all_existential)
  | Standard -> List.flatten (List.map (std_dep_scheme_for_variable qbf) all_existential)


(* QDIMACS validation *)

let check_atom a = 
  if a < 0 
  then raise 
  (InvalidQDIMACS "Atom list in quantifier set cannot contain negative integers")

let check_if_atoms atom_list = List.iter check_atom atom_list

let innermost_quantifier_is_existential prefix =
  if prefix = []
  then false
  else
    let var = List.nth prefix ((List.length prefix) - 1) in var.quantifier = E

(* Check that all the quantified variables were used in the prefix *)
let atom_in_clause atom clause =
  List.exists (fun literal -> atom = literal.atom) clause

let atom_in_matrix matrix quantified_variable =
  List.exists (atom_in_clause quantified_variable.variable) matrix

let all_atoms_in_prefix_are_in_matrix prefix matrix =
  List.for_all (atom_in_matrix matrix) prefix

(* Return all the atoms quantified in the prefix *)
let all_atoms_in_prefix prefix =
  List.map (fun var -> var.variable) prefix

(* Given a prefix return false if any atom is quantified more than once *)
let no_duplicate_atoms_in_prefix prefix =
  let all_atoms = all_atoms_in_prefix prefix in
  (List.sort compare_atoms all_atoms) = (List.sort_uniq compare_atoms all_atoms)

(* Given a QBF raise an exception if any validation check fails *)
let validate_qbf qbf = 
  if not (innermost_quantifier_is_existential qbf.qbf_prefix)
  then raise (InvalidQDIMACS "Innermost quantifier must be existential");
  if not (all_atoms_in_prefix_are_in_matrix qbf.qbf_prefix qbf.qbf_matrix)
  then raise (InvalidQDIMACS "All atoms in the prefix must appear in the matrix");
  if not (no_duplicate_atoms_in_prefix qbf.qbf_prefix)
  then raise (InvalidQDIMACS "Atoms must only be quantified once")
