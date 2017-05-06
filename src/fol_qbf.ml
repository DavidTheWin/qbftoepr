(* Types for QBF's in first order logic *)
open Lib
open Qbf
open Printf

type atom = int
type func = {func_name: string; func_arguments: atom list}

type pred_arg = Atom of atom | Func of func | True | False
type pred_args = pred_arg list
type predicate = {pred_name: string; pred_arguments: pred_args}
type sign = Pos | Neg

type literal = {sign: sign; pred: predicate}
type clause = literal list
type matrix = clause list

type quant_level = int
type quantifier = E | A
type quantified_variable = 
  {quant_level: quant_level; quantifier: quantifier; atom: atom}
type prefix = quantified_variable list

type fol_qbf = {prefix: prefix; matrix: matrix}

let fol_pred_name = "p"

(* Constructors *)

let make_function func_name func_args =
  {func_name = func_name; func_arguments = func_args}

let make_predicate pred_name pred_args =
  {pred_name = pred_name; pred_arguments = pred_args}

let make_pos_literal predicate =
  {sign = Pos; pred = predicate}

let make_neg_literal predicate =
  {sign = Neg; pred = predicate}

let build_fol_qbf prefix matrix =
  {prefix = prefix; matrix = matrix}

let clauses_for_introduced_predicate =
  [[make_pos_literal (make_predicate fol_pred_name [True])];
   [make_neg_literal (make_predicate fol_pred_name [False])]]
  
(* Conversion from QBF to FOL *)
(* Recursively copies the values from a Qbf.qbf to a Fol_qbf.fol_qbf then returns it *)

let convert_qbf_quant_var_to_fol (quantified_variable : Qbf.quantified_variable) =
  {quant_level = quantified_variable.quant_level;
   quantifier = (match quantified_variable.quantifier with E -> E | A -> A);
   atom = quantified_variable.variable}
  
let convert_qbf_prefix_to_fol qbf_prefix =
  List.map convert_qbf_quant_var_to_fol qbf_prefix

let convert_literal_to_fol (literal : Qbf.literal) =
  match literal.sign with
    Pos -> make_pos_literal (make_predicate fol_pred_name [Atom(literal.atom)])
  | Neg -> make_neg_literal (make_predicate fol_pred_name [Atom(literal.atom)])

let convert_clause_to_fol (clause : Qbf.clause) =
  List.map convert_literal_to_fol clause

let convert_matrix_to_fol qbf_matrix =
  List.map convert_clause_to_fol qbf_matrix

let convert_qbf_to_fol (qbf : Qbf.qbf)  =
  {prefix = convert_qbf_prefix_to_fol qbf.qbf_prefix;
   matrix = convert_matrix_to_fol qbf.qbf_matrix @ clauses_for_introduced_predicate}

(* String representations and printing *)

(* Functions to turn various types into a string representation *)
let quantifier_to_string quantifier =
  match quantifier with E -> "E" | A -> "A"

let quantified_variable_to_string quantified_variable =
  sprintf "%s X%d" 
    (quantifier_to_string quantified_variable.quantifier)
    quantified_variable.atom

let func_to_string func =
  if func.func_arguments = [] then func.func_name
  else sprintf "%s(%s)" 
    func.func_name 
    (String.concat "," (List.map (fun atom -> "X" ^ (string_of_int atom)) func.func_arguments))

let pred_arg_to_string pred_arg =
  match pred_arg with
    Atom(n) -> "X" ^ (string_of_int n)
  | Func(f) -> func_to_string f
  | True -> "true"
  | False -> "false"

let predicate_to_string predicate =
  if predicate.pred_arguments = [] 
  then predicate.pred_name
  else 
    sprintf 
    "%s(%s)" 
    predicate.pred_name 
    (String.concat "," (List.map pred_arg_to_string predicate.pred_arguments))

let literal_to_string literal =
  (match literal.sign with Pos -> "" | Neg -> "~")
    ^ (predicate_to_string literal.pred)

let clause_to_string clause =
  String.concat " ∨ " (List.map literal_to_string clause)

(* Functions to print various types to the terminal *)
let prefix_to_string prefix =
  String.concat " " (List.map quantified_variable_to_string prefix)

let matrix_to_string matrix =
  String.concat "\n" (List.map clause_to_string matrix)

let fol_qbf_to_string fol_qbf =
  sprintf "%s\n%s" (prefix_to_string fol_qbf.prefix) (matrix_to_string fol_qbf.matrix)

let print_fol_qbf qbf =
  printf "%s" (fol_qbf_to_string qbf)

(* Printing data structures in TPTP *)
let clause_to_string id clause =
  sprintf "cnf(cl_%d,plain,(%s))."
    id
    (String.concat " | " (List.map literal_to_string clause))

let print_clause out_channel id clause =
  fprintf out_channel "%s\n" (clause_to_string id clause)

let print_tptp_fol_qbf fol_qbf out_channel =
  List.iteri (print_clause out_channel) fol_qbf.matrix

(* Operations on QBFs *)

(* Skolemization functions *)
(* Given a prefix return a new prefix without the given variable *)
let prefix_without_quantifier prefix variable =
  List.filter (fun var -> var.atom != variable.atom) prefix

(* Given an argument list build a function with those arguments
 * If there are no arguments it will be named a constant *)
let build_skolem_function variable args : func =
  let name = (if args = [] then "c_" else "f_") ^ (string_of_int variable.atom) in
  make_function name args

(* Remove all existential variables from the prefix *)
let prefix_without_existential_variables prefix =
  List.filter (fun var -> var.quantifier != E) prefix

(* Build a list of pairs of skolem functions and the variables they are replacing *)
let rec build_skolem_func_list prefix dep_scheme =
  try
    let outermost = List.find (fun var -> var.quantifier = E) prefix in
    let func_args = List.assoc outermost.atom dep_scheme in
    [outermost.atom, build_skolem_function outermost func_args]
    @ (build_skolem_func_list (prefix_without_quantifier prefix outermost) dep_scheme)
  with Not_found -> []

(* Given an atom, return the function that should replace it, None if there is
 * no such function *)
let replace_atom_with_skolem_func atom skolem_list =
  try
    let new_arg = List.assoc atom skolem_list in
    [Func(new_arg)]
  with Not_found -> [Atom(atom)]

(* Return a new predicate after skolemization *)
let get_skolemized_predicate skolem_list predicate =
  let atom = List.hd predicate.pred_arguments in
  match atom with
    Atom(n) ->
      make_predicate
      predicate.pred_name
      (replace_atom_with_skolem_func n skolem_list)
  | _ -> predicate

let get_skolemized_literal skolem_list literal =
  {sign = literal.sign; pred = get_skolemized_predicate skolem_list literal.pred}

let get_skolemized_clause skolem_list clause =
  List.map (get_skolemized_literal skolem_list) clause

let get_skolemized_matrix matrix skolem_list =
  List.map (get_skolemized_clause skolem_list) matrix

(* Performs skolemization on a given fol_qbf
 * First builds all the skolem functions then removes all existential variables
 * from the prefix then replaces the relevant variables in the matrix with those
 * skolem functions *)
let skolemization fol_qbf dep_scheme =
  let skolem_list = build_skolem_func_list fol_qbf.prefix dep_scheme in
  build_fol_qbf
  (prefix_without_existential_variables fol_qbf.prefix)
  (get_skolemized_matrix fol_qbf.matrix skolem_list)

(* Functions for removing skolem functions *)
(* Given a function, return a predicate type with the function's data *)
let raise_function_to_predicate (func : func) =
  make_predicate 
    ("p_" ^ func.func_name) 
    (List.map (fun arg -> Atom(arg)) func.func_arguments)

(* Given a predicate, return a new predicate with the function removed
 * Assumes the predicate has a single argument, if it is a function then the
 * function is raised to become the predicate otherwise it does nothing
 * At this stage in the execution predicates have only one argument, either an
 * atom or a skolem function
 *)
let remove_function_from_predicate predicate =
  match List.hd predicate.pred_arguments with
    Func(f) -> raise_function_to_predicate f
  | _ -> predicate

let remove_function_from_literal literal =
  {sign = literal.sign; pred = remove_function_from_predicate literal.pred}

let remove_functions_from_clause clause =
  List.map remove_function_from_literal clause

let remove_functions_from_matrix matrix =
  List.map remove_functions_from_clause matrix

let remove_functions_from_fol_qbf fol_qbf =
  let new_matrix = remove_functions_from_matrix fol_qbf.matrix in
  build_fol_qbf fol_qbf.prefix new_matrix
