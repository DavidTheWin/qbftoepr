open Lib
open Printf
open Fol_qbf
open Qbf
open Io_handling

exception InvalidOptions of string

type options =
  | Debug
  | Help
  | Input
  | Output
  | DepScheme
  | String of string

let string_to_option = function
  | "--debug" -> Debug
  | "--help" -> Help
  | "-i" -> Input
  | "-o" -> Output
  | "-d" -> DepScheme
  | s -> String(s)

let input_filename = ref None
let output_filename = ref None
let dep_scheme = ref Trivial

(* debug modifiable part *)

let dbg_flag = ref false

type dbg_gr =
  | D_trace
  | D_elapsed

let dbg_gr_to_str = function
  | D_trace -> "trace"
  | D_elapsed -> "elapsed"

let dbg_groups =
  [
    (* D_trace; *)
    D_elapsed;
  ]

let module_name = "qbftoepr"

(* debug fixed part *)

let () = Lib.dbg_flag_msg !dbg_flag module_name

let dbg group str_lazy =
  Lib.dbg_out_pref !dbg_flag dbg_groups group dbg_gr_to_str module_name str_lazy

let dbg_env group f =
  Lib.dbg_env_set !dbg_flag dbg_groups group f

(* elapsed time code *)

(* keep the time *)
let last_timestamp = ref 0.0

(* set the timestamp *)
let timestamp () = last_timestamp := Unix.gettimeofday ()

(* helper: print the elapsed time, keep the time stamp *)
let elapsed_helper status =
  (* current time *)
  let current = Unix.gettimeofday () in
  (* report *)
  out_str
    (Format.sprintf
     "Timer report: %s: elapsed time %.7fs"
     status
     (current -. !last_timestamp));
  (* return current time *)
  current

(* print the elapsed time, keep the time stamp *)
let elapsed status = ignore (elapsed_helper status)

(* print the elapsed time, reset timer *)
let elapsed_ts status = last_timestamp := (elapsed_helper status)

let convert_qbf_to_epr input_filename output_filename dep_scheme =
  try
    dbg_env D_elapsed (fun () -> timestamp ());
    let qbf = parse_input_file input_filename in
    dbg_env D_elapsed (fun () -> (elapsed_ts "Parsing input"));
    dbg D_trace (lazy(qbf_to_string qbf));
    let fol_qbf = convert_qbf_to_fol qbf in
    dbg_env D_elapsed (fun () -> (elapsed_ts "Converting to first order logic"));
    dbg D_trace (lazy(fol_qbf_to_string fol_qbf));
    let dep_scheme_list = build_dep_scheme qbf dep_scheme in
    dbg_env D_elapsed (fun () -> (elapsed_ts "Dependency scheme computed"));
    let skolemized_qbf = skolemization fol_qbf dep_scheme_list in
    dbg_env D_elapsed (fun () -> (elapsed_ts "Skolemization"));
    dbg D_trace (lazy(fol_qbf_to_string skolemized_qbf));
    let qbf_with_no_functions = remove_functions_from_fol_qbf skolemized_qbf in
    dbg_env D_elapsed (fun () -> (elapsed_ts "Removing functions"));
    dbg D_trace (lazy(fol_qbf_to_string qbf_with_no_functions));
    print_tptp qbf_with_no_functions output_filename;
    dbg_env D_elapsed (fun () -> (elapsed_ts "Printing TPTP"))
  with ParsingError e -> printf "Fatal error: %s\n" e

let rec find_index item lst =
  match lst with
    [] -> raise Not_found
  | hd::tl -> if item = hd then 0 else 1 + find_index item tl

let help_str = "Run using arguments
-i filename: required
-o filename: optional, if not supplied output to stdout
-d dependency_scheme: optional, if not supplied default to trivial, valid options are 'standard'
--help: prints this text then exits
--debug: prints debug messages to stdout
example: ./qbftoepr -i qdimacs/problem.qdimacs -o tptp/problem.p -d standard --debug"

let check_for_help_option arg_list =
  if List.exists (fun arg -> arg = Help) arg_list
  then (printf "%s\n" help_str; exit 0)

let check_for_debug_option arg_list =
  if List.exists (fun arg -> arg = Debug) arg_list
  then dbg_flag := true

let check_for_input arg_list =
  if List.exists (fun arg -> arg = Input) arg_list
  then
    let input_index = find_index Input arg_list in
    let input_filename_arg = List.nth arg_list (input_index + 1) in
    match input_filename_arg with
      String(s) -> input_filename := Some(s)
    | _ -> raise (InvalidOptions "No input file specified after -i flag")
  else
    raise (InvalidOptions "An input file must be specified")

let check_for_output arg_list =
  if List.exists (fun arg -> arg = Output) arg_list
  then
    let output_index = find_index Output arg_list in
    let output_filename_arg = List.nth arg_list (output_index + 1) in
    match output_filename_arg with
      String(s) -> output_filename := Some(s)
    | _ -> raise (InvalidOptions "No output file specified after -o flag")
  else output_filename := None

let check_for_dep_scheme arg_list =
  if List.exists (fun arg -> arg = DepScheme) arg_list
  then
    let dep_scheme_index = find_index DepScheme arg_list in
    let dep_scheme_arg = List.nth arg_list (dep_scheme_index + 1) in
    dep_scheme := match dep_scheme_arg with
      String(s) ->
        (match s with
           "standard" -> Standard
         | _ -> Trivial)
     | _ -> raise (InvalidOptions "No dependency scheme specified after -d flag")

let parse_options string_args =
  try
    let args = Array.map string_to_option string_args in
    let arg_list = Array.to_list args in
    check_for_help_option arg_list;
    check_for_debug_option arg_list;
    check_for_input arg_list;
    check_for_output arg_list;
    check_for_dep_scheme arg_list
  with
    InvalidOptions e -> printf "Fatal error: %s\n" e; exit 0

let _ =
  parse_options Sys.argv;
  convert_qbf_to_epr !input_filename !output_filename !dep_scheme
