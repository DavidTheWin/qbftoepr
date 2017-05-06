open Printf
open Qbf
open Fol_qbf

exception ParsingError of string

let get_out_channel filename =
  match filename with
    Some(s) -> open_out s
  | None -> stdout

let get_in_channel filename =
  match filename with
    Some(s) -> open_in s
  | None -> stdin

let parse_input_file filename =
  let in_channel = get_in_channel filename in
  let lexbuf = Lexing.from_channel in_channel in
  try
    let qbf = Parser_qdimacs.main Lexer_qdimacs.token lexbuf in
      if(in_channel != stdin) then close_in in_channel;
      validate_qbf qbf;
      qbf
  with 
    | Parsing.Parse_error ->
        close_in_noerr in_channel;
        raise (ParsingError "Input file does not comply with QDIMACS grammar rules")
    | InvalidQDIMACS e ->
        close_in_noerr in_channel;
        raise (ParsingError e)

let print_tptp fol_qbf filename =
  let out_channel = get_out_channel filename in
  try
    print_tptp_fol_qbf fol_qbf out_channel;
    if(out_channel != stdout) then close_out out_channel
  with e -> close_out_noerr out_channel; raise e
