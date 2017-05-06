{
  open Parser_qdimacs
}

rule token = parse
    [' ' '\t']                {token lexbuf} (* Skip blanks *)
  | ['\n']                    {EOL}
  | ('-')?['1'-'9']['0'-'9']* {Num(int_of_string (Lexing.lexeme lexbuf))}
  | 'c' [^ '\n']*             {Comment(Lexing.lexeme lexbuf)}
  | 'p' [^ '\n']*             {Problem(Lexing.lexeme lexbuf)}
  | 'e'                       {Existential(Lexing.lexeme lexbuf)}
  | 'a'                       {Universal(Lexing.lexeme lexbuf)}
  | '0' [^ '\n']*             {Zero}
  | eof                       {EOF}
