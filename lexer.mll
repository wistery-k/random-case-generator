{
  open Syntax
  open Parser
  let sll = "abcdefghijklmnopqrstuvwxyz"
  let bll = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
}

let space = [' ' '\t']
let num = ['0'-'9']
let char = ['a'-'z']

rule token = parse
  | space+  { token lexbuf }
  | "type"  { TYPE }
  | "[a-z]" { CHAR sll }
  | "[A-Z]" { CHAR bll }
  | "[a-zA-Z]" { CHAR (sll^bll) } (* super-tenuki *)
  | "[A-Za-z]" { CHAR (sll^bll) } (* super-tenuki *)
  | '\n'    { EOL }
  | '['     { LBRACK }
  | ']'     { RBRACK }
  | '('     { LPAREN }
  | ')'     { RPAREN }
  | ','     { COMMA  }
  | ':'     { COLON  }
  | '='     { EQ     }
  | num+ as lxm  { INT (Int64.of_string lxm) }
  | char+ as lxm { IDENT lxm }
  | eof     { raise End_of_file }

(** sample input

type a = [1,10)
type b = [0,100)
n:a
a:b[n]

*)




















