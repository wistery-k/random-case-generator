%{
  open Syntax
  open Int64_op
%}

%token <string> IDENT
%token <Int64.t> INT
%token <string> CHAR
%token TYPE
%token LBRACK RBRACK LPAREN RPAREN
%token COMMA COLON
%token EQ EOL

%nonassoc IDENT INT CHAR TYPE LBRACK RBRACK LPAREN RPAREN COMMA COLON EQ EOL

%start topp
%type <Syntax.top> topp
%%

topp:
  | TYPE IDENT EQ typp EOL { Typedef ($2, $4) }
  | IDENT COLON IDENT LBRACK idlst EOL { Term ($1, (Type.Id $3, $5)) }
  | IDENT COLON IDENT EOL { Term ($1, (Type.Id $3, [])) }

typp:
  | CHAR { Type.Char $1 }
  | LBRACK INT COMMA INT RBRACK { Type.Int ($2, $4) }
  | LBRACK INT COMMA INT RPAREN { Type.Int ($2, $4 - 1L) }
  | LPAREN INT COMMA INT RBRACK { Type.Int ($2 + 1L, $4) }
  | LPAREN INT COMMA INT RPAREN { Type.Int ($2 + 1L, $4 - 1L) }

idlst:
  | IDENT RBRACK LBRACK idlst { $1 :: $4 }
  | IDENT RBRACK              { $1 :: [] }





















