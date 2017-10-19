%{
  open ParseBase
%}

%token <string> OP
%token EXISTS LET

%token EOF OPAREN CPAREN

%token SAT UNSAT MODEL ERROR DEFINE
%token <string> DQUOTED

%token <string> IDENT CINT CBOOL
%token INT BOOL

%start top
%type <ParseBase.parse> top

%%

top:
| SAT { ParseBase.Sat }
| UNSAT { ParseBase.Unsat }
| error = err { ParseBase.Error error }
| model = model { ParseBase.Model model }
| EOF { ParseBase.None }

err:
| OPAREN ; ERROR ; msg = DQUOTED ; CPAREN { msg }

model:
| OPAREN ; MODEL ; defs = list(pred_def) ; CPAREN { defs }

pred_def:
| OPAREN ;
  DEFINE ; id = ident ; OPAREN ; args = list(arg) ; CPAREN ;
  BOOL ; body = body ;
CPAREN { (id, (args, body)) }

arg:
| OPAREN ; id = ident ; t = typ ; CPAREN { (id, t) }

binding:
| OPAREN ; id = ident ; expr = body ; CPAREN { (id, expr) }

typ:
| INT { "Int" }
| BOOL { "Bool" }

body:
| OPAREN ;
    EXISTS ; OPAREN ; args = list(arg) ; CPAREN ; body = body ;
  CPAREN { ParseBase.Qtf (args, body) }
| OPAREN ; LET ; OPAREN ;
  bindings = list(binding) ; CPAREN ; body = body ;
CPAREN {
  ParseBase.Let (bindings, body)
}
| OPAREN ; op = OP ; args = list(body) ; CPAREN {
  ParseBase.App(op, args)
}
| OPAREN ; id = IDENT ; args = list(body) ; CPAREN {
  ParseBase.PApp(id, args)
}
| CBOOL { ParseBase.Leaf $1 }
| CINT { ParseBase.Leaf $1 }
| ident { ParseBase.Leaf $1 }

ident:
| IDENT { $1 }
