# Grammar LL(k)
I clarified with the module organiser that it was fine to use EBNF as opposed to
BNF standalone.

Using Extended-BNF (EBNF), we define our formal operators as, with BNF:
```
optional ::= "[" expression "]"              (* zero or one occurrence *)
repetition ::= "{" expression "}"              (* zero or more occurrences *)
repetition ::= expression "*"                  (* zero or more occurrences *)
positive-rep ::= expression "+"                  (* one or more occurrences *)

grouping ::= "(" expression ")"              (* grouping without capture *)
```

This grammar is now LL(3), with the highest lookahead declaration stemming from `decl`.

```
program ::= [extern_list] decl_list
extern_list ::= extern+ 
extern ::= "extern" type_spec IDENT "(" params ")" ";"
decl_list ::= decl+
decl ::= var_decl
        |  fun_decl
var_decl ::= num_type IDENT [dimensions] ";" 
        | "bool" IDENT ";"
type_spec ::= "void"
        | "bool"
        | num_type
num_type ::= "int" | "float"
dimensions ::= "[" expr "]" ["[" expr "]" ["[" expr "]"]]
param_dimensions ::= "[" [expr] "]" ["[" expr "]" ["[" expr "]"]]
fun_decl ::= type_spec IDENT "(" params ")" block
params ::= [param_list] | "void"
param_list ::= param ("," param)* 
param ::= num_type IDENT [param_dimensions] 
        | "bool" IDENT
block ::= "{" [local_decls] [stmt_list] "}"
local_decls ::= local_decl+
local_decl ::= num_type IDENT [dimensions] ";" 
        | "bool" IDENT ";"
stmt_list ::= stmt+
stmt ::= expr_stmt
        |  block
        |  if_stmt
        |  while_stmt
        |  return_stmt
expr_stmt ::= [expr] ";"
while_stmt ::= "while" "(" expr ")" stmt
if_stmt ::= "if" "(" expr ")" block [else_stmt]
else_stmt  ::= "else" block
return_stmt ::= "return" ";"
        |  "return" expr ";"
lvalue ::= IDENT [dimensions]
expr ::= lvalue "=" expr
        | or_expr 
or_expr ::= and_expr ("||" and_expr)*
and_expr ::= eq_expr ("&&" eq_expr)*
eq_expr ::= rel_expr (("==" | "!=") rel_expr)*
rel_expr ::= add_expr (("<=" | "<" | ">=" | ">") add_expr)*
add_expr ::= mul_expr (("+" | "-") mul_expr)*
mul_expr ::= unary_expr (("*" | "/" | "%") unary_expr)*
unary_expr ::= "-" unary_expr
        | "!" unary_expr
        | primary
primary ::= "(" expr ")"
        | IDENT "(" args ")"
        | lvalue 
        | INT_LIT
        | FLOAT_LIT 
        | BOOL_LIT 
args ::= [arg_list]
arg_list ::= expr ("," expr)*
```
