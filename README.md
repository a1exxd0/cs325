# CS325 Compiler Design

To clone the repository on GH, use:
```sh
# Recurse submodules is needed because we use fmt/ as a submodule.
git clone --recurse-submodules https://github.com/a1exxd0/cs325.git
```

A short introduction in how to produce the frontend for a C compiler producing LLVM IR.

Lexer is prewritten, we need to build a recursive descent predictive parser in C++.

# Quickstart
To build, you need `make`, and this tarball.

Then, run:
```
make mccomp
./tests/tests.sh
```

# Notes
I'm using two external third-party libraries and will reference accordingly: `{fmt}` and `TartanLlama/expected`.

Both are used extensively throughout the project. They come zipped.

# Original Grammar before changes for reference

```
program ::= extern_list decl_list
        | decl_list
extern_list ::= extern_list extern
        |  extern
extern ::= "extern" type_spec IDENT "(" params ")" ";"
decl_list ::= decl_list decl
        |  decl
decl ::= var_decl
        |  fun_decl
var_decl ::= var_type IDENT ";"
type_spec ::= "void"
        |  var_type
var_type  ::= "int" |  "float" |  "bool"
fun_decl ::= type_spec IDENT "(" params ")" block
params ::= param_list
        |  "void" | epsilon
param_list ::= param_list "," param
        |  param
param ::= var_type IDENT
block ::= "{" local_decls stmt_list "}"
local_decls ::= local_decls local_decl
        |  epsilon
local_decl ::= var_type IDENT ";"
stmt_list ::= stmt_list stmt
        |  epsilon
stmt ::= expr_stmt
        |  block
        |  if_stmt
        |  while_stmt
        |  return_stmt
expr_stmt ::= expr ";"
        |  ";"
while_stmt ::= "while" "(" expr ")" stmt
if_stmt ::= "if" "(" expr ")" block else_stmt
else_stmt  ::= "else" block
        |  epsilon
return_stmt ::= "return" ";"
        |  "return" expr ";"
# operators in order of increasing precedence
expr ::= IDENT "=" expr
        | rval
rval ::= rval "||" rval
        | rval "&&" rval
        | rval "==" rval | rval "!=" rval
        | rval "<=" rval | rval "<" rval | rval ">=" rval | rval ">" rval
        | rval "+" rval  | rval "-" rval
        | rval "*" rval  | rval "/" rval  | rval "%" rval
        | "-" rval | "!" rval
        | "(" expr ")"
        | IDENT | IDENT "(" args ")"
        | INT_LIT | FLOAT_LIT | BOOL_LIT
args ::= arg_list
        |  epsilon
arg_list ::= arg_list "," expr
        |  expr
```

