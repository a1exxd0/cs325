# FIRST(<nonterminal>) sets
```
FIRST(program) = {
    "extern",
    "int", "float", "bool", "void"
}
FIRST(extern_list) = {"extern"}
FIRST(extern) = {"extern"}
FIRST(decl_list) = {"int", "float", "bool", "void"}
FIRST(decl) = {
    "int", "float", "bool",
    "void"
}
FIRST(var_decl) = {"int", "float", "bool"}
FIRST(type_spec) = {
    "void",
    "int", "float", "bool"
}
FIRST(var_type) = {
    "int",
    "float",
    "bool"
}
FIRST(fun_decl) = {"void", "int", "float", "bool"}
FIRST(params) = {
    "int", "float", "bool",
    "void"
} u {ε}
FIRST(param_list) = {"int", "float", "bool"}
FIRST(param) = {"int", "float", "bool"}
FIRST(block) = {"{"}
FIRST(local_decls) = {"int", "float", "bool"} u {ε}
FIRST(local_decl) = {"int", "float", "bool"}
FIRST(stmt_list) = {";", IDENT, "-", "!", "(", INT_LIT, FLOAT_LIT, BOOL_LIT, "{", "if", "while", "return"} u {ε}
FIRST(stmt) = {
    ";", IDENT, "-", "!", "(", INT_LIT, FLOAT_LIT, BOOL_LIT,
    "{",
    "if",
    "while",
    "return"
}
FIRST(expr_stmt) = {
    ";", IDENT, "-", "!", "(", INT_LIT, FLOAT_LIT, BOOL_LIT
}
FIRST(while_stmt) = {"while"}
FIRST(if_stmt) = {"if"}
FIRST(else_stmt) = {else_stmt} u {ε}
FIRST(return_stmt) = {"return"}
FIRST(expr) = {
    IDENT,
    "-", "!", "(", INT_LIT, FLOAT_LIT, BOOL_LIT
}
FIRST(or_expr) = {"-", "!", "(", IDENT, INT_LIT, FLOAT_LIT, BOOL_LIT}
FIRST(and_expr) = {"-", "!", "(", IDENT, INT_LIT, FLOAT_LIT, BOOL_LIT}
FIRST(eq_expr) = {"-", "!", "(", IDENT, INT_LIT, FLOAT_LIT, BOOL_LIT}
FIRST(rel_expr) = {"-", "!", "(", IDENT, INT_LIT, FLOAT_LIT, BOOL_LIT}
FIRST(add_expr) = {"-", "!", "(", IDENT, INT_LIT, FLOAT_LIT, BOOL_LIT}
FIRST(mul_expr) = {"-", "!", "(", IDENT, INT_LIT, FLOAT_LIT, BOOL_LIT}
FIRST(unary_expr) = {
    "-", "!", "(", 
    IDENT, INT_LIT, FLOAT_LIT, BOOL_LIT
}
FIRST(primary) = {"(", IDENT, INT_LIT, FLOAT_LIT, BOOL_LIT}
FIRST(args) = {IDENT, "-", "!", "(", INT_LIT, FLOAT_LIT, BOOL_LIT} u {ε}
FIRST(arg_list) = {IDENT, "-", "!", "(", INT_LIT, FLOAT_LIT, BOOL_LIT}
```
