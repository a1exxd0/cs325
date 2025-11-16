# FIRST(<nonterminal>) sets
```
FIRST(program) = {"extern", "int", "float", "bool", "void"}
FIRST(extern_list) = {"extern"}
FIRST(extern) = {"extern"}
FIRST(decl_list) = {"int", "float", "bool", "void"}
FIRST(decl) = {
    "int", "float", "bool",
    "void",
}
FIRST(var_decl) = {
    "int", "float",
    "bool"
}
FIRST(type_spec) = {
    "void",
    "bool",
    "num_type"
}
FIRST(num_type) = {"int", "float"}
FIRST(dimensions) = {"["}
FIRST(param_dimensions) = {"["}
FIRST(fun_decl) = {"void", "bool", "int", "float"}
FIRST(params) = {
    "int", "float", "bool",
    "void"
} u {ε}
FIRST(param_list) = {"int", "float", "bool"}
FIRST(param) = {
    "int", "float",
    "bool"
}
FIRST(block) = {"{"}
FIRST(local_decls) = {"int", "float", "bool"}
FIRST(local_decl) = {
    "int", "float",
    "bool"
}
FIRST(stmt_list) = {IDENT, "-", "!", "(", INT_LIT, FLOAT_LIT, BOOL_LIT, ";", "{", "if", "while", "return"}
FIRST(stmt) = {
    IDENT, "-", "!", "(", INT_LIT, FLOAT_LIT, BOOL_LIT, ";",
    "{",
    "if",
    "while",
    "return"
}
FIRST(expr_stmt) = {IDENT, "-", "!", "(", INT_LIT, FLOAT_LIT, BOOL_LIT, ";"}
FIRST(while_stmt) = {"while"}
FIRST(if_stmt) = {"if"}
FIRST(else_stmt) = {"else"}
FIRST(return_stmt) = {"return"}
FIRST(lvalue) = {IDENT}
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
    "-",
    "!",
    "(", IDENT, INT_LIT, FLOAT_LIT, BOOL_LIT
}
FIRST(primary) = {
    "(",
    IDENT,
    INT_LIT,
    FLOAT_LIT,
    BOOL_LIT,
}
FIRST(args) = FIRST(arg_list) u {ε}
FIRST(arg_list) = FIRST(expr)
```

# FOLLOW(<nonterminal>) Sets
```
FOLLOW(program) = {$}
FOLLOW(extern_list) = {
    "int", "float", "bool", "void"
}
FOLLOW(extern) = {
    "extern",
    "int", "float", "bool", "void",
}
FOLLOW(decl_list) = {
    {$},
}
FOLLOW(decl) = {
    "int", "float", "bool", "void",
    {$},
}
FOLLOW(var_decl) = {
    "int", "float", "bool", "void", {$},
}
FOLLOW(type_spec) = {
    IDENT,
}
FOLLOW(num_type) = {
    IDENT,
}
FOLLOW(dimensions) = {
    ";",
    "=", "*", "/", "%", "+", "-", "<=", "<", ">=", ">", "==", "!=", "&&", "||", "]", ";", ")", ","
}
FOLLOW(param_dimensions) = {
    ",", ")",
}
FOLLOW(fun_decl) = {
    "int", "float", "bool", "void", {$},
}
FOLLOW(params) = {
    ")",
    ")",
}
FOLLOW(param_list) = {
    ")",
}
FOLLOW(param) = {
    ",",
    ")",
}
FOLLOW(block) = {
    "int", "float", "bool", "void", {$},
    IDENT, "-", "!", "(", INT_LIT, FLOAT_LIT, BOOL_LIT, ";", "{", "if", "while", "return", "}"
    "else",
}
FOLLOW(local_decls) = {
    IDENT, "-", "!", "(", INT_LIT, FLOAT_LIT, BOOL_LIT, ";", "{", "if", "while", "return"
    "}"
}
FOLLOW(local_decl) = {
    "int", "float", "bool",
    IDENT, "-", "!", "(", INT_LIT, FLOAT_LIT, BOOL_LIT, ";", "{", "if", "while", "return", "}"
}
FOLLOW(stmt_list) = {
    "}",
}
FOLLOW(stmt) = {
    IDENT, "-", "!", "(", INT_LIT, FLOAT_LIT, BOOL_LIT, ";", "{", "if", "while", "return",
    "}"
}
FOLLOW(expr_stmt) = {
    IDENT, "-", "!", "(", INT_LIT, FLOAT_LIT, BOOL_LIT, ";", "{", "if", "while", "return", "}"
}
FOLLOW(while_stmt) = {
    IDENT, "-", "!", "(", INT_LIT, FLOAT_LIT, BOOL_LIT, ";", "{", "if", "while", "return", "}"
}
FOLLOW(if_stmt) = {
    IDENT, "-", "!", "(", INT_LIT, FLOAT_LIT, BOOL_LIT, ";", "{", "if", "while", "return", "}"
}
FOLLOW(else_stmt) = {
    IDENT, "-", "!", "(", INT_LIT, FLOAT_LIT, BOOL_LIT, ";", "{", "if", "while", "return", "}"
}
FOLLOW(return_stmt) = {
    IDENT, "-", "!", "(", INT_LIT, FLOAT_LIT, BOOL_LIT, ";", "{", "if", "while", "return", "}"
}
FOLLOW(lvalue) = {
    "=",
    "*", "/", "%", "+", "-", "<=", "<", ">=", ">", "==", "!=", "&&", "||", "]", ";", ")", ","
}
FOLLOW(expr) = {
    "]",
    ";",
    ")",
    ",",
}
FOLLOW(or_expr) = {
    "]", ";", ")", ","
} 
FOLLOW(and_expr) = {
    "||",
    "]", ";", ")", ","
}
FOLLOW(eq_expr) = {
    "&&",
    "||", "]", ";", ")", ","
}
FOLLOW(rel_expr) = {
    "==",
    "!=",
    "&&", "||", "]", ";", ")", ","
}
FOLLOW(add_expr) = {
    "<=",
    "<",
    ">=",
    ">",
    "==", "!=", "&&", "||", "]", ";", ")", ","
}
FOLLOW(mul_expr) = {
    "+", 
    "-",
    "<=", "<", ">=", ">", "==", "!=", "&&", "||", "]", ";", ")", ","
}
FOLLOW(unary_expr) = {
    "*",
    "/",
    "%",
    "+", "-", "<=", "<", ">=", ">", "==", "!=", "&&", "||", "]", ";", ")", ","
}
FOLLOW(primary) = {
    "*", "/", "%", "+", "-", "<=", "<", ">=", ">", "==", "!=", "&&", "||", "]", ";", ")", ","
}
FOLLOW(args) = {
    ")",
}
FOLLOW(arg_list) = {
    ")",
}
```
