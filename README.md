# orchestra

Orchestra is a programming language designed to compile to LC3 Assembly.

# syntax

```
program : global_decl*

global_decl :
    | const_decl
    | let_decl
    | fn_decl

const_decl : "const" ident "=" int ";"

let_decl : "let" ident "=" int ";"

fn_decl : "fn" ident "(" ((ident ",")* ident)* ")" block

block : decl*
```
