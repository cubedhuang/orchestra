# orchestra

Orchestra is a programming language designed to compile to LC3 Assembly.

# syntax

```
program : global_declaration*

global_declaration :
    | global_variable
    | function

global_variable : 'let' identifier ('=' number)? ';'

function : 'fn' identifier '(' function_arguments? ')' block
function_arguments : identifier (',' identifier)*

block : '{' declaration* '}'

declaration :
    | variable
    | statement

variable : 'let' identifier ('=' expression)? ';'

statement :
    | block
    | if_statement
    | while_statement
    | return_statement
    | expression ';'

if_statement : 'if' '(' expression ')' statement ('else' statement)?
while_statement : 'while' '(' expression ')' statement
return_statement : 'return' expression? ';'

expression : assignment

assignment :
    | identifier '=' assignment
    | logical

logical :
    | logical ('and' | 'or') comparison
    | comparison

equality :
    | equality ('==' | '!=') comparison
    | comparison

comparison :
    | comparison ('<' | '<=' | '>' | '>=') terms
    | terms

terms :
    | terms ('+' | '-') factors

# no multiplication or division yet
factors :
    | unary

unary :
    | ('!' | '-' | '*') unary
    | primary

primary :
    | number
    | identifier
    | '&' identifier
    | call
    | grouping

call : identifier '(' arguments? ')'
arguments : expression (',' expression)*

grouping : '(' expression ')'

number : sign? value
sign : '+' | '-'
value :
    | <decimal literal>
    | '0x'<hex literal>
    | '0o'<octal literal>
    | '0b'<binary literal>
```
