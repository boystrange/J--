# J--

## Introduction

J-- is a compiler for the Java-- language, a core subset of Java
used to teach the first undergraduate programming course.

## Syntax

|     Entity |   | Productions                                            | Description           |
|-----------:|---|--------------------------------------------------------|-----------------------|
|    Program | : | Element*                                               |                       |
|    Element | : | Method                                                 |                       |
|            |   | Stmt                                                   |                       |
|     Method | : | Type ID `(` (Type ID (`,` Type ID)*)? `)` Stmt         |                       |
| SimpleStmt | : |                                                        | empty statement       |
|            |   | Expr                                                   | expression statement  |
|            |   | Type Init (`,` Init)*                                  | local variables       |
|       Stmt | : | SimpleStmt `;`                                         | simple statement      |
|            |   | `if` `(` Expr `)` Stmt (`else` Stmt)?                  | conditional statement |
|            |   | `while` `(` Expr `)` Stmt                              | while loop            |
|            |   | `do` Stmt `while` `(` Expr `)` `;`                     | do-while loop         |
|            |   | `for` `(` SimpleStmt `;` Expr? `;` SimpleStmt `)` Stmt | for loop              |
|            |   | `return` Expr? `;`                                     | return statement      |
|            |   | `assert` Expr `;`                                      | assertion             |
|            |   | `{` Stmt* `}`                                          | block                 |
|       Init | : | ID (`=` InitExpr)?                                     |                       |
|  ArrayExpr | : | `{` (InitExpr (`,` InitExpr)*)? `}`                    | array expression      |
|   InitExpr | : | Expr                                                   |                       |
|            |   | ArrayExpr                                              |                       |
|       Expr | : | Literal                                                | literal constant      |
|            |   | ID                                                     | reference             |
|            |   | ID `(` (Expr (`,` Expr)*)? `)`                         | method invocation     |
|            |   | `new` Type (`[` Expr `]`)+                             | array creation        |
|            |   | `new` Type ArrayExpr                                   | array initialization  |
|            |   | Expr `[` Expr `]`                                      | array reference       |
|            |   | Expr `.` `length`                                      | array length          |
|            |   | `(` Expr `)`                                           | parentheses           |
|            |   | `(` Type `)` Expr                                      | type cast             |
|            |   | Expr `=` Expr                                          | assignment            |
|            |   | Expr BINOP Expr                                        | infix binary operator |
|            |   | Expr STEP                                              | postfix step operator |
|            |   | STEP Expr                                              | prefix step operator  |
|            |   | UNOP Expr                                              | prefix unary operator |
|            |   | Expr `?` Expr `:` Expr                                 | ternary expression    |
|    Literal | : | INT                                                    |                       |
|            |   | FLOAT                                                  |                       |
|            |   | DOUBLE                                                 |                       |
|            |   | BOOLEAN                                                |                       |
|            |   | CHAR                                                   |                       |
|            |   | STRING                                                 |                       |
|       Type | : | `void`                                                 |                       |
|            |   | `boolean`                                              |                       |
|            |   | `int`                                                  |                       |
|            |   | `float`                                                |                       |
|            |   | `double`                                               |                       |
|            |   | `char`                                                 |                       |
|            |   | `String`                                               |                       |
|            |   | Type `[` `]`                                           |                       |
|      BINOP | : | MATHOP \| RELOP \| BOOLOP                              |                       |
|     MATHOP | : | `+` \| `-` \| `*` \| `/` \| `%`                        | math operations       |
|      RELOP | : | `==` \| `!=` \| `<` \| `>` \| `<=` \| `>=`             |                       |
|     BOOLOP | : | `&&` \| `\|\|`                                         |                       |
|       UNOP | : | `+` \| `-` \| `!`                                      |                       |
|       STEP | : | `++` \| `--`                                           |                       |
|    BOOLEAN | : | `true` \| `false`                                      |                       |
|      DIGIT | : | [`0`-`9`]                                              |                       |
|      ALPHA | : | [`a`-`z` `A`-`Z`]                                      |                       |
|       NEXT | : | ALPHA \| DIGIT \| `_`                                  |                       |
|         ID | : | ALPHA NEXT+                                            |                       |
|        NAT | : | DIGIT+                                                 |                       |
|        INT | : | `-`? NAT                                               |                       |
|      FLOAT | : | `-`? (NAT `.` NAT? \| NAT? `.` NAT) EXP?               |                       |
|     DOUBLE | : | FLOAT (`d` \| `D`)                                     |                       |
|        EXP | : | (`e` \| `E`) SIGN? NAT                                 |                       |
|       SIGN | : | `+` \| `-`                                             |                       |
