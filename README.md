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
|     Method | : | Type `ID` `(` (Type ID (`,` Type ID)*)? `)` Stmt       |                       |
| SimpleStmt | : |                                                        | empty statement       |
|            |   | Expr                                                   | expression statement  |
|            |   | Type Init (`,` Init)*                                  | local variables       |
|       Stmt | : | SimpleStmt `;`                                         | simple statement      |
|            |   | `if` `(` Expr `)` Stmt (`else` Stmt)?                  | conditional statement |
|            |   | `while` `(` Expr `)` Stmt                              | while loop            |
|            |   | `do` Stmt `while` `(` Expr `)` `;`                     | do-while loop         |
|            |   | `for` `(` SimpleStmt `;` Expr? `;` SimpleStmt `)` Stmt | for loop              |
|            |   | `return` Expr? `;`                                     | return statement      |
|            |   | `{` Stmt* `}`                                          | block                 |
|       Init | : | ID (`=` Expr)?                                         |                       |
|        Ref | : | ID                                                     |                       |
|            |   | ref `[` Expr `]`                                       |                       |
|       Expr | : | Literal                                                | literal constant      |
|            |   | ID `(` (Expr (`,` Expr)*)? `)`                         | method invocation     |
|            |   | `new` Type (`[` Expr `]`)+                             | array creation        |
|            |   | Ref                                                    | reference             |
|            |   | `(` Expr `)`                                           | parentheses           |
|            |   | Ref `=` Expr                                           | assignment            |
|            |   | `(` Type `)` Expr                                      | type cast             |
|            |   | Ref STEPOP                                             | postfix step operator |
|            |   | STEPOP Ref                                             | prefix step operator  |
|            |   | PREOP Expr                                             | prefix unary operator |
|            |   | Expr BINOP Expr                                        | infix binary operator |
|    Literal | : | `null`                                                 | null reference        |
|            |   | INT                                                    |                       |
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
|      BINOP | : | `+` \| `-` \| `*` \| `/` \| `%`                        |                       |
|            |   | `==` \| `!=` \| `<` \| `>` \| `<=` \| `>=`             |                       |
|            |   | `&&` \| `\|\|`                                         |                       |
|      PREOP | : | `+` \| `-`                                             |                       |
|            |   | `!`                                                    |                       |
|     STEPOP | : | `++` \| `--`                                           |                       |
|    BOOLEAN | : | `true` \| `false`                                      |                       |
|      DIGIT | : | [`0`-`9`]                                              |                       |
|      ALPHA | : | [`a`-`z` `A`-`Z`]                                      |                       |
|       NEXT | : | ALPHA \| DIGIT \| `_`                                  |                       |
|         ID | : | ALPHA NEXT+                                            |                       |
|        NAT | : | DIGIT+                                                 |                       |
|        INT | : | MINUS? NAT                                             |                       |
|      FLOAT | : | MINUS? (NAT `.` NAT? \| NAT? `.` NAT) EXP?             |                       |
|     DOUBLE | : | FLOAT (`d` \| `D`)                                     |                       |
|        EXP | : | (`e` \| `E`) SIGN? NAT                                 |                       |
|      MINUS | : | `-`                                                    |                       |
|       SIGN | : | `+` \| `-`                                             |                       |
