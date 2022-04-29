# J--

## Introduction

J-- is a compiler for the Java-- language, a core subset of Java
used to teach the first undergraduate programming course.

## Syntax

|---------|---|--------------------------------------------|-----------------|
| Prog    | : | Elem*                                      |                 |
| Elem    | : | Method                                     |                 |
|         |   | Stmt                                       |                 |
| Method  | : | Type `ID` `(` Args `)` `{` Slist `}`       |                 |
| Args    | : | ( Type ID (`,` Type ID)* )?                |                 |
| Stmt    | : | `;`                                        | Empty statement |
|         |   | `if` `(` Expr `)` Stmt (`else` Stmt)?      | Conditional     |
|         |   | `while` `(` Expr `)` Stmt                  | while loop      |
|         |   | `do` Stmt `while` `(` Expr `)` `;`         | do-while loop   |
|         |   | `for` `(` Stmt `;` Expr `;` Stmt `)` Stmt  | for loop        |
|         |   | `return` Expr? `;`                         |                 |
|         |   | Expr `;`                                   |                 |
|         |   | `{` Slist `}`                              |                 |
| Slist   | : | (Stmt \| Decl)*                            |                 |
| Decl    | : | Type ( Init (`,` Init)* ) `;`              |                 |
| Init    | : | ID (`=` Expr)?                             |                 |
| Ref     | : | ID                                         |                 |
|         |   | ref `[` Expr `]`                           |                 |
| Expr    | : | Lit                                        |                 |
|         |   | ID `(` (Expr (`,` Expr)*)? `)`             |                 |
|         |   | `new` Type `[` Expr `]`                    |                 |
|         |   | Ref `=` Expr                               |                 |
|         |   | Ref POSTOP                                 |                 |
|         |   | PREOP Ref                                  |                 |
|         |   | Ref                                        |                 |
|         |   | `(` Expr `)`                               |                 |
|         |   | Expr BINOP Expr                            |                 |
| Lit     | : | INT                                        |                 |
|         |   | FLOAT                                      |                 |
|         |   | DOUBLE                                     |                 |
|         |   | BOOLEAN                                    |                 |
|         |   | CHAR                                       |                 |
|         |   | STRING                                     |                 |
| Type    | : | AType                                      |                 |
|         |   | Type `[` `]`                               |                 |
| AType   | : | `int`                                      |                 |
|         |   | `float`                                    |                 |
|         |   | `double`                                   |                 |
|         |   | `boolean`                                  |                 |
|         |   | `char`                                     |                 |
|         |   | `String`                                   |                 |
|         |   | `void`                                     |                 |
| BINOP   | : | `+` \| `-` \| `*` \| `/` \| `%`            |                 |
|         |   | `==` \| `!=` \| `<` \| `>` \| `<=` \| `>=` |                 |
|         |   | `&&` \| `\|\|`                             |                 |
| PREOP   | : | `+` \| `-`                                 |                 |
|         |   | `!`                                        |                 |
|         |   | `++` \| `--`                               |                 |
| POSTOP  | : | `++` \| `--`                               |                 |
| BOOLEAN | : | `true` \| `false`                          |                 |
| ID      | : | [`a`-`z``A`-`Z`]+                          |                 |
| NAT     | : | [`0`-`9`]+                                 |                 |
| INT     | : | SIGN? NAT                                  |                 |
| FLOAT   | : | SIGN? (NAT `.` NAT? \| NAT? `.` NAT) EXP?  |                 |
| DOUBLE  | : | FLOAT `d`                                  |                 |
| EXP     | : | (`e` \| `E`) INT                           |                 |
| SIGN    | : | `+` \| `-`                                 |                 |
