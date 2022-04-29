grammar JavaMM;

prog	: elem* ;

elem	: method						# DeclElem
	| stmt							# StmtElem
	;

stmt	: ';'							# EmptyStmt
    	| 'if' '(' expr ')' stmt ('else' stmt)?                 # IfStmt
    	| 'while' '(' expr ')' stmt                             # WhileStmt
    	| 'do' stmt 'while' '(' expr ')' ';'                    # DoWhileStmt
	| 'for' '(' ';' expr ';' ')' stmt			# ForStmt
    	| 'return' expr? ';'                              	# ReturnStmt
    	| expr ';'	       	   				# ExprStmt
    	| '{' stmt* '}'                                         # BlockStmt
	| decl	    						# DeclStmt
    	;

decl	: type (init (',' init)*) ';'
	;

init	: ID ('=' expr)?
	;

ref	: ID							# IdRef
	| ref '[' expr ']'					# ArrayRef
	;

expr	: literal                                               # LiteralExpr
    	| ID '(' (expr (',' expr)* )? ')'			# CallExpr
	| 'new' type '[' expr ']'     				# NewExpr
	| ref '=' expr 	    	      				# AssignmentExpr
	| ref op=('++' | '--')					# PostExpr
	| op=('++' | '--') ref					# PreExpr
	| ref                                                   # RefExpr
	| '(' expr ')'                                          # ParensExpr
	| '!' expr                                              # NotExpr
	| expr op=('*' | '/' | '%') expr                        # MulExpr
	| expr op=('+' | '-') expr                              # AddExpr
	| expr op=('==' | '!=' | '<' | '>' | '<=' | '>=') expr	# RelExpr
	| expr '&&' expr                                        # AndExpr
	| expr '||' expr                                        # OrExpr
	;

literal	: INT							# IntConst
	| FLOAT							# FloatConst
	| DOUBLE						# DoubleConst
	| BOOLEAN						# BooleanConst
	| CHAR							# CharConst
	| STRING						# StringConst
	;

type	: ATYPE							# AtomicType
	| type '[' ']'						# ArrayType
	;

method  : type ID '(' (bind (',' bind)*)? ')' '{' stmt* '}' ;

bind	: type ID ;

ADD	: '+' ;
SUB	: '-' ;
MUL	: '*' ;
DIV	: '/' ;
MOD	: '%' ;
AND	: '&&' ;
OR	: '||' ;
NOT	: '!' ;
EQ	: '==' ;
NE	: '!=' ;
LT	: '<' ;
GT	: '>' ;
LE	: '<=' ;
GE	: '>=' ;

ATYPE	: 'void' | 'boolean' | 'int' | 'float' | 'double' | 'char' | 'String' ;
BOOLEAN : 'true' | 'false' ;  // match booleans
INT     : [0-9]+ ;        // match integer numbers
ID      : [a-zA-Z_]+ ;    // match lower-case identifiers

CHARLIT : [^\\] | '\\'[trn] ;

CHAR    : '\'' CHARLIT '\'' ;
STRING  : '"' CHARLIT* '"' ;

FLOAT	: SIGN? (INT '.' INT? | INT? '.' INT) EXP? ;
DOUBLE	: FLOAT 'd' ;
SIGN	: '+' | '-' ;
EXP	: ('e' | 'E') SIGN? INT ;

WS      : [ \t\r\n]+ -> skip ; // skip spaces, tabs, newlines
