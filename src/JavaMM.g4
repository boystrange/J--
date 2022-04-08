grammar JavaMM;

prog	: method* ;

stmt	: ID '=' expr ';'                                       # Assign
    	| 'if' '(' expr ')' stmt ('else' stmt)?                 # If
    	| 'while' '(' expr ')' stmt                             # While
    	| 'do' stmt 'while' '(' expr ')' ';'                    # DoWhile
    	| 'return' expr? ';'                              	# Return
    	| expr ';'	       	   				# ExprStmt
    	| '{' slist '}'                                         # Block
    	;

slist	:							# Empty
	| bind '=' expr ';' slist				# Decl
	| stmt slist						# Seq
	;

expr	: INT                                                   # Int
    	| BOOL    						# Bool
    	| ID '(' (expr (',' expr)* )? ')'			# Call
	| ID                                                    # Id
	| '(' expr ')'                                          # Parens
	| '!' expr                                              # Not
	| expr op=('*' | '/' | '%') expr                        # Mul
	| expr op=('+' | '-') expr                              # Add
	| expr op=('==' | '!=' | '<' | '>' | '<=' | '>=') expr	# Rel
	| expr '&&' expr                                        # And
	| expr '||' expr                                        # Or
	;

type	: ATYPE ;

method  : type ID '(' (bind (',' bind)*)? ')' '{' slist '}' ;

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

ATYPE	: 'void' | 'boolean' | 'int' ;
BOOL    : 'true' | 'false' ;  // match booleans
INT     : [0-9]+ ;        // match integer numbers
ID      : [a-zA-Z_]+ ;    // match lower-case identifiers

WS      : [ \t\r\n]+ -> skip ; // skip spaces, tabs, newlines
