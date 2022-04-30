import java.util.regex.*;

public enum Token {
    WHITESPACE("\\s+"),
    NUM("\\d+"),
    WHILE("while"),
    DO("do"),
    IF("if"),
    THEN("then"),
    ELSE("else"),
    FI("fi"),
    TRUE("true"),
    FALSE("false"),
    PRINT("print"),
    ID("\\w+"),
    LPAREN("\\("),
    RPAREN("\\)"),
    LBRACE("\\{"),
    RBRACE("\\}"),
    LBRACK("\\["),
    RBRACK("\\]"),
    INC("\\+\\+"),
    DEC("\\-\\-"),
    ADD("\\+"),
    SUB("\\-"),
    MUL("\\*"),
    DIV("\\/"),
    MOD("\\%"),
    EQ("=="),
    NE("\\!="),
    GE(">="),
    LE("<="),
    GT(">"),
    LT("<"),
    AND("\\&\\&"),
    OR("\\|\\|"),
    NOT("\\!"),
    ASSIGN("="),
    SEMICOLON(";"),
    COLON(":"),
    COMMA(","),
    DOT("\\."),
    EOS("\\z"),
    SYMBOL("."),
    ;

    private final Pattern pattern;

    private Token(String regexp) {
	pattern = Pattern.compile(regexp);
    }

    public String match(String s) {
	Matcher m = pattern.matcher(s);
	return m.lookingAt() ? m.group() : null;
    }
}
