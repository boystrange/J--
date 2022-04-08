
NULL =

SOURCES = \
  JavaMMLexer.java \
  JavaMMParser.java \
  JavaMMVisitor.java \
  JavaMMBaseVisitor.java \
  Globals.java \
  Program.java \
  Method.java \
  Bind.java \
  Type.java \
  VoidType.java \
  IntType.java \
  BooleanType.java \
  MethodType.java \
  Node.java \
  Stmt.java \
  EmptyStmt.java \
  AssignStmt.java \
  IfStmt.java \
  WhileStmt.java \
  DoWhileStmt.java \
  ExprStmt.java \
  ReturnStmt.java \
  BlockStmt.java \
  SeqStmt.java \
  DeclStmt.java \
  Expr.java \
  CallExpr.java \
  LogicExpr.java \
  NotExpr.java \
  RelExpr.java \
  IntExpr.java \
  BoolExpr.java \
  AndExpr.java \
  OrExpr.java \
  IdExpr.java \
  BinExpr.java \
  TreeBuilder.java \
  JavaMM.java \
  Env.java \
  NilEnv.java \
  ConsEnv.java \
  $(NULL)

CLASSES = $(SOURCES:%.java=%.class)

JAR = JavaMM.jar
ANTLRJAR = /usr/local/Cellar/antlr/4.9.3/antlr-4.9.3-complete.jar

all: $(JAR)

run: $(JAR)
	@java -classpath .:$(JAR):$(ANTLRJAR) MiniJavaC -

$(JAR): $(SOURCES)
	@javac -classpath $(ANTLRJAR) $^
	@jar -c -e MiniJavaC -f $@ $(CLASSES)

%.interp %Lexer.java %Parser.java %Visitor.java %BaseVisitor.java: %.g4
	@antlr -visitor -no-listener $<

%.class: %Lexer.java %Parser.java
	@javac -classpath $(ANTLRJAR) $^

clean:
	@rm -f *Lexer.java *Parser.java *Visitor.java *BaseVisitor.java *.class *.tokens *.interp $(JAR)
