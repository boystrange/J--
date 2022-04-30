// This file is part of J--

// J-- is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// J-- is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with J--. If not, see <http://www.gnu.org/licenses/>.

// Copyright 2022 Luca Padovani

import java.util.*;

public class TreeBuilder
    extends JavaMMBaseVisitor<Node>
    implements JavaMMVisitor<Node> {

    /////////////
    // PROGRAM //
    /////////////

    public Program visitProg(JavaMMParser.ProgContext ctxt) {
	List<Node> elements = new ArrayList<>();
	for (JavaMMParser.ElemContext elem : ctxt.elem())
	    elements.add((Node) visit(elem));
	return new Program(elements);
    }

    ///////////
    // TYPES //
    ///////////

    public Type visitAtomicType(JavaMMParser.AtomicTypeContext ctxt) {
	String s = ctxt.ATYPE().getText();
	if (s.equals("void")) return VoidType.instance;
	else if (s.equals("boolean")) return BooleanType.instance;
	else if (s.equals("int")) return IntType.instance;
	else throw Globals.internalerror("typeOfString: " + s);
    }

    //////////
    // BIND //
    //////////

    public Bind visitBind(JavaMMParser.BindContext ctxt) {
	Type t = (Type) visit(ctxt.type());
	String id = ctxt.ID().getText();
	return new Bind(t, id);
    }

    ////////////
    // METHOD //
    ////////////

    public Method visitMethod(JavaMMParser.MethodContext ctxt) {
	Type type = (Type) visit(ctxt.type());
	String name = ctxt.ID().getText();
	List<Bind> args = new ArrayList<>();
	for (JavaMMParser.BindContext bind : ctxt.bind())
	    args.add((Bind) visit(bind));
	List<Stmt> stmts = new ArrayList<>();
	for (JavaMMParser.StmtContext stmt : ctxt.stmt())
	    stmts.add((Stmt) visit(stmt));
	return new Method(type, name, args, new BlockStmt(stmts));
    }

    /////////////////
    // DECLARATION //
    /////////////////

    public DeclStmt visitDecl(JavaMMParser.DeclContext ctxt) {
	Type type = (Type) visit(ctxt.type());
	List<Init> inits = new ArrayList<>();
	for (JavaMMParser.InitContext init : ctxt.init())
	    inits.add((Init) visit(init));
	return new DeclStmt(type, inits);
    }

    // INITIALIZATION //

    public Init visitInit(JavaMMParser.InitContext ctxt) {
	String name = ctxt.ID().getText();
	if (ctxt.expr() != null) {
	    Expr expr = (Expr) visit(ctxt.expr());
	    return new Init(name, expr);
	} else
	    return new Init(name, null);
    }

    ///////////////
    // STATEMENT //
    ///////////////

    public Stmt visitBlock(JavaMMParser.BlockStmtContext ctxt) {
	List<Stmt> stmts = new ArrayList<>();
	for (JavaMMParser.StmtContext stmt : ctxt.stmt())
	    stmts.add((Stmt) visit(stmt));
	return new BlockStmt(stmts);
    }

    public Stmt visitIf(JavaMMParser.IfStmtContext ctxt) {
	Expr e = (Expr) visit(ctxt.expr());
	Stmt s1 = (Stmt) visit(ctxt.stmt(0));
	if (ctxt.stmt(1) != null) {
	    Stmt s2 = (Stmt) visit(ctxt.stmt(1));
	    return new IfStmt(e, s1, s2);
	} else
	    return new IfStmt(e, s1, null);
    }

    public Stmt visitWhile(JavaMMParser.WhileStmtContext ctxt) {
	Expr e = (Expr) visit(ctxt.expr());
	Stmt s = (Stmt) visit(ctxt.stmt());
	return new WhileStmt(e, s);
    }

    public Stmt visitDoWhile(JavaMMParser.DoWhileStmtContext ctxt) {
	Stmt s = (Stmt) visit(ctxt.stmt());
	Expr e = (Expr) visit(ctxt.expr());
	return new DoWhileStmt(s, e);
    }

    public Stmt visitReturn(JavaMMParser.ReturnStmtContext ctxt) {
	if (ctxt.expr() != null) {
	    Expr e = (Expr) visit(ctxt.expr());
	    return new ReturnStmt(e);
	} else
	    return new ReturnStmt(null);
    }

    public Stmt visitExprStmt(JavaMMParser.ExprStmtContext ctxt) {
	Expr e = (Expr) visit(ctxt.expr());
	return new ExprStmt(e);
    }

    /////////////
    // LITERAL //
    /////////////

    public Const visitInt(JavaMMParser.IntConstContext ctxt) {
	return null;
    }

    public Const visitFloat(JavaMMParser.FloatConstContext ctxt) {
	return null;
    }

    public Const visitDouble(JavaMMParser.DoubleConstContext ctxt) {
	return null;
    }

    public Const visitChar(JavaMMParser.CharConstContext ctxt) {
	return null;
    }

    public Const visitString(JavaMMParser.StringConstContext ctxt) {
	return null;
    }

    public Const visitBoolean(JavaMMParser.BooleanConstContext ctxt) {
	String s = ctxt.BOOLEAN().getText();
	return new BooleanConst(s.equals("true"));
    }

    ///////////////
    // REFERENCE //
    ///////////////

    ////////////////
    // EXPRESSION //
    ////////////////

    public Expr visitLiteral(JavaMMParser.LiteralExprContext ctxt) {
	Const c = (Const) visit(ctxt.literal());
	return new ConstExpr(c);
    }

    public Expr visitRef(JavaMMParser.RefExprContext ctxt) {
	Ref ref = (Ref) visit(ctxt.ref());
	return new RefExpr(ref);
    }

    public Expr visitCall(JavaMMParser.CallExprContext ctxt) {
	List<Expr> args = new ArrayList<>();
	for (JavaMMParser.ExprContext c : ctxt.expr())
	    args.add((Expr) visit(c));
	return new CallExpr(ctxt.ID().getText(), args);
    }

    public Node visitParens(JavaMMParser.ParensExprContext ctxt) {
	return visit(ctxt.expr());
    }

    public Expr visitAdd(JavaMMParser.AddExprContext ctxt) {
	Expr e1 = (Expr) visit(ctxt.expr(0));
	Expr e2 = (Expr) visit(ctxt.expr(1));
	switch (ctxt.op.getType()) {
	case JavaMMParser.ADD:
	    return new BinExpr(e1, e2, BinExpr.Op.ADD);
	case JavaMMParser.SUB:
	    return new BinExpr(e1, e2, BinExpr.Op.SUB);
	default:
	    throw Globals.internalerror("visitAdd");
	}
    }

    public Expr visitMul(JavaMMParser.MulExprContext ctxt) {
	Expr e1 = (Expr) visit(ctxt.expr(0));
	Expr e2 = (Expr) visit(ctxt.expr(1));
	switch (ctxt.op.getType()) {
	case JavaMMParser.MUL:
	    return new BinExpr(e1, e2, BinExpr.Op.MUL);
	case JavaMMParser.DIV:
	    return new BinExpr(e1, e2, BinExpr.Op.DIV);
	case JavaMMParser.MOD:
	    return new BinExpr(e1, e2, BinExpr.Op.REM);
	default:
	    throw Globals.internalerror("visitMul");
	}
    }

    public Expr visitRel(JavaMMParser.RelExprContext ctxt) {
	Expr e1 = (Expr) visit(ctxt.expr(0));
	Expr e2 = (Expr) visit(ctxt.expr(1));
	switch (ctxt.op.getType()) {
	case JavaMMParser.EQ:
	    return new RelExpr(e1, e2, RelExpr.Op.EQ);
	case JavaMMParser.NE:
	    return new RelExpr(e1, e2, RelExpr.Op.NE);
	case JavaMMParser.LT:
	    return new RelExpr(e1, e2, RelExpr.Op.LT);
	case JavaMMParser.GT:
	    return new RelExpr(e1, e2, RelExpr.Op.GT);
	case JavaMMParser.LE:
	    return new RelExpr(e1, e2, RelExpr.Op.LE);
	case JavaMMParser.GE:
	    return new RelExpr(e1, e2, RelExpr.Op.GE);
	default:
	    throw Globals.internalerror("visitRel");
	}
    }

    public Expr visitAnd(JavaMMParser.AndExprContext ctxt) {
	Expr e1 = (Expr) visit(ctxt.expr(0));
	Expr e2 = (Expr) visit(ctxt.expr(1));
	return new AndExpr(e1, e2);
    }

    public Expr visitOr(JavaMMParser.OrExprContext ctxt) {
	Expr e1 = (Expr) visit(ctxt.expr(0));
	Expr e2 = (Expr) visit(ctxt.expr(1));
	return new OrExpr(e1, e2);
    }

    public Expr visitNot(JavaMMParser.NotExprContext ctxt) {
	Expr e = (Expr) visit(ctxt.expr());
	return new NotExpr(e);
    }
}
