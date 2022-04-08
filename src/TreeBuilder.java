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

// Copyright 2022 Luca Padovaniimport org.antlr.v4.runtime.*;

import java.util.*;

public class TreeBuilder
    extends JavaMMBaseVisitor<Node>
    implements JavaMMVisitor<Node> {

    /////////////
    // PROGRAM //
    /////////////

    public Program visitProg(JavaMMParser.ProgContext ctxt) {
	List<Method> methods = new ArrayList<>();
	for (JavaMMParser.MethodContext method : ctxt.method())
	    methods.add((Method) visit(method));
	return new Program(methods);
    }

    ///////////
    // TYPES //
    ///////////

    public Type visitType(JavaMMParser.TypeContext ctxt) {
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
	Stmt body = (Stmt) visit(ctxt.slist());
	return new Method(type, name, args, body);
    }

    ////////////////////
    // STATEMENT LIST //
    ////////////////////

    public Stmt visitEmpty(JavaMMParser.EmptyContext ctxt) {
	return new EmptyStmt();
    }

    public Stmt visitSeq(JavaMMParser.SeqContext ctxt) {
	Stmt stmt = (Stmt) visit(ctxt.stmt());
	Stmt slist = (Stmt) visit(ctxt.slist());
	return slist instanceof EmptyStmt ? stmt : new SeqStmt(stmt, slist);
    }

    public Stmt visitDecl(JavaMMParser.DeclContext ctxt) {
	Bind bind = (Bind) visit(ctxt.bind());
	Expr init = (Expr) visit(ctxt.expr());
	Stmt slist = (Stmt) visit(ctxt.slist());
	return new DeclStmt(bind, init, slist);
    }

    ///////////////
    // STATEMENT //
    ///////////////

    public Stmt visitBlock(JavaMMParser.BlockContext ctxt) {
	Stmt slist = (Stmt) visit(ctxt.slist());
	return slist;
    }

    public Stmt visitAssign(JavaMMParser.AssignContext ctxt) {
	String id = ctxt.ID().getText();
	Expr e = (Expr) visit(ctxt.expr());
	return new AssignStmt(id, e);
    }

    public Stmt visitIf(JavaMMParser.IfContext ctxt) {
	Expr e = (Expr) visit(ctxt.expr());
	Stmt s1 = (Stmt) visit(ctxt.stmt(0));
	if (ctxt.stmt(1) != null) {
	    Stmt s2 = (Stmt) visit(ctxt.stmt(1));
	    return new IfStmt(e, s1, s2);
	} else
	    return new IfStmt(e, s1, null);
    }

    public Stmt visitWhile(JavaMMParser.WhileContext ctxt) {
	Expr e = (Expr) visit(ctxt.expr());
	Stmt s = (Stmt) visit(ctxt.stmt());
	return new WhileStmt(e, s);
    }

    public Stmt visitDoWhile(JavaMMParser.DoWhileContext ctxt) {
	Stmt s = (Stmt) visit(ctxt.stmt());
	Expr e = (Expr) visit(ctxt.expr());
	return new DoWhileStmt(s, e);
    }

    public Stmt visitReturn(JavaMMParser.ReturnContext ctxt) {
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

    ////////////////
    // EXPRESSION //
    ////////////////

    public Expr visitInt(JavaMMParser.IntContext ctxt) {
	int n = Integer.parseInt(ctxt.INT().getText());
	return new IntExpr(n);
    }

    public Expr visitBool(JavaMMParser.BoolContext ctxt) {
	String s = ctxt.BOOL().getText();
	return new BoolExpr(s.equals("true"));
    }

    public Expr visitId(JavaMMParser.IdContext ctxt) {
	String id = ctxt.ID().getText();
	return new IdExpr(id);
    }

    public Expr visitCall(JavaMMParser.CallContext ctxt) {
	List<Expr> args = new ArrayList<>();
	for (JavaMMParser.ExprContext c : ctxt.expr())
	    args.add((Expr) visit(c));
	return new CallExpr(ctxt.ID().getText(), args);
    }

    public Node visitParens(JavaMMParser.ParensContext ctxt) {
	return visit(ctxt.expr());
    }

    public Expr visitNot(JavaMMParser.NotContext ctxt) {
	Expr e = (Expr) visit(ctxt.expr());
	return new NotExpr(e);
    }

    public Expr visitAdd(JavaMMParser.AddContext ctxt) {
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

    public Expr visitMul(JavaMMParser.MulContext ctxt) {
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

    public Expr visitRel(JavaMMParser.RelContext ctxt) {
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

    public Expr visitAnd(JavaMMParser.AndContext ctxt) {
	Expr e1 = (Expr) visit(ctxt.expr(0));
	Expr e2 = (Expr) visit(ctxt.expr(1));
	return new AndExpr(e1, e2);
    }

    public Expr visitOr(JavaMMParser.OrContext ctxt) {
	Expr e1 = (Expr) visit(ctxt.expr(0));
	Expr e2 = (Expr) visit(ctxt.expr(1));
	return new OrExpr(e1, e2);
    }
}
