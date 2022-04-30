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

public class IfStmt extends Stmt {
    private Expr e;
    private Stmt s1;
    private Stmt s2;

    public IfStmt(Expr e, Stmt s1, Stmt s2) {
	this.e = e;
	this.s1 = s1;
	this.s2 = s2;
    }

    public boolean returns() {
	return s2 != null && s1.returns() && s2.returns();
    }

    public void typecheck(Env env, Type r) {
	e.typecheck(env, BooleanType.instance);
	s1.typecheck(env, r);
	if (s2 != null) s2.typecheck(env, r);
    }

    public int stack() {
	if (s2 == null) return Math.max(e.stack(), s1.stack());
	else return Math.max(e.stack(), Math.max(s1.stack(), s2.stack()));
    }

    public int locals() {
	return s2 == null ? s1.locals() : Math.max(s1.locals(), s2.locals());
    }

    public void compile(String lnext) {
	if (s2 == null) {
	    String ltrue = Globals.newlabel();
	    e.compile(ltrue, lnext);
	    Globals.emitlabel(ltrue);
	    s1.compile(lnext);
	} else {
	    String ltrue = Globals.newlabel();
	    String lfalse = Globals.newlabel();
	    e.compile(ltrue, lfalse);
	    Globals.emitlabel(ltrue);
	    s1.compile(lnext);
	    Globals.emitlabel(lfalse);
	    s2.compile(lnext);
	}
    }
}
