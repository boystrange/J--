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

public class WhileStmt extends Stmt {
    private Expr e;
    private Stmt s;

    public WhileStmt(Expr e, Stmt s) {
	this.e = e;
	this.s = s;
    }

    public void typecheck(Env env, Type ret) {
	e.typecheck(env, BooleanType.instance);
	s.typecheck(env, ret);
    }

    public int stack() {
	return Math.max(e.stack(), s.stack());
    }

    public int locals() {
	return s.locals();
    }

    public void compile(String lnext) {
	String ltest = Globals.newlabel();
	String ltrue = Globals.newlabel();
	Globals.emitlabel(ltest);
	e.compile(ltrue, lnext);
	Globals.emitlabel(ltrue);
	s.compile(ltest);
    }
}