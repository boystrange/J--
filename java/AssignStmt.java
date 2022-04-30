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

public class AssignStmt extends Stmt {
    private String name;
    private Expr expr;
    private Bind bind;

    public AssignStmt(String name, Expr expr) {
	this.name = name;
	this.expr = expr;
	this.bind = null;
    }

    public void typecheck(Env env, Type r) {
	bind = env.lookup(name);
	expr.typecheck(env, bind.type);
    }

    public int stack() {
	return expr.stack();
    }

    public void compile(String lnext) {
	expr.compile();
	Globals.emit("istore " + bind.address() + " ; " + name);
	Globals.emit("goto " + lnext);
    }
}
