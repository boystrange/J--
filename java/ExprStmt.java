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

public class ExprStmt extends Stmt {
    private Expr expr;
    private Type type;

    public ExprStmt(Expr expr) {
	this.expr = expr;
    }

    public void typecheck(Env env, Type r) {
	type = expr.typeof(env);
    }

    public int stack() {
	return expr.stack();
    }

    public void compile(String lnext) {
	expr.compile();
	for (int i = 0; i < type.size(); i++) Globals.emit("pop");
	Globals.emit("goto " + lnext);
    }
}
