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

public class DeclStmt extends Stmt {
    private Bind bind;
    private Expr init;
    private Stmt stmt;

    public DeclStmt(Bind bind, Expr init, Stmt stmt) {
	this.bind = bind;
	this.init = init;
	this.stmt = stmt;
    }

    public boolean returns() {
	return stmt.returns();
    }

    public void typecheck(Env env, Type r) {
	bind.resolve(env.size());
	init.typecheck(env, bind.type);
	stmt.typecheck(env.enter(bind), r);
    }

    public int stack() {
	return Math.max(init.stack(), stmt.stack());
    }

    public int locals() {
	return bind.type.size() + stmt.locals();
    }

    public void compile(String lnext) {
	init.compile();
	Globals.emit("istore " + bind.address() + " ; " + bind.name);
	stmt.compile(lnext);
    }
}
