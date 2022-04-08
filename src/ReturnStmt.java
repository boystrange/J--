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

public class ReturnStmt extends Stmt {
    private Expr e;

    public ReturnStmt(Expr e) {
	this.e = e;
    }

    public boolean returns() {
	return true;
    }

    public void typecheck(Env env, Type r) {
	if (e == null) {
	    if (r != VoidType.instance)
		throw Globals.typemismatch(r, VoidType.instance);
	} else e.typecheck(env, r);
    }

    public int stack() {
	return e == null ? 0 : e.stack();
    }

    public void compile(String lnext) {
	if (e == null)
	    Globals.emit("return");
	else {
	    e.compile();
	    Globals.emit("ireturn"); // type dependent
	}
    }
}
