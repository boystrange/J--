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

public class BinExpr extends Expr {
    public static enum Op { ADD, SUB, MUL, DIV, REM };
    private Expr e1;
    private Expr e2;
    private Op op;

    public BinExpr(Expr e1, Expr e2, Op op) {
	this.e1 = e1;
	this.e2 = e2;
	this.op = op;
    }

    public Type typeof(Env env) {
	e1.typecheck(env, IntType.instance);
	e2.typecheck(env, IntType.instance);
	return IntType.instance;
    }

    public int stack() {
	return Math.max(e1.stack(), IntType.instance.size() + e2.stack());
    }

    public void compile() {
	e1.compile();
	e2.compile();
	Globals.emit("i" + op.toString().toLowerCase());
    }
}
