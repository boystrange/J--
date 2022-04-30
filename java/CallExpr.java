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

public class CallExpr extends Expr {
    private String name;
    private List<Expr> args;
    private MethodType type;

    public CallExpr(String name, List<Expr> args) {
	this.name = name;
	this.args = args;
	this.type = null;
    }

    public Type typeof(Env env) {
	Bind bind = env.lookup(name);
	if (!(bind.type instanceof MethodType))
	    Globals.typeerror(name + " is not a method");
	type = (MethodType) bind.type;
	if (type.args.size() != args.size())
	    throw Globals.typeerror(name + " expects " + type.args.size() + " argument(s) but is given " + args.size());
	for (int i = 0; i < args.size(); i++)
	    args.get(i).typecheck(env, type.args.get(i));
	return type.type;
    }

    public int stack() {
	int argsize = 0;
	int maxsize = 0;
	for (int i = 0; i < args.size(); i++) {
	    maxsize = Math.max(maxsize, argsize + args.get(i).stack());
	    argsize += type.args.get(i).size();
	}
	return Math.max(maxsize, type.type.size());
    }

    public void compile() {
	for (Expr e : args) e.compile();
	Globals.emit("invokestatic " + name + type.descriptor());
    }

    public void compile(String ltrue, String lfalse) {
	compile();
	Globals.emit("ifne " + ltrue);
	Globals.emit("goto " + lfalse);
    }
}
