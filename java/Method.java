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

public class Method extends Node {
    public final Type type;
    public final String name;
    private List<Bind> args;
    private Stmt body;

    public Method(Type type, String name, List<Bind> args, Stmt body) {
	this.type = type;
	this.name = name;
	this.args = args;
	this.body = body;
    }

    public void typecheck(Env env) {
	for (Bind bind : args) {
	    bind.resolve(env.size());
	    env = env.enter(bind);
	}
	body.typecheck(env, type);
	if (!body.returns() && type != VoidType.instance)
	    throw Globals.typeerror("method " + name + " must return " + type);
    }

    public void compile() {
	Globals.emitmethod(".method public static " + name + typeof().descriptor());
	Globals.emitmethod("\t.limit stack " + body.stack());
	Globals.emitmethod("\t.limit locals " + locals());
	String lreturn = Globals.newlabel();
	body.compile(lreturn);
	if (!body.returns()) {
	    Globals.emitlabel(lreturn);
	    Globals.emit("return");
	}
	Globals.emitmethod(".end method");
    }

    public int locals() {
	int locals = 0;
	for (Bind bind : args) locals += bind.type.size();
	return locals + body.locals();
    }

    public Type typeof() {
	List<Type> targs = new ArrayList<>();
	for (Bind bind : args) targs.add(bind.type);
	return new MethodType(type, targs);
    }
}
