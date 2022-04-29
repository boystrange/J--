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

public class Expr extends Node {
    public Type typeof(Env env) { return null; }

    public void typecheck(Env env, Type t) {
	Type s = typeof(env);
	if (t != s) throw Globals.typemismatch(t, s);
    }

    public int stack() { return 0; }
    public void compile() { }

    public void compile(String ltrue, String lfalse) {
	throw new RuntimeException("internal error");
    }
}
