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

public abstract class LogicExpr extends Expr {
    public int stack() {
	// Conservatively assume that the compile() method below is
	// always invoked for every logic expression
	return 1;
    }

    public void compile() {
	String ltrue = Globals.newlabel();
	String lfalse = Globals.newlabel();
	String lnext = Globals.newlabel();
	this.compile(ltrue, lfalse);
	Globals.emitlabel(ltrue);
	Globals.emit("ldc 1");
	Globals.emit("goto " + lnext);
	Globals.emitlabel(lfalse);
	Globals.emit("ldc 0");
	Globals.emitlabel(lnext);
    }
}
