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

// Copyright 2022 Luca Padovani

import java.util.*;

public class DeclStmt extends Stmt {
    private Type type;
    private List<Init> inits;

    public DeclStmt(Type type, List<Init> inits) {
	this.type = type;
	this.inits = inits;
    }

    public boolean returns() {
	return false;
    }

    public void typecheck(Env env, Type r) {
	// bind.resolve(env.size());
	// if (init != null) init.typecheck(env, bind.type);
    }

    public int stack() {
	assert false;
	// return init != null ? init.stack() : 0;
	return 0;
    }

    public int locals() {
	assert false;
	//return bind.type.size();
	return 0;
    }

    public void compile(String lnext) {
	// if (init != null) {
	//     init.compile();
	//     Globals.emit("istore " + bind.address() + " ; " + bind.name);
	// }
	assert false; // GOTO lnext
    }
}
