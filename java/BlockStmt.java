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

public class BlockStmt extends Stmt {
    private List<Stmt> stmts;

    public BlockStmt(List<Stmt> stmts) {
	this.stmts = stmts;
    }

    public boolean returns() {
	assert false;
	// return s.returns();
	return false;
    }

    public void typecheck(Env env, Type r) {
	assert false;
	// s.typecheck(env, r);
    }

    public int stack() {
	assert false;
	// return s.stack();
	return 0;
    }

    public int locals() {
	assert false;
	// return s.locals();
	return 0;
    }

    public void compile(String lnext) {
	assert false;
	// s.compile(lnext);
    }
}
