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

public class Program extends Node {
    public List<Method> methods;

    public Program(List<Method> methods) {
	this.methods = methods;
    }

    public void typecheck() {
	List<Bind> binds = new ArrayList<>();
	for (Method method : methods)
	    binds.add(new Bind(method.typeof(), method.name));
	Env env = new NilEnv().enter(binds);
	for (Method method : methods)
	    method.typecheck(env);
    }

    public void compile() {
	for (Method method : methods) method.compile();
    }
}
