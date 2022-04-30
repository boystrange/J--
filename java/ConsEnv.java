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

public class ConsEnv extends Env {
    private Bind bind;
    private Env next;

    public ConsEnv(Bind bind, Env env) {
	this.bind = bind;
	this.next = env;
    }

    public Bind lookup(String name) {
	return bind.name.equals(name) ? bind : next.lookup(name);
    }

    public int size() {
	return bind.type.size() + next.size();
    }
}
