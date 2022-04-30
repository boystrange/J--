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

public class MethodType extends Type {
    public final Type type;
    public final List<Type> args;

    public MethodType(Type type, List<Type> args) {
	this.type = type;
	this.args = args;
    }

    public String toString() {
	return "method";
    }

    public int size() {
	return 0;
    }

    public String descriptor() {
	String desc = "(";
	for (Type t : args) desc += t.descriptor();
	return desc + ")" + type.descriptor();
    }
}
