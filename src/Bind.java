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

public class Bind extends Node {
    public final Type   type;
    public final String name;
    private int addr;

    public Bind(Type type, String name) {
	this.name = name;
	this.type = type;
	this.addr = -1;
    }

    public void resolve(int addr) {
	if (this.addr >= 0) Globals.internalerror(name + " resolved twice");
	this.addr = addr;
    }

    public int address() {
	return addr;
    }
}
