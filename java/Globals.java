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

public abstract class Globals {
    private static int nextLabel = 0;
    private static boolean pendingLabel = false;

    public static void emit(String code) {
	System.out.println("\t" + code);
	pendingLabel = false;
    }

    public static String newlabel() {
	return "L" + nextLabel++;
    }

    public static void emitlabel(String label) {
	if (pendingLabel) System.out.println("");
	System.out.print(label + ":");
	pendingLabel = true;
    }

    public static void emitmethod(String s) {
	System.out.println(s);
	pendingLabel = false;
    }

    public static RuntimeException typeerror(String msg) {
	return new RuntimeException("type error: " + msg);
    }

    public static RuntimeException typemismatch(Type t, Type s) {
	return typeerror(t + " does not match " + s);
    }

    public static RuntimeException internalerror(String msg) {
	return new RuntimeException("internal error: " + msg);
    }

    public static RuntimeException unknown(String id) {
	return new RuntimeException("unknown identifier: " + id);
    }
}
