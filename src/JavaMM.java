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

import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.tree.*;
import java.io.FileInputStream;
import java.io.InputStream;

public class JavaMM {
    public static void main(String[] args) throws Exception {
	if (args.length != 1) {
	    System.out.println("Java-- Compiler 0.1");
	    System.out.println("Usage: j-- <source>");
	    System.out.println("where <source> is a file name or - for standard input.");
	    System.exit(-1);
	}
	CharStream chars = args[0].equals("-")
	    ? CharStreams.fromStream(System.in)
	    : CharStreams.fromFileName(args[0]);
	JavaMMLexer lexer = new JavaMMLexer(chars);
	CommonTokenStream tokens = new CommonTokenStream(lexer);
	JavaMMParser parser = new JavaMMParser(tokens);
	ParseTree tree = parser.prog();
	Program prog = (Program) new TreeBuilder().visit(tree);
	// prog.typecheck();
	// prog.compile();
    }
}
