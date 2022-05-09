// This file is part of J--

// J-- is free software: you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the Free Software
// Foundation, either version 3 of the License, or (at your option) any later
// version.

// J-- is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
// A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along with
// J--. If not, see <http://www.gnu.org/licenses/>.

// Copyright 2022 Luca Padovani

import java.util.Scanner;

public class StandardLibrary {
    public static void print(String s) {
        System.out.print(s);
    }

    public static void println(String s) {
        System.out.println(s);
    }

    public static int getInt(String s) {
        System.out.print(s + ": ");
        return new Scanner(System.in).nextInt();
    }

    public static float getFloat(String s) {
        System.out.print(s + ": ");
        return new Scanner(System.in).nextFloat();
    }

    public static double getDouble(String s) {
        System.out.print(s + ": ");
        return new Scanner(System.in).nextDouble();
    }

    public static char getChar(String s) {
        return getString(s).charAt(0);
    }

    public static String getString(String s) {
        System.out.print(s + ": ");
        return new Scanner(System.in).nextLine();
    }

    public static String boolean_to_String(boolean b) {
        return Boolean.toString(b);
    }

    public static String int_to_String(int n) {
        return Integer.toString(n);
    }

    public static String float_to_String(float n) {
        return Float.toString(n);
    }

    public static String double_to_String(double n) {
        return Double.toString(n);
    }

    public static String char_to_String(char c) {
        return Character.toString(c);
    }

    public static String String_concat(String x, String y) {
        return x + y;
    }

    public static void check_assertion(boolean b, String msg) {
        if (!b) throw new AssertionError(msg);
    }

    public static double milliseconds() {
        return System.currentTimeMillis() / 1000.0d;
    }
}