public class StandardLibrary {
    public static void print(String s) {
        System.out.print(s);
    }

    public static void println(String s) {
        System.out.println(s);
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
}