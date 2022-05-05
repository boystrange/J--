public class StandardLibrary {
    public static void print(String s) {
        System.out.print(s);
    }

    public static void println(String s) {
        System.out.println(s);
    }

    public static String boolean_to_string(boolean b) {
        return Boolean.toString(b);
    }

    public static String int_to_string(int n) {
        return Integer.toString(n);
    }

    public static String float_to_string(float n) {
        return Float.toString(n);
    }

    public static String double_to_string(double n) {
        return Double.toString(n);
    }

    public static String char_to_string(char c) {
        return Character.toString(c);
    }
}