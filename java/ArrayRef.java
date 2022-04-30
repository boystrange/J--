public class ArrayRef extends Ref {
    private Ref base;
    private Expr expr;

    public ArrayRef(Ref base, Expr expr) {
	this.base = base;
	this.expr = expr;
    }
}
