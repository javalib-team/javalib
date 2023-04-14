public class Test3 {
	public static int Test3(String[] args) {
		boolean c = true && false;
		boolean d = true && false;
		return c ? (d ? 1 : 2) : (d ? 3 : 4);
	}
}
