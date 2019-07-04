public class Properties {
    public static void main(String[] args)
        throws Exception {
        String value = System.getProperty("sun.boot.class.path");
	if (value != null) {
	    System.out.println(value);
	}
    }
}
