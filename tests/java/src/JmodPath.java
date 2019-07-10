public class JmodPath {
    public static void main(String[] args)
    {
        String value = System.getProperty("java.home");
	String[] parts = value.split("/");
	String last = parts[parts.length-1];
	if (! last.equals("jre")) {
	    System.out.println(value.concat("/jmods"));
	}
    }
}
