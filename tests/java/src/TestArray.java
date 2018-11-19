import java.util.Arrays;

class TestArray {
    
    public static void main(String[] args) {
        
        int[] intArray = new int[10];
	int[] cp = intArray.clone();
	
	System.out.println(intArray);
	System.out.println(cp);
	
	Object[] objArray = new Object[10];
	Object[] ocp = objArray.clone();

	String s = cp.toString();
	System.out.println(s);

	int i = cp.hashCode();
	System.out.println(i);
	
	Class c = cp.getClass();
	System.out.println(c);
	
	boolean b = cp.equals(intArray);
	System.out.println(b);
		
	String h = "hello";
	Class cl = String.class;
	Class al = int[][].class;
	System.out.println(cl);
	System.out.println(al);
    }
}
