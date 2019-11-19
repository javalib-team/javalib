import java.util.function.*;

interface I
{
    public String op(ExampleLambda t, String a, String b);
}

interface J
{
    String s1 = "Default";
    String s2 = "Static";

    public String virtual(String a, String b);

    public default void test_default(int i) {
	Consumer<String> c = v -> System.out.println(v + " " + i);
        c.accept(s1);
	System.out.println("Interface call : " + virtual(s1, s1));
    }

    public static void test_static(int i) {
	Consumer<String> c = v -> System.out.println(v + " " + i);
        c.accept(s2);
    }
}

class ExampleLambda implements J {

    private String s1;
    
    public String virtual(String a, String b) {
	return "virtual("+s1+"("+a+","+b+"))";
    }

    private String special(String a, String b) {
	return "special("+s1+"["+a+";"+b+"])";
    }
    
    public Supplier<String> get_s() {
	return () -> "special("+this.s1+")";
    }

    public static void main(String [] argv) {
	Supplier<Integer> supplier = null;
	try {
	    supplier = () -> Integer.valueOf(42+Integer.parseInt(argv[0]));
	    System.out.println(supplier.get());
	} catch (Exception e) {
	    System.out.println("First argument should be an int.");
	}

	try {
	    String s = "value";
	    Consumer<String> consumer = (value) -> System.out.println("("+s+","+value+")");
	    consumer.accept(argv[1]);
	} catch (Exception e) {
	    System.out.println("Second argument should be an string.");
	}
	
	IntConsumer cint = (i) -> System.out.println(i);
	cint.accept(Integer.parseInt(argv[0]));

	long ll1 = 22L;
	long ll2 = 10L;
	long ll3 = 10L;
	final Supplier<Integer> supp = supplier;
	LongConsumer clong = (l) -> System.out.println(l+ll1+ll2+ll3+supp.get());
	clong.accept(Integer.parseInt(argv[0]));
	
	ExampleLambda e = new ExampleLambda();
	e.s1 = argv[1];
	e.test_default(42);
	J.test_static(-42);
	I f = ExampleLambda::virtual;
	System.out.println("Test Invoke Virtual : " + f.op(e,"arg1","arg2"));
	I g = J::virtual;
	System.out.println("Test Invoke Interface : " + g.op(e,"arg1","arg2"));
	I h = ExampleLambda::special;
	System.out.println("Test Invoke Special : " + h.op(e,"arg1","arg2"));

	Supplier<String> ss = e.get_s();
	System.out.println(ss.get());

	Function<String,String> func  = String::new;
	System.out.println(func.apply("Test New Invoke Special"));
    }
}
