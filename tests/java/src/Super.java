class Super {
    public static void main(String [] argv) {
	for (int i=0; i<argv.length; i++) {
	    try {
		Class cl = Class.forName(argv[i]).getSuperclass();
		System.out.println(cl.getName());
	    } catch (Exception e) {
		System.out.println("undefined_class");
	    }
	}
    }
}
