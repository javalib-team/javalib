public class LambdaInvokeSpecial {

    class T
    {
	private int v;

	public T(int v)
	{
	    this.v = v;
	}

	public int get_v()
	{
	    return this.v;
	}
    }

    public T get_T()
    {
	return new T(42);
    }
    
    interface I
    {
	public T op();
    }

    public int test()
    {
	I f = () -> this.get_T();
	int v = f.op().get_v();
	return v;
    }
    
    public static void main(String[] args)
    {
	int v = (new LambdaInvokeSpecial()).test();
	System.out.println(v);
    }
}
