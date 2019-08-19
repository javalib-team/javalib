public class LambdaInvokeInterface {

    interface IT
    {
	public int get_v();
    }

    static class T implements IT
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

    interface I
    {
	public int op(T t);
    }

    public static void main(String[] args)
    {
	I f = IT::get_v;
	T t = new T(42);
	int v = f.op(t);
	System.out.println(v);
    }
}
