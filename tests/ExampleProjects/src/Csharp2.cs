namespace SpiseMisu
{
    public interface IFooBar
    {
        void FooBar();
        int GetSet { get; set; }
    }

    public enum Enum
    {
        Foo = 0,
        Bar = 42
    }

    public struct Struct
    {
        public int Foo;
        public float Bar;

        public Struct(int foo, float bar)
        {
            Foo = foo;
            Bar = bar;
        }

        public int StructMethod()
        {
            return 42;
        }
    }

    public static class StaticClass
    {
        public static int StaticMethod(int x, int y)
        {
            return x + y + 42;
        }
    }

    public class Class
    {
        public Class()
        {

        }

        public Class(int parameter1)
        {

        }

        public static int Constant { get { return 42; } }

        public int GetSet { get; set; }

        public static int StaticMethod()
        {
            return 42;
        }

        public int Method()
        {
            return 42;
        }
    }
}
