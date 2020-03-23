using System;
using System.Linq;
using System.Collections.Generic;
namespace SpiseMisu
{
    public class MyEventArgs:EventArgs
    {
        public string Name { get; set; }
    }

    public static class StaticClass
    {
        public static List<TR> Method<T,TR>(List<T> list, Func<T,TR> map)
        {
            return (from p in list select map(p)).ToList();
        }
    }

    public class Class
    {
        public event EventHandler<MyEventArgs> SomeEvent;
        public List<int> Method(List<MyEventArgs> list)
        {
            return (from p in list select p.Name.Length).ToList();
        }
    }
}
