using System;
namespace SpiseMisu
{
    [System.AttributeUsage(System.AttributeTargets.All, Inherited = false, AllowMultiple = true)]
    public sealed class MyAttribute : System.Attribute
    {
        readonly string positionalString;
        
        public MyAttribute (string positionalString)
        {
            this.positionalString = positionalString;
        }
        
        public string PositionalString
        {
            get { return positionalString; }
        }
        
        // This is a named argument
        public int NamedInt { get; set; }
    }

    [MyAttribute("<Enum>", NamedInt=1)]
    public enum Enum
    {
        [MyAttribute("<Foo>", NamedInt=2)]
        Foo=0,
        [MyAttribute("<Bar>", NamedInt=3)]
        [Obsolete("Use Foo")]
        Bar=42
    }
    [MyAttribute("<Class>", NamedInt=4)]
    public class Class
    {
        public void Method([MyAttribute("<Class>", NamedInt=4)]int parameter1)
        {
        }
    }
}
