using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace WebSharper.CSharp.Sitelets.Tests
{

    [JavaScript, EndPoint("/"), EndPoint("/home", order: 1), Serializable]
    public class CSharpEndPointRoot
    {
        public virtual int Tag => 0;
        public virtual object[] Data => new object[0];

        [Serializable]
        [EndPoint("/sub1/{X}")]
        [EndPoint("/sub1full/{X}", order: 1, inheritRoute: false)]
        public class Sub1 : CSharpEndPointRoot
        {
            public override int Tag => 1;
            public override object[] Data => new object[] { X };

            public int X; 
        }

        public override bool Equals(object obj)
        {
            switch (obj)
            {
                case CSharpEndPointRoot o:
                    return Tag == o.Tag && Data.SequenceEqual(o.Data);
                default:
                    return false;
            }
        }

        public override int GetHashCode() => Tuple.Create(Tag, Data).GetHashCode();
    }
}
