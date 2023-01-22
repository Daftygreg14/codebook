using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Lab6
{
    internal class Program
    {
        static void Main(string[] args)
        {
            Bosh piekarnik = new Bosh();
            Kucharz kucharz = new Kucharz();

            kucharz.GotujObiad(piekarnik);
        }
    }
}
