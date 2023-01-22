using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Lab6
{
    internal class Kucharz
    {
        public void GotujObiad(Piekarnik piekarnik)
        {
            Console.WriteLine("Rozpoczynam gotowanie prosiaka");
            piekarnik.Uruchom(50, 60);
        }
    }
}
