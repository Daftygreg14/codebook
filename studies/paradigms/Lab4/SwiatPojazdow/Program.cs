using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace SwiatPojazdow
{
    internal class Program
    {
        static void Main(string[] args)
        {
            Samochod ferrari = new Samochod();
            ferrari.Predkosc = 100;
            Console.WriteLine(ferrari.Predkosc);
            ferrari.Predkosc = 200;
            Console.WriteLine(ferrari.Predkosc);
            ferrari.Predkosc = -100;
            Console.WriteLine(ferrari.Predkosc);

            Samochod proche = new Samochod(230);
            proche.Predkosc = 100;
            Console.WriteLine(proche.Predkosc);
            proche.Predkosc = 200;
            Console.WriteLine(proche.Predkosc);
            proche.Predkosc = -100;
            Console.WriteLine(proche.Predkosc);
        }
    }
}
