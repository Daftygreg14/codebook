using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Lab7
{
    public class Wlosy
    {
        private ConsoleColor _kolor;
        private int _dlugosc;

        public Wlosy(ConsoleColor kolor, int dlugosc)
        {
            _kolor = kolor;
            _dlugosc = dlugosc;
        }

        public int Dlugosc { get { return _dlugosc; } }
        public ConsoleColor Kolor { get { return _kolor; } }

        public void Przytnij(int nowaDlugosc)
        {
            if(nowaDlugosc >= 0 && nowaDlugosc < _dlugosc) {
                Console.WriteLine("Przycinanie...");
                _dlugosc = nowaDlugosc;
            } else
            {
                Console.WriteLine("Nie mozna przyciac wlosow");
            }
        }

        public void Przefarbuj(ConsoleColor kolor)
        {
            Console.WriteLine("Farbowanie...");
            _kolor = kolor;
        }
        
    }
}
