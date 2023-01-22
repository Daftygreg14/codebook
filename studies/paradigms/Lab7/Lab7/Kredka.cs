using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Lab7
{
    public class Kredka
    {
        private ConsoleColor _kolor;
        public Kredka(ConsoleColor kolor)
        {
            _kolor = kolor;
        }

        public void Rysuj(char znak)
        {
            Console.ForegroundColor = _kolor;
            Console.Write(znak);
            Console.ResetColor();
        }
    }
}
