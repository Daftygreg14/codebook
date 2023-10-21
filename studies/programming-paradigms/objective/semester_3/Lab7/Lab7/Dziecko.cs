using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Lab7
{
    public class Dziecko
    {
        private Wlosy _wlosy;
        private Relacja[] _relacje; 

        public Dziecko(ConsoleColor wlosyColor, int wlosyDlugosc)
        {
            _wlosy = new Wlosy(wlosyColor, wlosyDlugosc);
            _relacje = new Relacja[] { };
        }

        public void OpiszSwojeWlosy()
        {
            Console.WriteLine("Mam " + _wlosy.Dlugosc + " cm dlugich wlosow o kolorze " + _wlosy.Kolor);
        }

        public void CzyMaszPrzyjaciela()
        {
            if(_relacje.Any((el) => el.CzyPrzyjazn() && el.CzyTrwa()))
            {
                Console.WriteLine("Tak Mam!!!");
            }
            else 
            {
                Console.WriteLine("Nie, nie mam..."); 
            }
        }
        public void PrzytnijWlosy(int dlugosc)
        {
            _wlosy.Przytnij(dlugosc);
        }

        public void PrzefarbujWlosy(ConsoleColor color)
        {
            _wlosy.Przefarbuj(color);
        }

        public void Napisz(string Text, Kredka kredka)
        {
            foreach (char znak in Text)
            {
                kredka.Rysuj(znak);
            }
        } 

        public void ZaprzyjaznijSie(Dziecko dziecko)
        {
            Relacja nowaRelacja = new Relacja(this, dziecko, Relacja.EnumRodzajRelacji.Przyjaciel);
            _relacje = _relacje.Append(nowaRelacja).ToArray();
        }
    }
}
