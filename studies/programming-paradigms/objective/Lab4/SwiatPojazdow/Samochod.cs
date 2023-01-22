using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace SwiatPojazdow
{
    internal class Samochod
    {
        private double _predkosc, _predkoscMax, _zbiornik, _paliwo, _dystans;
        /// <summary>
        /// Konstruktor samochodu ustawiajacy predkosc max 180
        /// </summary>
        public Samochod()
        {
            _predkosc = 0;
            _predkoscMax = 230;
            _dystans = 0;
            _zbiornik = 50;
            _paliwo = 5;
        }
        /// <summary>
        /// Konstrukro samochodu ustawiajacy predkosc max do 330
        /// </summary>
        /// <param name="vMax"></param>
        public Samochod(double vMax):this()
        {
            _predkoscMax = calculateVMax(vMax);
        }
        /// <summary>
        /// Konstruktor samochodu ustawiajacy wszystkie parametry
        /// </summary>
        /// <param name="vMax"></param>
        /// <param name="dystans"</param>
        /// <param name="paliwo"</param>
        /// <param name="zbiornik"</param>
        /// <returns></returns>
        public Samochod(double vMax, double dystans, double zbiornik, double paliwo) :this()
        {
            _predkoscMax = calculateVMax(vMax);
            _dystans = dystans >= 0 ? dystans : 0;
            _zbiornik = zbiornik >= 0 ? zbiornik : 50;
            _paliwo = paliwo <= _zbiornik ? paliwo : _zbiornik;
        }
        ~Samochod()
        {
            Console.WriteLine("Pamietaj o zlomowaniu");
        }

        private double calculateVMax(double vMax)
        {
            if (vMax < 330 && vMax > 0)
            {
               return vMax;
            }
            else
            {
                return 330;
            }
        }

        public double Predkosc
        {
            get { return _predkosc; }
            set
            {
                if (value <= _predkoscMax & value >= 0)
                {
                    _predkosc = value;
                } 
                else if (value < 0) {
                    _predkosc = 0.2 * _predkoscMax;
                }
                else
                {
                    _predkosc = _predkoscMax;
                }
            }
        }

        public double PredkoscKmH
        {
            get { return Math.Round((_predkosc * 1000) / 3600, 2); }
        }

        public double Paliwo
        {
            get { return _paliwo; }
        }

        public void Przyspiesz(double nowaPredkosc)
        {
            double predkoscPoczatkowa = Predkosc;
            double wzrost = nowaPredkosc - predkoscPoczatkowa;
            double zuzyciePaliwa = wzrost > 0 ? 0.2 * wzrost : 0;

            if (predkoscPoczatkowa < nowaPredkosc && nowaPredkosc > 0 && zuzyciePaliwa < Paliwo)
            {
                Predkosc = nowaPredkosc;
                _paliwo = wzrost > 0 ? (_paliwo - 0.2 * wzrost) : _paliwo;
            }
        }

        public void Hamuj(double predkosc)
        {
            double predkoscPoczatkowa = Predkosc;
            double predkoscKoncowa = predkoscPoczatkowa - predkosc;

            if (predkoscPoczatkowa >= predkosc && predkoscKoncowa >= 0)
            {
                Predkosc = predkoscKoncowa;
            }
        }

        public string Wyswietl()
        {
            return $"Zbiornik: #{_zbiornik}.\nStan baku: #{Paliwo}.\n";
        }

        public double Tankuj(double paliwo)
        {
            double nPaliwo = _paliwo + paliwo;
            if (nPaliwo <= _zbiornik)
            {
                _paliwo = nPaliwo;
            } else
            {
                throw new Exception("Zbiornik Exceeded :D");
            }
            return _paliwo;
        }
    }
}
