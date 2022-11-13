using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace SwiatPojazdow
{
    internal class Samochod
    {
        private double _predkosc, _predkoscMax, _zbiornik, _paliwo;
        /// <summary>
        /// Konstruktor samochodu ustawiajacy predkosc max 180
        /// </summary>
        public Samochod()
        {
            _predkosc = 0;
            _predkoscMax = 180;
            _zbiornik = 30;
            _paliwo = 5;
        }
        /// <summary>
        /// Konstrukro samochodu ustawiajacy predkosc max do 330
        /// </summary>
        /// <param name="vMax"></param>
        public Samochod(double vMax):this()
        {
            if (vMax < 330)
            {
                _predkoscMax = vMax;
            }
            else
            {
                _predkoscMax = 330;
            }
        }

        public double Predkosc
        {
            get { return _predkosc; }
            set
            {
                if (value <= _predkoscMax & value > 0)
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

        public double Paliwo
        {
            get { return _paliwo; }
        }

        public double Tankuj(double paliwo)
        {
            double nPaliwo = _paliwo + paliwo;
            if (nPaliwo <= _zbiornik)
            {
                _paliwo = nPaliwo;
            }
            return _paliwo;
        }
    }
}
