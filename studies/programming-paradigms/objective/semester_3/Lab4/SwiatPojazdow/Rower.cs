using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace SwiatPojazdow
{
    public class Rower
    {
        private int _kola;
        private double _predkosc, _predkoscMax, _dystans;

        public Rower()
        {
            _kola = 2;
            _predkosc = 0;
            _dystans = 0;
            _predkoscMax = 40;
        }

        public Rower(int kola):this()
        {
            this._kola = kola > 0 ? kola : throw new Exception("Zepsutych nie robimy");
            this._predkoscMax = kola > 2 ? 30 : 40;
        }

        ~Rower()
        {
            Console.WriteLine("A mogles oddac;");
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
                else if (value < 0)
                {
                    _predkosc = 0;
                }
                else
                {
                    _predkosc = _predkoscMax;
                }
            }
        }

        public double Dystans
        {
            get { return _dystans;  }
        }

        public void Jedz(double odleglosc)
        {
            _dystans = _dystans + odleglosc;
        }

        public void Przyszpiesz(double przyspieszenie)
        {
            if (przyspieszenie > 0 && Predkosc + przyspieszenie <= _predkoscMax)
            {
                Predkosc = Predkosc + przyspieszenie;
                _dystans = _dystans + przyspieszenie * 0.2;
            }
        }

        public void Hamuj(double spowolnienie)
        {
            if (spowolnienie < 0 && Predkosc + spowolnienie >= 0)
            {
                Predkosc = Predkosc + spowolnienie;
            } else if (spowolnienie < 0)
            {
                throw new Exception("Chcesz sie zabic?");
            }
        }
    }
}
