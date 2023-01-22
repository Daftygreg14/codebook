using Lab6;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;

namespace Lab6
{
    class Piekarnik
    {
        private bool _wlaczony, _lampka;
        protected bool Lampka { get { return _lampka; } }
        protected double _czasPieczenia;
        protected double _temperatura, _temperaturaMax, _temperaturaUstawiona;
        protected double _mocChlodzenia = 0.01, _mocGrzania = 0.2;

        public Piekarnik() : this(220) { }
        public Piekarnik(double tempMax)
        {
            _temperatura = 20;
            _temperaturaMax = tempMax;
        }

        public void Uruchom(int temp, int czas)
        {
            _wlaczony = true;
            _czasPieczenia = czas * 1000;
            _temperaturaUstawiona = temp;   
            while(_wlaczony)
            {
                int czasInterval = 100;
                Wyswietl();

                Thread.Sleep(czasInterval);
                _czasPieczenia = _czasPieczenia - czasInterval;
                if(TemperaturaWymagan())
                {
                    _lampka = true;
                } else
                {
                    _lampka = false;
                    if (_temperatura < _temperaturaMax) { Grzanie(); }
                }

                _wlaczony = !OsiagnietoCzas();
                Chlodzenie();

                if(Console.KeyAvailable)
                {
                    Interakcja();
                }
            }
        }

        protected virtual void Grzanie()
        {
            _temperatura += _mocGrzania;
        }
        protected virtual void Chlodzenie()
        {
            if (_temperatura > 20) { _temperatura -= _mocChlodzenia; };
        }

        protected virtual bool OsiagnietoCzas()
        {
            return _czasPieczenia <= 0;
        }

        protected virtual void Interakcja()
        {
            var key = Console.ReadKey();
            if (key.Key == ConsoleKey.Escape) { _wlaczony = false; };
        }
        protected virtual bool TemperaturaWymagan()
        {
            return _temperatura >= _temperaturaUstawiona;
        }

        protected virtual void Wyswietl()
        {
            Console.SetCursorPosition(0, 1);
            Console.ForegroundColor = ConsoleColor.White;
            Console.WriteLine($"Temperatura: {_temperatura}");
        }

    }
}

class Amica:Piekarnik
{
    protected override void Wyswietl()
    {
        base.Wyswietl();
        var color = Lampka ? ConsoleColor.Green : ConsoleColor.Red;
        Console.ForegroundColor = color;
        Console.WriteLine($"Lampka", color);
    }
}

class Bosh:Piekarnik
{
    public Bosh() : base() {
        _mocGrzania = 0.5;
    }

    protected override void Wyswietl()
    {
        base.Wyswietl();
        Console.WriteLine($"Czas Pieczenia: {_czasPieczenia / 1000}");
        var color = Lampka ? ConsoleColor.Green : ConsoleColor.Red;
        Console.ForegroundColor = color;
        Console.WriteLine($"Lampka", color);
    }
}