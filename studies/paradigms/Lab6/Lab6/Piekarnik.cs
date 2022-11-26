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

        public Piekarnik() : this(220) { }
        public Piekarnik(double tempMax)
        {
            _temperatura = 20;
            _temperaturaMax = tempMax;
        }

        public void Uruchom(int temp, int czas)
        {
            _wlaczony = true;
            _czasPieczenia = czas;
            _temperaturaUstawiona = temp;   
            while(_wlaczony)
            {
                Wyswietl();
                Thread.Sleep(100);
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
            _temperatura += 0.2;
        }
        protected virtual void Chlodzenie()
        {
            if (_temperatura > 20) { _temperatura -= 0.01; };
        }

        protected virtual bool OsiagnietoCzas()
        {
            return false;
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
            Console.SetCursorPosition(1, 1);
            Console.WriteLine($"{_temperatura}");
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