using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Lab5
{
    public class Wykladowca : Osoba
    {
        private string _numerUmowy;
        private double _stawkaGodzinowa;
        public Wykladowca(string imie, string nazwisko, string numerUmowy) : base(imie, nazwisko)
        {
            NumerUmowy = numerUmowy;
        }

        public string NumerUmowy
        {
            get { return _numerUmowy; }
            set { _numerUmowy = value; }
        }

        public double StawkaGodzinowa
        {
            get { return _stawkaGodzinowa; }
            set
            {
                _stawkaGodzinowa = value;
            }
        }

        override public string WyswietlDane()
        {
            return $"Student: {Imie} {Nazwisko}\nUmowa: {NumerUmowy} Stawka: {StawkaGodzinowa}\nAdres: {Adres}";
        }

        public double Wyplata(int liczbaGodzin)
        {
            return liczbaGodzin * StawkaGodzinowa;
        }
    }
}
