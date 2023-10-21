using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Lab5
{
    public abstract class Osoba
    {
        private string _imie, _adres, _nazwisko;
        public Osoba(string imie, string nazwisko)
        {
            Imie = imie;
            Nazwisko = nazwisko;
        }

        public string Imie
        {
            get { return _imie; }
            set { _imie = value; }
        }
        public string Nazwisko
        {
            get { return _nazwisko; }
            set { _nazwisko = value; }
        }
        public string Adres
        {
            get { return _adres; }
            set { _adres = value; }
        }

        public abstract string WyswietlDane();
    }
}
