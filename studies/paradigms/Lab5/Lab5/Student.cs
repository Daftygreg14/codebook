using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Lab5
{
    public class Student : Osoba
    {
        private string _nrIndeksu;
        private double _srednia;

        public Student(string imie, string nazwisko, string numerIndeksu): base(imie, nazwisko)
        {
            NrIndeksu = numerIndeksu;
        }

        public string NrIndeksu
        {
            get { return _nrIndeksu; }
            set
            {
                _nrIndeksu = value;
            }
        }
        public double Srednia
        {
            get { return _srednia; }
            set
            {
                _srednia = value;
            }
        }

        override public string WyswietlDane()
        {
            return $"Student: {Imie} {Nazwisko}\nIndeks: {NrIndeksu} Srednia: {Srednia}\nAdres: {Adres}";
        }
    }
}
