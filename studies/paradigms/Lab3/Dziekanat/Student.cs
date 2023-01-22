using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Dziekanat
{
    internal class Student
    {
        public string Imie, Nazwisko, Grupa, Indeks;

        public Student(string imie = "", string nazwisko = "", string grupa = "", string indeks = "")
        {
            Imie = imie;
            Nazwisko = nazwisko;
            Grupa = grupa;   
            Indeks = indeks;
        }

        public string Wyswietl()
        {
            return $"{this.Imie} {this.Nazwisko} {this.Grupa} {this.Indeks}";
        }
    }
}
