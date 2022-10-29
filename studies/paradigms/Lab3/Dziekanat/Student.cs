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

        public void Zapisz()
        {
            string folder = Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData) + @"\Dziekanat";
            if (!Directory.Exists(folder))
            {
                Directory.CreateDirectory(folder);
            }
            string[] dane = new string[] { Imie, Nazwisko, Grupa, Indeks };
            File.WriteAllLines($@"{folder}\{Nazwisko}_{Indeks}.txt", dane);
        }
    }
}
