using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection.Emit;
using System.Text;
using System.Threading.Tasks;
using Rybarczyk.UiTools;

namespace Dziekanat
{
    internal class StudentManager
    {
        private List<Student> studenci;
        private StudentRepozytorium studentRepo;

        public StudentManager(StudentRepozytorium repo)
        {
            studentRepo = repo;
            studenci = studentRepo.WczytajStudentow();
        }


        public int ListaStudentow()
        {
            Console.Clear();
            Menu lista = new Menu();
            string[] dane = new string[studenci.Count()];
            for(int i = 0; i < dane.Length; i++)
            {
                Student s = studenci[i];
                dane[i] = $"{s.Imie} {s.Nazwisko} {s.Grupa} {s.Indeks}";
            }
            lista.Konfiguruj(dane);
            return lista.Wyswietl();
        }

        public void EdytujStudenta()
        {
            int nrStudenta = ListaStudentow();
            Student student = studenci[nrStudenta];
            Console.Clear();
            Console.Write($"Imie     ({student.Imie})           :"); student.Imie = zmianaJakNiePuste(student.Imie);
            Console.Write($"Nazwisko ({student.Nazwisko})       :"); student.Nazwisko = zmianaJakNiePuste(student.Nazwisko);
            Console.Write($"Grupa    ({student.Grupa})          :"); student.Grupa = zmianaJakNiePuste(student.Grupa);
            Console.Write($"Indeks   ({student.Indeks})         :"); student.Indeks = zmianaJakNiePuste(student.Indeks);
        }

        private string zmianaJakNiePuste(string staraWartosc)
        {
            string nowaWartosc = Console.ReadLine();
            if (nowaWartosc != "")
            {
                return nowaWartosc;
            } else
            {
                return staraWartosc;
            }
        }

        public void DodajStudenta()
        {
            Student nowyStudent = new Student();
            Console.Clear();
            Console.WriteLine("********** Dodawanie Studenta *********");
            Console.Write("Imie            :"); nowyStudent.Imie = Console.ReadLine();
            Console.Write("Nazwisko        :"); nowyStudent.Nazwisko = Console.ReadLine();
            Console.Write("Grupa           :"); nowyStudent.Grupa = Console.ReadLine();
            nowyStudent.Indeks = NowyNumerIndeksu();
            studenci.Add(nowyStudent);
        }

        private string NowyNumerIndeksu()
        {
            int maxIndex = studenci.Max(s => Int32.Parse(s.Indeks));
            int nowyNumer = maxIndex + 1;
            return nowyNumer.ToString();
        }

        internal void ZapiszZmiany()
        {
            studenci.ForEach(s => studentRepo.ZapiszStudenta(s));
        }
    }
}
