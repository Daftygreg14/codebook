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
            Console.Clear();
            Console.WriteLine("********** Edycja Studenta *********");
            Console.WriteLine("Podaj Numer Indeksu     :"); string nrIndeks = Console.ReadLine();
            Student student = studenci.Find(s => s.Indeks == nrIndeks);
            if (student != null)
            {
                Console.WriteLine(student.Wyswietl());
                Console.WriteLine("Nowe Imie            :"); student.Imie = Console.ReadLine();
                Console.WriteLine("Nowe Nazwisko        :"); student.Nazwisko = Console.ReadLine();
                Console.WriteLine("Nowa Grupa           :"); student.Grupa = Console.ReadLine();
            } else
            {
                Console.WriteLine("Nie ma takiego studenta");
                Console.WriteLine("Wcisnij Enter aby szukac ponownie");
                if (Console.ReadKey().Key == ConsoleKey.Enter)
                {
                    EdytujStudenta();
                }
            }
        }

        public void DodajStudenta()
        {
            Student nowyStudent = new Student();
            Console.Clear();
            Console.WriteLine("********** Dodawanie Studenta *********");
            Console.WriteLine("Imie            :"); nowyStudent.Imie = Console.ReadLine();
            Console.WriteLine("Nazwisko        :"); nowyStudent.Nazwisko = Console.ReadLine();
            Console.WriteLine("Grupa           :"); nowyStudent.Grupa = Console.ReadLine();
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
