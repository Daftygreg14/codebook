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

        public StudentManager()
        {
            studenci = new List<Student>(4);
        }

        private void LadujDane()
        {
            studenci.Add(new Student("Piotr", "Rybarczyk", "Z304", "10117"));
            studenci.Add(new Student("Adam", "Lepkowski", "Z303", "10116"));
            studenci.Add(new Student("Krzysztof", "Budzynski", "Z302", "10115"));
            studenci.Add(new Student("Tomasz", "Dudziec", "Z301", "10114"));
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
            return 0;
        }

        public void EdytujStudenta()
        {

        }

        public void DodajStudenta()
        {
            Student newStudent = new Student();
            Console.Clear();
            Console.WriteLine("********** Dodawanie Studenta *********");
            Console.WriteLine("Imie            :"); newStudent.Imie = Console.ReadLine();
            Console.WriteLine("Nazwisko        :"); newStudent.Nazwisko = Console.ReadLine();
            Console.WriteLine("Grupa           :"); newStudent.Grupa = Console.ReadLine();
            Console.WriteLine("Index           :"); newStudent.Indeks = Console.ReadLine();
        }

        internal void ZapiszZmiany()
        {
            throw new NotImplementedException();
        }
    }
}
