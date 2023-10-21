using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

namespace Dziekanat
{
    internal class StudentRepozytorium
    {
        private string folder;

        public StudentRepozytorium()
        {
            folder = Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData) + @"\Dziekanat";
            mozeStworzFolder();
        }

        public List<Student> WczytajStudentow()
        {
            List<string[]> daneStudentow = new List<string[]>();

            foreach (string plik in Directory.EnumerateFiles(folder, "*.txt"))
            {
                string[] student = File.ReadAllLines(plik);
                daneStudentow.Add(student);
            }

            int liczbaStudentow = daneStudentow.Count;
            List<Student> studenci = new List<Student>(liczbaStudentow);
            for(int i = 0; i < liczbaStudentow; i++)
            {
                string[] student = daneStudentow[i];
                Student nowyStudent = new Student(student[0], student[1], student[2], student[3]);
                studenci.Add(nowyStudent);
            }

            return studenci;
        }

        /// <summary>
        /// TODO: override previous data in name/indeks changed
        /// </summary>
        /// <param name="s"></param>
        public void ZapiszStudenta(Student s)
        {
            string[] dane = new string[] { s.Imie, s.Nazwisko, s.Grupa, s.Indeks };
            File.WriteAllLines($@"{folder}\{s.Nazwisko}_{s.Indeks}.txt", dane);
        }


        private void mozeStworzFolder()
        {
            if (!Directory.Exists(folder))
            {
                Directory.CreateDirectory(folder);
            }
        }
    }
}
