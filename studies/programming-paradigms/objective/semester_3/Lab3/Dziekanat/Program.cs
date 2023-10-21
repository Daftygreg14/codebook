using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Rybarczyk.UiTools;

namespace Dziekanat
{
    internal class Program
    {
        static void Main(string[] args)
        {
            Menu menu = new Menu();
            StudentRepozytorium studentRepo = new StudentRepozytorium();
            StudentManager studentManager = new StudentManager(studentRepo);

            menu.Konfiguruj(new string[] { "Lista studentow", "Dodaj studenta", "Edytuj studenta", "Zapisz zmiany", "Zakoncz" });
            int zadanie;
            
            do
            {
                Console.Clear();
                zadanie = menu.Wyswietl();
                Console.WriteLine(zadanie);

                switch(zadanie)
                {
                    case 0:
                        studentManager.ListaStudentow();
                        break;
                    case 1:
                        studentManager.DodajStudenta();
                        break;
                    case 2:
                        studentManager.EdytujStudenta();
                        break;
                    case 3:
                        studentManager.ZapiszZmiany();
                        break;
                }

            } while ((zadanie != 4 && zadanie != -1));

        }
    }
}
