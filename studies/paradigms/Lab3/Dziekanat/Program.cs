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
            menu.Konfiguruj(new string[] { "Lista studentow", "Dodaj studenta", "Edytuj studenta", "Zapisz zmiany", "Zakoncz" });
            int zadanie;
            do
            {
                zadanie = menu.Wyswietl();
                Console.WriteLine(zadanie);

                switch(zadanie)
                {
                    case 0:
                        break;
                    case 1:
                        break;
                    case 2:
                        break;
                    case 3:
                        break;
                }

            } while ((zadanie != 4 && zadanie != -1));

        }
    }
}
