using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace SwiatPojazdow
{
    internal class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("Starting Program");
            Samochod ferrari = new Samochod();

            // By default v = 0
            Console.WriteLine($"Bazowa: #{ferrari.Predkosc}");
            // V can be increased to 100
            ferrari.Predkosc = 100;
            Console.WriteLine($"Dane: 100. Predkosc: #{ferrari.Predkosc}");
            Console.WriteLine($"Dane: 100. Predkosc km/h: #{ferrari.PredkoscKmH}");
            // V can not be bigger than 180
            ferrari.Predkosc = 240;
            Console.WriteLine($"Dane 200. Predkosc: #{ferrari.Predkosc}");
            // V can not be smaller than 0
            ferrari.Predkosc = -100;
            Console.WriteLine($"Dane -100. Predkosc: #{ferrari.Predkosc}");

            Console.WriteLine("Press any key to continue");
            Console.ReadLine();

            int porcheVMax = 280;
            Samochod porche = new Samochod(porcheVMax);
            // By default v = 0
            Console.WriteLine($"Bazowa: #{porche.Predkosc}");
            // V can be increased to 220
            porche.Predkosc = 220;
            Console.WriteLine($"Dane 220. Predkosc: #{porche.Predkosc}");
            // V can not be bigger than 280
            porche.Predkosc = 300;
            Console.WriteLine($"Dane 240. Predkosc: #{porche.Predkosc}");
            // V can not be smaller than 0
            porche.Predkosc = -100;
            Console.WriteLine($"Dane -100. Predkosc: #{porche.Predkosc}");

            Console.WriteLine("Press any key to continue");
            Console.ReadLine();

            Samochod fiat = new Samochod(90, 100, 30, 5);
            Console.WriteLine($"Paliwo poczatkowe: #{fiat.Paliwo}");
            fiat.Tankuj(10);
            Console.WriteLine($"Paliwo po tankowaniu: #{fiat.Paliwo}");
            try
            {
                fiat.Tankuj(10000);
            }
            catch (Exception)
            {
                Console.WriteLine("Nie udalo sie zatankowac.");
            }
            fiat.Przyspiesz(50);
            Console.WriteLine($"Predkosc po przyspieszeniu: #{fiat.Predkosc}");
            Console.WriteLine($"Paliwo po przyspieszeniu: #{fiat.Paliwo}");

            fiat.Hamuj(50);
            Console.WriteLine($"Predkosc po hamowaniu: #{fiat.Predkosc}");
            Console.WriteLine(fiat.Wyswietl());

            Rower gorski = new Rower();
            Console.WriteLine($"Gorski Dystans: #{gorski.Dystans}");
            gorski.Jedz(100);
            Console.WriteLine($"Gorski Dystans: #{gorski.Dystans}");
            gorski.Przyszpiesz(40);
            Console.WriteLine($"Gorski Predkosc: #{gorski.Predkosc}");
            Console.WriteLine($"Gorski Dystans: #{gorski.Dystans}");
            gorski.Hamuj(-40);
            Console.WriteLine($"Gorski Predkosc: #{gorski.Predkosc}");
            Console.WriteLine($"Gorski Dystans: #{gorski.Dystans}");

            Rower dzieciecy = new Rower(3);
            Console.WriteLine($"Dzieciecy Dystans: #{dzieciecy.Dystans}");
            dzieciecy.Jedz(100);
            Console.WriteLine($"Dzieciecy Dystans: #{dzieciecy.Dystans}");
            dzieciecy.Przyszpiesz(40);
            Console.WriteLine($"Dzieciecy Predkosc: #{dzieciecy.Predkosc}");
            Console.WriteLine($"Dzieciecy Dystans: #{dzieciecy.Dystans}");
            dzieciecy.Hamuj(-10);
            Console.WriteLine($"Dzieciecy Predkosc: #{dzieciecy.Predkosc}");
            Console.WriteLine($"Dzieciecy Dystans: #{dzieciecy.Dystans}");
            dzieciecy.Hamuj(-100);
        }
    }
}
