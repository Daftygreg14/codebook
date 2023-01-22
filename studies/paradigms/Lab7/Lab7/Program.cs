using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Lab7
{
    internal class Program
    {
        static void Main(string[] args)
        {
            Dziecko dziecko = new Dziecko(ConsoleColor.Black, 10);
            dziecko.OpiszSwojeWlosy();
            // Przytnij Wlosy do 5 cm
            dziecko.PrzytnijWlosy(5);
            dziecko.OpiszSwojeWlosy();
            // Przedluz wlosy
            dziecko.PrzytnijWlosy(15);
            dziecko.OpiszSwojeWlosy();
            // Przefarbuj wlosy na zielony
            dziecko.PrzefarbujWlosy(ConsoleColor.Green);
            dziecko.OpiszSwojeWlosy();
            // Pisanie
            Kredka niebieskaKredka = new Kredka(ConsoleColor.Blue);
            Kredka zoltaKredka = new Kredka(ConsoleColor.Yellow);
            dziecko.Napisz("Ala ma kota", niebieskaKredka);
            Console.WriteLine();
            dziecko.Napisz("A kot ma Ale", zoltaKredka);
            Console.WriteLine();
            // Relacja
            dziecko.CzyMaszPrzyjaciela();
            Dziecko inneDziecko = new Dziecko(ConsoleColor.Red, 20);
            dziecko.ZaprzyjaznijSie(dziecko);
            dziecko.CzyMaszPrzyjaciela();
        }

    }
}
