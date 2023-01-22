using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Lab5
{
    internal class Program
    {
        static void Main(string[] args)
        {
            Student student = new Student("Henry", "", "112233");
            student.Nazwisko = "Lee";
            student.Srednia = 2.0;
            student.Adres = "Nibylandia";
            PrzywitajSie(student);
            Console.WriteLine($"Student: {student.Imie} {student.Nazwisko}");
            Console.WriteLine($"Student: {student.NrIndeksu} : {student.Srednia}");
            Console.WriteLine($"Student: {student.Adres}");
            Console.WriteLine(student.WyswietlDane());

            Wykladowca wykladowca = new Wykladowca("", "Bon Jovi", "z1234");
            wykladowca.Imie = "John";
            wykladowca.StawkaGodzinowa = 25.00;
            wykladowca.Adres = "New York";
            PrzywitajSie(wykladowca);
            Console.WriteLine($"Wykladowca: {wykladowca.Imie} {wykladowca.Nazwisko}");
            Console.WriteLine($"Wykladowca: {wykladowca.NumerUmowy} : {wykladowca.StawkaGodzinowa}");
            Console.WriteLine($"Wykladowca: {wykladowca.Adres}");
            Console.WriteLine(wykladowca.WyswietlDane());
            Console.WriteLine($"Wykladowca zarobil: {wykladowca.Wyplata(50)}");

            Osoba studentDwa = new Student("Jack", "Sparrow", "Z303");
            PrzywitajSie(studentDwa);
            Console.WriteLine(studentDwa.WyswietlDane());
            Osoba wykladowcaDwa = new Wykladowca("Pirat", "Dwa", "102/2331");
            PrzywitajSie(wykladowcaDwa);
            Console.WriteLine(wykladowcaDwa.WyswietlDane());
        }

        static void PrzywitajSie(Osoba osoba)
        {
            Console.WriteLine($"Czesc {osoba.Imie} {osoba.Nazwisko}");
        }
    }
}
