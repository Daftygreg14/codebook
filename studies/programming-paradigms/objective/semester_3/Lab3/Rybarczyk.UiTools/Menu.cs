using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Rybarczyk.UiTools
{
    public class Menu
    {
        private string[] elementy;
        /// <summary>
        /// Konfiguruje elementy menu. Przyjmuje tylko tablice do 20 elementów
        /// </summary>
        /// <param name="elementyMenu">Tablica elementów</param>
        public void Konfiguruj(string[] elementyMenu)
        {
            if (elementyMenu.Length <= 20)
            {
                elementy = elementyMenu;

            }
            else
            {
                elementy = new string[0];
            }
        }
        public int Wyswietl()
        {
            int wybrany = 0;
            if (elementy != null)
            {
                ConsoleKeyInfo keyInfo;
                Console.BackgroundColor = ConsoleColor.DarkBlue;
                do
                {
                    Console.SetCursorPosition(0, 0);
                    for (int i = 0; i < elementy.Length; i++)
                    {
                        if (wybrany == i)
                        {
                            Console.BackgroundColor = ConsoleColor.Blue;
                        }
                        else
                        {
                            Console.BackgroundColor = ConsoleColor.DarkBlue;
                        }
                        Console.WriteLine(elementy[i].PadRight(20));
                    }

                    keyInfo = Console.ReadKey();

                    if (keyInfo.Key == ConsoleKey.UpArrow)
                    {
                        wybrany--;
                    }
                    else if (keyInfo.Key == ConsoleKey.DownArrow)
                    {
                        wybrany++;
                    }
                } while (keyInfo.Key != ConsoleKey.Enter && keyInfo.Key != ConsoleKey.Escape);
            }

            Console.ResetColor();
            return wybrany;
        }
    }

}
