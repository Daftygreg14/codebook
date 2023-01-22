using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TicTacToeConsole
{
    internal class Program
    {
        static void Main(string[] args)
        {
            PlayGame();
        }

        static void PlayGame()
        {
            Console.WriteLine("Zagramy? Y/N");
            var key = Console.ReadKey();
            switch (key.Key)
            {
                case ConsoleKey.Y:
                    Console.Clear();
                    ConsoleGame game = new ConsoleGame();
                    game.Play();
                    break;
                case ConsoleKey.N:
                    Console.Clear();
                    Console.WriteLine("Nie to nie... bye!");
                    break;
                default:
                    Console.Clear();
                    PlayGame();
                    break;
            }

        }
    }
}
