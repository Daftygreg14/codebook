using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TicTacToeCore;

namespace TicTacToeConsole
{
    internal class ConsoleRegistrationUI : ConsoleAbstractUI
    {
        public ConsoleRegistrationUI(Game game) : base(game) { }

        public override void RenderTitle()
        {
            Console.WriteLine($"Gra {_game.Uuid}. Rejestracja graczy");
        }
        public void PlayerOneRegistration()
        {
            HumanPlayer player = registerHuman();
            _game.RegisterPlayerOne(player);
        }

        public void PlayerTwoRegistration()
        {
            Console.WriteLine("Kto bedzie twoim przeciwnikiem? [C]zlowiek/[K]omputer");
            ConsoleKeyInfo key = Console.ReadKey();

            switch (key.Key)
            {
                case ConsoleKey.C:
                    registerPlayerTwo();
                    break;
                case ConsoleKey.K:
                    Console.WriteLine("\nPrzkro mi, ale developer jeszcze nie umie a AI");
                    Console.WriteLine("\nCzy chcesz zagrac z czlowiekiem?: Y/N");
                    key = Console.ReadKey();
                    if (key.Key == ConsoleKey.Y)
                    {
                        registerPlayerTwo();
                    }
                    else
                    {
                        _game.FinishGame();
                    }
                    break;
                default:
                    break;
            }
        }

        private HumanPlayer registerHuman()
        {
            Console.WriteLine("\nJak sie nazywasz: ");
            string playerName = getPlayerName();
            Console.WriteLine("Co chcesz cos powiedziec jak wygrasz?");
            string whenWon = Console.ReadLine();
            Console.WriteLine("A co gdy przegrasz?");
            string whenLose = Console.ReadLine();

            return new HumanPlayer(playerName, whenWon, whenLose);
        }

        private void registerPlayerTwo()
        {
            HumanPlayer player = registerHuman();
            _game.RegisterPlayerTwo(player);
        }

        private string getPlayerName()
        {
            string name = Console.ReadLine();
            while (name == string.Empty)
            {
                Console.WriteLine("Prosze podaj swoje imie...");
                name = Console.ReadLine();
            }

            return name;
        }
    }
}
