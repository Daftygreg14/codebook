using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Xml.Linq;
using TicTacToeCore;

namespace TicTacToeConsole
{
    internal class ConsoleGameUI : ConsoleAbstractUI
    {
        public ConsoleGameUI(Game game) : base(game) { }
        public override void RenderTitle()
        {
            switch (_game.State)
            {
                case Game.GameStateEnum.Init:
                    Console.WriteLine($"Gra: {_game.Uuid}");
                    break;
                case Game.GameStateEnum.PlayerOneRegistrated:
                    Console.WriteLine($"Gra: {_game.Uuid}. Host: {_game.PlayerOne.Name} (x)");
                    break;
                case Game.GameStateEnum.PlayerTwoRegistrated:
                    Console.WriteLine($"Gra: {_game.Uuid}. Host: {_game.PlayerOne.Name} (x) Przeciwnik: {_game.PlayerTwo.Name} (o)");
                    break;
                case Game.GameStateEnum.Finished:
                    Console.WriteLine($"Gra: {_game.Uuid} skonczona");
                    break;
                default:
                    Console.WriteLine($"Gra: {_game.Uuid}. Host: {_game.PlayerOne.Name} (x) Przeciwnik: {_game.PlayerTwo.Name} (o)");
                    Console.WriteLine($"Tura: {_game.GetCurrentPlayer().Name}");
                    break;
            }
        }

        public void SetupBoard()
        {
            Console.WriteLine("Aktualnie obslugujemy tylko 3x3");
            Console.WriteLine("Zgadzasz sie? Y/N");
            var key = Console.ReadKey();
            if (key.Key == ConsoleKey.Y)
            {
                _game.InitializeBoard(3);
            }
            else
            {
                _game.FinishGame();
            }
        }

        public void RenderBoard(Game game)
        {
            for (int row = 0; row < game.BoardSize(); row++)
            {
                for (int col = 0; col < game.BoardSize(); col++)
                {
                    Console.Write(displayField(game, row, col));
                }
                Console.WriteLine();
            }
        }
        public void PlayTurn()
        {
            int colNum = getPosition("kolumne");
            int rowNum = getPosition("wiersz");
            if (_game.FieldValue(rowNum, colNum) == null)
            {
                _game.TakeShot(rowNum, colNum);
            }
            else
            {
                Console.WriteLine("Przykor mi. Pole jest juz zajete");
                PlayTurn();
            }
        }
        public void RenderResult()
        {
            Player winner = _game.GetWinner();
            Player loser = _game.GetLoser();

            if (winner == null)
            {
                Console.WriteLine("Mamy Remis!");
                Console.WriteLine($"[{winner.Name}]: {winner.TextWhenLose()}");
                Console.WriteLine($"[{loser.Name}]: {loser.TextWhenLose()}");
            }
            else
            {
                Console.WriteLine($"Wygrywa {winner.Name}");
                Console.WriteLine($"[{winner.Name}]: {winner.TextWhenWon()}");
                Console.WriteLine($"[{loser.Name}]: {loser.TextWhenLose()}");
            }
        }

        private string displayField(Game game, int row, int col)
        {
            if (game.FieldValue(row, col) == null)
            {
                return ".";
            }
            else if (game.FieldValue(row, col) == game.PlayerOne)
            {
                return "x";
            }
            else
            {
                return "o";
            }
        }

        private int getPosition(string colOrRow)
        {
            Console.WriteLine($"Podaj {colOrRow}: ");
            int posNum = int.Parse(Console.ReadLine());
            if (posNum < 1 || posNum > _game.BoardSize())
            {
                Console.WriteLine("Przykro mi, wartosc poza zakresem");
                return getPosition(colOrRow);
            }
            else
            {
                return posNum - 1;
            }
        }
    }
}
