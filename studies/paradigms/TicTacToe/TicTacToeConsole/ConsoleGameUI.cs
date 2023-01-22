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

        public void RenderBoard()
        {
            for (int row = 0; row < _game.BoardSize(); row++)
            {
                for (int col = 0; col < _game.BoardSize(); col++)
                {
                    Player player = _game.GetFieldAssigment(row, col);
                    string filedValue = fieldValue(player);
                    ConsoleColor color = fieldColor(player);

                    Console.ForegroundColor = color;
                    Console.Write(filedValue);
                    Console.ForegroundColor = ConsoleColor.White;
                }
                Console.WriteLine();
            }
        }
        public void PlayTurn()
        {
            int colNum = getPosition("kolumne");
            int rowNum = getPosition("wiersz");
            if (_game.GetFieldAssigment(rowNum, colNum) == null)
            {
                _game.TakeShot(rowNum, colNum);
            }
            else
            {
                Console.WriteLine("Przykro mi. Pole jest juz zajete");
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

        private string fieldValue(Player player)
        {
            if (player == null)
            {
                return ".";
            }
            else if (player == _game.PlayerOne)
            {
                return "x";
            }
            else
            {
                return "o";
            }
        }

        private ConsoleColor fieldColor(Player player) {
            if(player == null)
            {
                return ConsoleColor.White;
            } 
            else if (player == _game.PlayerOne)
            {
                return ConsoleColor.Green;
            } 
            else
            {
                return ConsoleColor.Blue;
            }
        }
    }
}
