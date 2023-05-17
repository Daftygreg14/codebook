using GamePlatformUI.Abstracts;
using GamePlatformUI.Areas.BattelShips;
using GamePlatformUI.Areas.TicTacToe;

namespace GamePlatformUI.Factories
{
    public class MatchFactory
    {
        public Match CreateGame(string type, Int64 gameId)
        {
            switch (type)
            {
                case "TicTacToe":
                    return new TicTacToeMatch(gameId);
                case "Battleships":
                    return new BattelShipsMatch(gameId);
                default:
                    throw new ArgumentException($"Invalid game type: {type}");
            }
        }
    }
}
