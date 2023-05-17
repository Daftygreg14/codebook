using GamePlatformUI.Areas.TicTacToe;
using GamePlatformUI.Models;
using GamePlatformUI.Presenters;

namespace GamePlatformUI.Factories
{
    public class GamePresenterFactory
    {
        public GamePresenter CreateGamePresenter(Game game, string? userId)
        {
            switch (game.GameType)
            {
                case "TicTacToe":
                    TicTacToePresenter match = new TicTacToePresenter(game, userId);
                    return match;
                default:
                    throw new ArgumentException($"Invalid game type: {game.GameType}");
            }
        }
    }
}
