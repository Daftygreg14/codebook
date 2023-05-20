using GamePlatformUI.Areas.Games.TicTacToe;
using GamePlatformUI.Areas.Games;

using GamePlatformUI.Models;

namespace GamePlatformUI.Factories
{
    public class MatchFactory
    {
        public Match CreateMatch(Game game, string hostId)
        {
            switch (game.GameType)
            {
                case "TicTacToe":
                    return CreateTicTacToeMatch(game, hostId);
                default:
                    throw new ArgumentException($"Invalid game type: {game.GameType}");
            }
        }
        
        public Match LoadMatch(Game game)
        {
            switch(game.GameType)
            {
                case "TicTacToe":
                    return TicTacToeMatch.FromJsonString(game.GameMatchJson);
                default:
                    throw new ArgumentException($"Invalid game type: {game.GameType}");
            }
        }

        private TicTacToeMatch CreateTicTacToeMatch(Game game, string hostId)
        {
            TicTacToeMatch match = new TicTacToeMatch();
            Player player = new Player(hostId);
            match.RegisterPlayerOne(player);
            return match;
        }
    }
}
