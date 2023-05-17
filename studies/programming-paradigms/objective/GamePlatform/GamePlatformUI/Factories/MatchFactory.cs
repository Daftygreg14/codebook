using GamePlatformUI.Areas.TicTacToe;

namespace GamePlatformUI.Factories
{
    public class MatchFactory
    {
        public TicTacToeMatch CreateGame(string type, string hostId)
        {
            switch (type)
            {
                case "TicTacToe":
                    TicTacToeMatch match = CreateTicTacToeMatch(hostId);
                    return match;
                default:
                    throw new ArgumentException($"Invalid game type: {type}");
            }
        }

        private TicTacToeMatch CreateTicTacToeMatch(string hostId)
        {
            TicTacToeMatch match = new TicTacToeMatch();
            match.RegisterPlayerOne(new Player(hostId));
            match.InitializeBoard();
            return match;
        }
    }
}
