using GamePlatformUI.Models;

namespace GamePlatformUI.Presenters
{
    public abstract class GamePresenter
    {
        public Int64 id;
        public string type, state, hostName;
        public string createdAt, updatedAt;
        public GamePresenter(Game game)
        {            
            id = game.Id;
            type = game.GameType;
            state = getGameState(game);
            hostName = getHostName(game);
            createdAt = game.CreatedAt.ToShortDateString();
            updatedAt = game.UpdatedAt.ToShortDateString();
        }

        public abstract bool canStart();

        public abstract bool canJoin();

        public abstract bool canDelete();

        private string getGameState(Game game)
        {
            if (game.GameState != null)
            {
                return game.GameState;
            }
            return "unknown";
        }
        private string getHostName(Game game)
        {
            if (game.Host() != null)
            {
                return game.Host().DisplayName();
            }
            return string.Empty;
        }
        
    }
}
