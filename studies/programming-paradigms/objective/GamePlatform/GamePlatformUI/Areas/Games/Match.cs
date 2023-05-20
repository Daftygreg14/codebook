using GamePlatformUI.Areas.Games.TicTacToe;
using GamePlatformUI.Models;

namespace GamePlatformUI.Areas.Games
{
    public abstract class Match
    {
        public Match() {}

        // Match API
        public abstract string State();
        public abstract void Start();
        public abstract void JoinGame(Game game, string userId);
        public abstract string CurrentPlayerId();

        public void StartGame(Game game, string userId)
        {
            var gamePlayer = game.GamePlayers.FirstOrDefault(gp => gp.IsHost && gp.PlayerId == userId);
            if (gamePlayer != null)
            {
                this.Start();
            }
            
        }

        // Serialization API
        public abstract string ToJsonString();
        public static Match FromJsonString(string jsonString)
        {
            throw new System.NotImplementedException();
        }
    }
}
