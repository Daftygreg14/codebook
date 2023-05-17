using GamePlatformUI.Areas.Identity.Data;
using GamePlatformUI.Areas.TicTacToe;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace GamePlatformUI.Models
{
    public class Game
    {
        [Key]
        public Int64 Id { get; set; }

        [ForeignKey("GameType")]
        public string GameType { get; set; }

        public string? GameState { get; set; }
        public string? GameMatchJson { get; set; }

        [DataType(DataType.DateTime)]
        public DateTime CreatedAt { get; set; }
        
        [DataType(DataType.DateTime)]
        public DateTime UpdatedAt { get; set; }
        
        public ICollection<GamePlayer>? GamePlayers { get; set; }

        public void StartGame(string userId)
        {
            var gamePlayer = GamePlayers.FirstOrDefault(gp => gp.IsHost && gp.PlayerId == userId);
            if (gamePlayer != null )
            {
                var match = TicTacToeMatch.FromJsonString(this.GameMatchJson);
                match.Start();
                this.GameState = match.State.ToString();
                this.GameMatchJson = match.ToJsonString();
            }

        }

        public void JoinGame(string userId)
        {
            if (GamePlayers.FirstOrDefault(gp => gp.PlayerId == userId) == null)
            {
                var match = TicTacToeMatch.FromJsonString(this.GameMatchJson);
                match.RegisterPlayerTwo(new Player(userId));
                this.GameState = match.State.ToString();
                this.GameMatchJson = match.ToJsonString();
            }
        }
        
        // [TODO] Refactor to preload Player in single query
        public User? Host()
        {
            var gamePlayer = GamePlayers.Where(gp => gp.IsHost).First();
            if (gamePlayer != null)
            {
                return gamePlayer.Player;
            }
            return null;
        }
    }
}
