using GamePlatformUI.Areas.Games;
using GamePlatformUI.Areas.Identity.Data;
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

        public void StoreMatch(Match match)
        {
            GameMatchJson = match.ToJsonString();
            GameState = match.State();
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
