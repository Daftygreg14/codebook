using GamePlatformUI.Areas.Identity.Data;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;
using System.Numerics;

namespace GamePlatformUI.Models
{
    public class Game
    {
        [Key]
        public Int64 Id { get; set; }

        [ForeignKey("GameType")]
        public string GameType { get; set; }

        public string? GameState { get; set; }
        public string? GameBoard { get; set; }

        [DataType(DataType.DateTime)]
        public DateTime CreatedAt { get; set; }
        
        [DataType(DataType.DateTime)]
        public DateTime UpdatedAt { get; set; }
        
        public ICollection<GamePlayer>? GamePlayers { get; set; }
        
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
