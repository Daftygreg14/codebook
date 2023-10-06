using System.ComponentModel.DataAnnotations.Schema;
using System.ComponentModel.DataAnnotations;
using GamePlatformUI.Areas.Identity.Data;

namespace GamePlatformUI.Models
{
    public class GamePlayer
    {
        [Key]
        public Int64 Id { get; set; }

        [ForeignKey("Game")]
        public Int64 GameId { get; set; }
        public Game Game { get; set; }

        public bool IsHost { get; set; }

        [ForeignKey("AspNetUsers")]
        public string PlayerId { get; set; }
        public User Player { get; set; }

        [DataType(DataType.DateTime)]
        public DateTime CreatedAt { get; set; }

        [DataType(DataType.DateTime)]
        public DateTime UpdatedAt { get; set; }
    }
}
