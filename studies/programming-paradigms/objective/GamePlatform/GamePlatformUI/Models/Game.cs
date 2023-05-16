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

        [DataType(DataType.DateTime)]
        public DateTime CreatedAt { get; set; }

        [DataType(DataType.DateTime)]
        public DateTime UpdatedAt { get; set; }
    }
}
