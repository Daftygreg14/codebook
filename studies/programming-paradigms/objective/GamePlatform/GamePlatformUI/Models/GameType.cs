using System.ComponentModel.DataAnnotations;

namespace GamePlatformUI.Models
{
    public class GameType
    {
        [Key]
        public string Type { get; set; }

        public bool Available { get; set; }

        [DataType(DataType.DateTime)]
        public DateTime CreatedAt { get; set; }

        [DataType(DataType.DateTime)]
        public DateTime UpdatedAt { get; set; }
    }
}
