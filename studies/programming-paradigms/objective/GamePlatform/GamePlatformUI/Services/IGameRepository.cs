using GamePlatformUI.Models;
using GamePlatformUI.Areas.Identity.Data;

namespace GamePlatformUI.Services
{
    public interface IGameRepository
    {
        IEnumerable<Game> GetGames();
        Game GetGame(Int64 gameId);
        Game AddGame(Game game, string hostId);
        void UpdateGame(Game game);
        void DeleteGame(Int64 type);
    }
}
