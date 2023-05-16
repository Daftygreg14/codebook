using GamePlatformUI.Models;

namespace GamePlatformUI.Services
{
    public interface IGameRepository
    {
        IEnumerable<Game> GetGames();
        Game GetGame(Int64 gameId);
        Game AddGame(Game game);
        void DeleteGame(Int64 type);
    }
}
