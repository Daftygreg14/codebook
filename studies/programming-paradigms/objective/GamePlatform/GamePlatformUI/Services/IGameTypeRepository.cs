using GamePlatformUI.Models;

namespace GamePlatformUI.Services
{
    public interface IGameTypeRepository
    {
        IEnumerable<GameType> GetGameTypes();
        IEnumerable<GameType> GetAvailableGameTypes();

        GameType? GetGameType(string type);
        GameType AddGameType(GameType gameType);
        GameType UpdateGameType(GameType gameType);
    }
}
