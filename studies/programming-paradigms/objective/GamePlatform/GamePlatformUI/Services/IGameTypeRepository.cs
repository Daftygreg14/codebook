using GamePlatformUI.Models;

namespace GamePlatformUI.Services
{
    public interface IGameTypeRepository
    {
        IEnumerable<GameType> GetGameTypes();
        GameType? GetGameType(string type);
        GameType AddGameType(GameType gameType);
        GameType UpdateGameType(GameType gameType);
        void DeleteGameType(string type);
    }
}
