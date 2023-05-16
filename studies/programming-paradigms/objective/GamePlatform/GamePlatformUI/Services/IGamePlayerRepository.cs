using GamePlatformUI.Models;

namespace GamePlatformUI.Services
{
    public interface IGamePlayerRepository
    {
        GamePlayer AddGamePlayer(GamePlayer gamePlayer);
        void DeleteGamePlayer(Int64 id);
    }
}
