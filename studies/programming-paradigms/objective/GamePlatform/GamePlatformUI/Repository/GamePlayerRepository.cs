using GamePlatformUI.Services;
using GamePlatformUI.Models;

namespace GamePlatformUI.Repository
{
    public class GamePlayerRepository : IGamePlayerRepository
    {
        private readonly ApplicationDbContext _db;
        public GamePlayerRepository(ApplicationDbContext db)
        {
            _db = db;
        }

        public GamePlayer AddGamePlayer(GamePlayer gamePlayer)
        {
            _db.GamePlayers.Add(gamePlayer);
            _db.SaveChanges();
            return gamePlayer;
        }

        public void DeleteGamePlayer(Int64 gamePlayerId)
        {
            var gamePlayer = _db.GamePlayers.Find(gamePlayerId);
            if (gamePlayer != null)
            {
                _db.GamePlayers.Remove(gamePlayer);
                _db.SaveChanges();
            }
        }
    }
}
