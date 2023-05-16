using Microsoft.EntityFrameworkCore;
using GamePlatformUI.Models;
using GamePlatformUI.Services;

namespace GamePlatformUI.Repository
{
    public class GameTypeRepository : IGameTypeRepository
    {
        private readonly ApplicationDbContext _db;
        public GameTypeRepository(ApplicationDbContext db)
        {
            _db = db;
        }

        public IEnumerable<GameType> GetGameTypes()
        {
            return _db.GameTypes;
        }

        public GameType? GetGameType(string type)
        {
            return _db.GameTypes.Find(type);
        }

        public GameType AddGameType(GameType gameType)
        {
            _db.GameTypes.Add(gameType);
            _db.SaveChanges();
            return gameType;
        }

        public GameType UpdateGameType(GameType gameType)
        {
            var gameTypeChanges = _db.GameTypes.Attach(gameType);
            gameTypeChanges.State = EntityState.Modified;
            _db.SaveChanges();
            return gameType;
        }

        public void DeleteGameType(string type)
        {
            GameType gameType = _db.GameTypes.Find(type);
            if (gameType != null)
            {
                _db.GameTypes.Remove(gameType);
                _db.SaveChanges();
            }
        }
    }
}
