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

        public IEnumerable<GameType> GetAvailableGameTypes()
        {
            return _db.GameTypes.Where(gt => gt.Available);
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
    }
}
