using Microsoft.EntityFrameworkCore;
using GamePlatformUI.Models;
using GamePlatformUI.Services;

namespace GamePlatformUI.Repository
{
    public class GameRepository : IGameRepository
    {
        private readonly ApplicationDbContext _db;
        public GameRepository(ApplicationDbContext db)
        {
            _db = db;
        }

        public IEnumerable<Game> GetGames()
        {
            return _db.Games;
        }
        
        public Game? GetGame(Int64 gameId)
        {
            return _db.Games.Find(gameId);
        }
        
        public Game AddGame(Game game)
        {
            _db.Games.Add(game);
            _db.SaveChanges();
            return game;
        }
        
        public void DeleteGame(Int64 gameId)
        {
            var game = _db.Games.Find(gameId);
            if (game != null)
            {
                _db.Games.Remove(game);
                _db.SaveChanges();
            }
        }
    }
}
