using GamePlatformUI.Models;
using GamePlatformUI.Services;
using Microsoft.EntityFrameworkCore;

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
            return _db.Games.Include(g => g.GamePlayers).ThenInclude(gp => gp.Player);
        }
        
        public Game? GetGame(Int64 gameId)
        {
            return _db.Games.Include(g => g.GamePlayers).ThenInclude(gp => gp.Player).FirstOrDefault(g => g.Id == gameId);
        }

        public Game AddGame(Game game, string hostId)
        {
            using var transaction = _db.Database.BeginTransaction();
            try
            {
                _db.Games.Add(game);
                _db.SaveChanges();

                var gamePlayer = new GamePlayer
                {
                    GameId = game.Id,
                    PlayerId = hostId,
                    IsHost = true,
                };
                
                _db.GamePlayers.Add(gamePlayer);
                _db.SaveChanges();

                transaction.Commit();
            }
            catch (Exception)
            {
                transaction.Rollback();
                throw;
            }

            return game;
        }

        public void UpdateGame(Game game)
        {
            _db.Update(game);
            _db.SaveChanges();
        }

        public void DeleteGame(Int64 gameId)
        {
            var game = _db.Games.Find(gameId);

            if (game != null)
            {
                using var transaction = _db.Database.BeginTransaction();
                try
                {
                    _db.GamePlayers.RemoveRange(_db.GamePlayers.Where(gp => gp.GameId == gameId));
                    _db.SaveChanges();

                    _db.Games.Remove(game);
                    _db.SaveChanges();

                    transaction.Commit();
                }
                catch (Exception)
                {
                    transaction.Rollback();
                    throw;
                }
            }
            
        }
    }
}
