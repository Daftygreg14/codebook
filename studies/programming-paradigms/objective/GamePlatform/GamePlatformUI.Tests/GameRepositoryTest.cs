using GamePlatformUI.Areas.Identity.Data;
using GamePlatformUI.Models;
using GamePlatformUI.Repository;
using Microsoft.EntityFrameworkCore;
using System.Security.AccessControl;

namespace GamePlatformUI.Tests;

public class GameRepositoryTest : IDisposable
{
    private readonly DbContextOptions<ApplicationDbContext> _options;
    private readonly string _dbName;

    public GameRepositoryTest()
    {
        var _dbName = Guid.NewGuid().ToString();
        _options = new DbContextOptionsBuilder<ApplicationDbContext>()
            .UseSqlServer($"Server=localhost;Database={_dbName};User=sa;Password=example_123;TrustServerCertificate=true;")
            .Options;

        Console.WriteLine(_options);

        using var context = new ApplicationDbContext(_options);
        context.Database.Migrate();
    }
    public void Dispose()
    {
        // Cleanup: delete the database after tests run
        using var context = new ApplicationDbContext(_options);
        context.Database.EnsureDeleted();
    }

    [Fact]
    public void AddGame_ShouldAddGameType()
    {
        // [GIVEN] Regsiter new game with game type
        using var context = new ApplicationDbContext(_options);
        var gameType = new GameType
        {
            Type = "TestType",
            CreatedAt = DateTime.UtcNow,
            UpdatedAt = DateTime.UtcNow
        };
        context.GameTypes.Add(gameType);
        context.SaveChanges();
        var user = new User
        {
            UserName = "TestUser",
            Email = "example@example.com",
            PasswordHash = "TestPassword"
        };
        context.Users.Add(user);
        context.SaveChanges();

        // [WHEN] Add new game type
        var game = new Game
        {
            GameType = gameType.Type,
            CreatedAt = DateTime.UtcNow,
            UpdatedAt = DateTime.UtcNow
        };

        var repo = new GameRepository(context);
        repo.AddGame(game, user.Id);
        
        // [THEN] Verify that the game type was added
        var resultGame = context.Games.Find(game.Id);
        Assert.NotNull(resultGame);
        Assert.Equal("TestType", resultGame.GameType);

        var resultGamePlayer = context.GamePlayers.Where(x => x.GameId == game.Id).First();
        Assert.NotNull(resultGamePlayer);
        Assert.True(resultGamePlayer.IsHost);
    }

    [Fact]
    public void GetGame_ShouldReturnGame()
    {
        // [GIVEN] Regsiter new game with game type
        using var context = new ApplicationDbContext(_options);
        var gameType = new GameType
        {
            Type = "TestType2",
            CreatedAt = DateTime.UtcNow,
            UpdatedAt = DateTime.UtcNow
        };
        context.GameTypes.Add(gameType);
        context.SaveChanges();


        // [WHEN] Add new game type
        var game = new Game
        {
            GameType = gameType.Type,
            CreatedAt = DateTime.UtcNow,
            UpdatedAt = DateTime.UtcNow
        };
        context.Games.Add(game);
        context.SaveChanges();

        // [THEN] Verify that the game type was added
        var repo = new GameRepository(context);
        var result = repo.GetGame(game.Id);
        Assert.NotNull(result);
        Assert.Equal("TestType2", result.GameType);
    }

    [Fact]
    public void DeleteGame_ShouldDeleteGame()
    {
        // [GIVEN] A new game type
        using var context = new ApplicationDbContext(_options);
        var gameType = new GameType
        {
            Type = "TestType3",
            CreatedAt = DateTime.UtcNow,
            UpdatedAt = DateTime.UtcNow
        };
        context.GameTypes.Add(gameType);
        context.SaveChanges();

        var game = new Game
        {
            GameType = gameType.Type,
            CreatedAt = DateTime.UtcNow,
            UpdatedAt = DateTime.UtcNow
        };
        context.Games.Add(game);
        context.SaveChanges();

        // [WHEN] Game Is Deleted
        var repo = new GameRepository(context);
        repo.DeleteGame(game.Id);

        // [THEN] Verify that the game type was added
        var result = context.Games.Find(game.Id);
        Assert.Null(result);
    }
}