using GamePlatformUI.Models;
using GamePlatformUI.Repository;
using Microsoft.EntityFrameworkCore;

namespace GamePlatformUI.Tests;

public class GameTypeRepositoryTests : IDisposable
{
    private readonly DbContextOptions<ApplicationDbContext> _options;
    private readonly string _dbName;

    public GameTypeRepositoryTests()
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
    public void AddGameTypeAsync_ShouldAddGameType()
    {
        // [GIVEN] A new game type
        using var context = new ApplicationDbContext(_options);
        var gameType = new GameType
        {
            Type = "TestType",
            Available = true,
            CreatedAt = DateTime.UtcNow,
            UpdatedAt = DateTime.UtcNow
        };

        // [WHEN] Add new game type
        var repo = new GameTypeRepository(context);
        repo.AddGameType(gameType);

        // [THEN] Verify that the game type was added
        var result = context.GameTypes.Find("TestType");
        Assert.NotNull(result);
        Assert.Equal("TestType", result.Type);
        Assert.True(result.Available);
    }

    [Fact]
    public void GetGameTypeAsync_ShouldUpdateType()
    {
        // [GIVEN] A new game type
        using var context = new ApplicationDbContext(_options);
        var gameType = new GameType
        {
            Type = "TestType",
            Available = true,
            CreatedAt = DateTime.UtcNow,
            UpdatedAt = DateTime.UtcNow
        };
        context.GameTypes.Add(gameType);
        context.SaveChanges();

        // [WHEN] Get game type
        var repo = new GameTypeRepository(context);
        gameType.Available = false;
        var result = repo.UpdateGameType(gameType);

        // [THEN] Verify that the game type was added
        Assert.NotNull(result);
        Assert.Equal("TestType", result.Type);
        Assert.False(result.Available);
    }

    [Fact]
    public void GetGameTypeAsync_ShouldDeleteType()
    {
        // [GIVEN] A new game type
        using var context = new ApplicationDbContext(_options);
        var gameType = new GameType
        {
            Type = "TestType",
            Available = true,
            CreatedAt = DateTime.UtcNow,
            UpdatedAt = DateTime.UtcNow
        };
        context.GameTypes.Add(gameType);
        context.SaveChanges();

        // [WHEN] Get game type
        var repo = new GameTypeRepository(context);
        repo.DeleteGameType("TestType");

        // [THEN] Verify that the game type was added
        var result = context.GameTypes.Find("TestType");
        Assert.Null(result);
    }
}