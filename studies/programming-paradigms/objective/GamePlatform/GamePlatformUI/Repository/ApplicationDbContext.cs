using GamePlatformUI.Areas.Identity.Data;
using GamePlatformUI.Models;
using Microsoft.AspNetCore.Identity.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore;
using System.Reflection.Emit;
using System.Runtime.Intrinsics.Arm;

namespace GamePlatformUI.Repository;

public class ApplicationDbContext : IdentityDbContext<User>
{
    public ApplicationDbContext(DbContextOptions<ApplicationDbContext> options)
        : base(options)
    {
    }

    public DbSet<GameType> GameTypes { get; set; }
    public DbSet<Game> Games { get; set; }
    public DbSet<GamePlayer> GamePlayers { get; set; }

    protected override void OnModelCreating(ModelBuilder builder)
    {
        base.OnModelCreating(builder);

        // Default Properties
        builder.Entity<GameType>().Property(gt => gt.UpdatedAt).ValueGeneratedOnAddOrUpdate().HasDefaultValueSql("GETDATE()");
        builder.Entity<Game>().Property(g => g.CreatedAt).ValueGeneratedOnAdd().HasDefaultValueSql("GETDATE()");
        builder.Entity<Game>().Property(g => g.UpdatedAt).ValueGeneratedOnAddOrUpdate().HasDefaultValueSql("GETDATE()");

        // Relationships
        builder.Entity<Game>()
           .HasMany(g => g.GamePlayers)
           .WithOne(gp => gp.Game)
           .HasForeignKey(gp => gp.GameId);

        builder.Entity<User>()
            .HasMany(u => u.GamePlayers)
            .WithOne(gp => gp.Player)
            .HasForeignKey(gp => gp.PlayerId);

        builder.Entity<GameType>().HasData(
            new GameType
            {
                Type = "TicTacToe",
                Available = true
            },
            new GameType
            {
                Type = "Battleships",
                Available = false
            }
        );
    }


}
