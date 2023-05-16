using GamePlatformUI.Areas.Identity.Data;
using GamePlatformUI.Models;
using Microsoft.AspNetCore.Identity;
using Microsoft.AspNetCore.Identity.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore;
using System.Reflection.Emit;

namespace GamePlatformUI.Repository;

public class ApplicationDbContext : IdentityDbContext<User>
{
    public ApplicationDbContext(DbContextOptions<ApplicationDbContext> options)
        : base(options)
    {
    }

    public DbSet<GameType> GameTypes { get; set; }

    protected override void OnModelCreating(ModelBuilder builder)
    {
        base.OnModelCreating(builder);

        // Game Types DB Context
        builder.Entity<GameType>().Property(gt => gt.UpdatedAt).ValueGeneratedOnAddOrUpdate().HasDefaultValueSql("GETUTCDATE()");
    }
}
