using Microsoft.EntityFrameworkCore;
using Microsoft.AspNetCore.Identity;
using GamePlatformUI.Areas.Identity.Data;
using GamePlatformUI.Repository;
using GamePlatformUI.Services;

var builder = WebApplication.CreateBuilder(args);
string connectionString = builder.Configuration.GetConnectionString("DefaultConnection");

// Add services to the container.
builder.Services.AddControllersWithViews();
builder.Services.AddDbContext<ApplicationDbContext>(options => options.UseSqlServer(connectionString));
builder.Services.AddScoped<IGameTypeRepository, GameTypeRepository>();
builder.Services.AddScoped<IGameRepository, GameRepository>();

// Identity
builder.Services.
    AddDefaultIdentity<User>(options => {
        options.SignIn.RequireConfirmedAccount = false;
        options.SignIn.RequireConfirmedEmail = false;
        options.SignIn.RequireConfirmedPhoneNumber = false;
    }).
    AddEntityFrameworkStores<ApplicationDbContext>().
    AddUserManager<UserManager<User>>();


var app = builder.Build();

// Configure the HTTP request pipeline.
if (!app.Environment.IsDevelopment())
{
    app.UseExceptionHandler("/Home/Error");
}
app.UseStaticFiles();

app.UseRouting();
app.UseAuthentication();
app.UseAuthorization();

app.MapControllerRoute(
    name: "default",
    pattern: "{controller=Home}/{action=Index}/{id?}");

// Game routes
app.MapControllerRoute(
    name: "GamesIndex",
    pattern: "Games",
    defaults: new { controller = "Games", action = "Index" });
app.MapControllerRoute(
    name: "GamesCreate",
    pattern: "Games/Create",
    defaults: new { controller = "Games", action = "Create" });
app.MapControllerRoute(
    name: "GamesDelete",
    pattern: "Games/Delete/{id}",
    defaults: new { controller = "Games", action = "Delete" });

app.MapRazorPages();
app.Run();
