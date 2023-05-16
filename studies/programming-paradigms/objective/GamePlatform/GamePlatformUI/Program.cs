using Microsoft.EntityFrameworkCore;
using Microsoft.AspNetCore.Identity;
using GamePlatformUI.Areas.Identity.Data;
using GamePlatformUI.Repository;
using GamePlatformUI.Services;

var builder = WebApplication.CreateBuilder(args);
string connectionString = Environment.GetEnvironmentVariable("DB_CONNECTION_STRING");

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
    AddEntityFrameworkStores<ApplicationDbContext>();


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
app.MapRazorPages();
app.Run();
