using Microsoft.EntityFrameworkCore.Migrations;

#nullable disable

namespace GamePlatformUI.Migrations
{
    /// <inheritdoc />
    public partial class CreateGamePlayerSchema : Migration
    {
        /// <inheritdoc />
        protected override void Up(MigrationBuilder migrationBuilder)
        {
            migrationBuilder.CreateTable(
                name: "GamePlayers",
                columns: table => new
                {
                    Id = table.Column<long>(type: "bigint", nullable: false).Annotation("SqlServer:Identity", "1, 1"),
                    GameId = table.Column<long>(type: "bigint", nullable: false),
                    PlayerId = table.Column<long>(type: "nvarchar(450)", nullable: false),
                    CreatedAt = table.Column<DateTime>(type: "datetime2", defaultValueSql: "GETDATE()", nullable: false),
                    UpdatedAt = table.Column<DateTime>(type: "datetime2", defaultValueSql: "GETDATE()", nullable: false)
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_GamePlayers", x => x.Id);
                    table.ForeignKey("FK_GamePlayers_Games_GameId", x => x.GameId, "Games", "Id");
                    table.ForeignKey("FK_GamePlayers_AspNetUsers_PlayerId", x => x.PlayerId, "AspNetUsers", "Id");
                });
        }

        /// <inheritdoc />
        protected override void Down(MigrationBuilder migrationBuilder)
        {

        }
    }
}
