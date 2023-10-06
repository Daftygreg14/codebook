using System;
using Microsoft.EntityFrameworkCore.Migrations;

#nullable disable

namespace GamePlatformUI.Migrations
{
    /// <inheritdoc />
    public partial class CreateGameSchema : Migration
    {
        /// <inheritdoc />
        protected override void Up(MigrationBuilder migrationBuilder)
        {
            migrationBuilder.CreateTable(
                name: "Games",
                columns: table => new
                {
                    Id = table.Column<long>(type: "bigint", nullable: false).Annotation("SqlServer:Identity", "1, 1"),
                    GameType = table.Column<string>(type: "nvarchar(450)", nullable: false),
                    CreatedAt = table.Column<DateTime>(type: "datetime2", defaultValueSql: "GETDATE()", nullable: false),
                    UpdatedAt = table.Column<DateTime>(type: "datetime2", defaultValueSql: "GETDATE()", nullable: false),
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_Games", x => x.Id);
                    table.ForeignKey("FK_Games_GameTypes_GameType", x => x.GameType, "GameTypes", "Type");
                });
        }

        /// <inheritdoc />
        protected override void Down(MigrationBuilder migrationBuilder)
        {
            migrationBuilder.DropTable(name: "Games");
        }
    }
}
