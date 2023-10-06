using System;
using Microsoft.EntityFrameworkCore.Migrations;

#nullable disable

namespace GamePlatformUI.Migrations
{
    /// <inheritdoc />
    public partial class UpdateGameSchema : Migration
    {
        /// <inheritdoc />
        protected override void Up(MigrationBuilder migrationBuilder)
        {
            migrationBuilder.AddColumn<string>(
                name: "GameBoard",
                table: "Games",
                type: "nvarchar(max)",
                nullable: true,
                defaultValue: "init");

            migrationBuilder.AddColumn<string>(
                name: "GameState",
                table: "Games",
                type: "nvarchar(max)",
                nullable: true
            );

            migrationBuilder.CreateIndex(
                name: "IX_GamePlayers_GameId",
                table: "GamePlayers",
                column: "GameId");

            migrationBuilder.CreateIndex(
                name: "IX_GamePlayers_PlayerId",
                table: "GamePlayers",
                column: "PlayerId");
        }

        /// <inheritdoc />
        protected override void Down(MigrationBuilder migrationBuilder)
        {
            migrationBuilder.DropColumn(
                name: "GameBoard",
                table: "Games");

            migrationBuilder.DropColumn(
                name: "GameState",
                table: "Games");
        }
    }
}
