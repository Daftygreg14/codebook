using System;
using Microsoft.EntityFrameworkCore.Migrations;

#nullable disable

namespace GamePlatformUI.Migrations
{
    /// <inheritdoc />
    public partial class UpdateGameColumns : Migration
    {
        /// <inheritdoc />
        protected override void Up(MigrationBuilder migrationBuilder)
        {
            migrationBuilder.RenameColumn(
                name: "GameBoard",
                table: "Games",
                newName: "GameMatchJson");
        }

        /// <inheritdoc />
        protected override void Down(MigrationBuilder migrationBuilder)
        {
            migrationBuilder.RenameColumn(
                name: "GameMatchJson",
                table: "Games",
                newName: "GameBoard");
        }
    }
}
