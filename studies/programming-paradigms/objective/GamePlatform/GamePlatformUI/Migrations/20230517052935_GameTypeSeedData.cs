using System;
using Microsoft.EntityFrameworkCore.Migrations;

#nullable disable

#pragma warning disable CA1814 // Prefer jagged arrays over multidimensional

namespace GamePlatformUI.Migrations
{
    /// <inheritdoc />
    public partial class GameTypeSeedData : Migration
    {
        /// <inheritdoc />
        protected override void Up(MigrationBuilder migrationBuilder)
        {
            migrationBuilder.InsertData(
                table: "GameTypes",
                columns: new[] { "Type", "Available", "CreatedAt" },
                values: new object[,]
                {
                    { "Battleships", false, new DateTime(2023, 5, 17, 7, 29, 35, 656, DateTimeKind.Local).AddTicks(3380) },
                    { "TicTacToe", true, new DateTime(2023, 5, 17, 7, 29, 35, 656, DateTimeKind.Local).AddTicks(3306) }
                });
        }

        /// <inheritdoc />
        protected override void Down(MigrationBuilder migrationBuilder)
        {
            migrationBuilder.DeleteData(
                table: "GameTypes",
                keyColumn: "Type",
                keyValue: "Battleships");

            migrationBuilder.DeleteData(
                table: "GameTypes",
                keyColumn: "Type",
                keyValue: "TicTacToe");
        }
    }
}
