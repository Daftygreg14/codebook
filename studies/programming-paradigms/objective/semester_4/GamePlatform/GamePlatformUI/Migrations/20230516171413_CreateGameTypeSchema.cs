using Microsoft.EntityFrameworkCore.Migrations;

#nullable disable

namespace GamePlatformUI.Migrations
{
    /// <inheritdoc />
    public partial class CreateGameTypeSchema : Migration
    {
        /// <inheritdoc />
        protected override void Up(MigrationBuilder migrationBuilder)
        {
            migrationBuilder.CreateTable(
                name: "GameTypes",
                columns: table => new
                {
                    Type = table.Column<string>(type: "nvarchar(450)", nullable: false),
                    Available = table.Column<bool>(type: "bit", nullable: false, defaultValue: false),
                    CreatedAt = table.Column<DateTime>(type: "datetime2", defaultValueSql: "GETDATE()", nullable: false),
                    UpdatedAt = table.Column<DateTime>(type: "datetime2", defaultValueSql: "GETDATE()", nullable: false),
                },
                constraints: table =>
                {
                    table.PrimaryKey("PK_GameTypes", x => x.Type);

                });
            migrationBuilder.CreateIndex(
                name: "IX_GameTypes_Available",
                table: "GameTypes",
                column: "Available",
                unique: false
            );
        }

        /// <inheritdoc />
        protected override void Down(MigrationBuilder migrationBuilder)
        {
            migrationBuilder.DropTable(name: "GameTypes");
        }
    }
}
