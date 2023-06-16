# Game Platform
## Concept
Game Platoform application as a IV semester project for the Programming Paradigms course at the Faculty of Computer Science and Engineering - Skopje.
It covers basic MVC for BattelShip game, and shows ability how to extend it to other games in future once needed. 

## Modules
### Game Platform UI
ASP.NET Core MVC application that provides the user interface for the Game Platform.
It is a simple web application that uses bootstrap for styling html elements.

Additionaly it uses the following core libraries:
- Microsoft.AspNetCore.Identity.EntityFrameworkCore - for user management. It provides the user registration and login functionality.
- Microsoft.EntityFrameworkCore.SqlServer - for database access. It provides the functionality to access the database.
- Microsoft.EntityFrameworkCore.Tools - for database migrations. It provides the functionality to create and update the database schema.
- Microsoft.VisualStudio.Web.CodeGeneration.Design - for scaffolding. I just hate to write boilerplate code.

### Dependencies
- SqlServer - for database access. It provides the functionality to access the database.

### Game Platform UI Tests
xUnit test project that contains the tests for the Game Platform UI project.
Only basic set of tests are implemented. The tests are not covering all the functionality of the application.

## Database
