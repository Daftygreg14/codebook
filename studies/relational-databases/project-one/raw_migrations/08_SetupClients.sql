USE FoodCourt
GO

CREATE SCHEMA Clients
GO

BEGIN TRANSACTION T1
CREATE TABLE Clients.Customers (
	ID INT IDENTITY(1,1) PRIMARY KEY
	, FirstName VARCHAR(50) NOT NULL
	, LastName VARCHAR(50) NULL
	, PhoneNumber VARCHAR(15) NULL
	, Email VARCHAR(255) NULL
	, Blocked BIT NOT NULL DEFAULT 0
	/* Timestamps */
	, CreatedAt DATETIME2 DEFAULT GETDATE() NOT NULL
	, UpdatedAt DATETIME2 DEFAULT GETDATE() NOT NULL
)

CREATE TABLE Clients.Addresses (
  ID INT IDENTITY(1,1) PRIMARY KEY
  , CustomerID INT NOT NULL
  , AddressOne VARCHAR(255) NOT NULL
  , AddressTwo VARCHAR(255) NULL
  , ZipCode CHAR(5) NOT NULL
  , CityID INT NOT NULL
  , Description VARCHAR(Max) NULL
  /* Timestamps */
  , CreatedAt DATETIME2 DEFAULT GETDATE() NOT NULL
  , UpdatedAt DATETIME2 DEFAULT GETDATE() NOT NULL
  /* Foreign Keys */
  , CONSTRAINT FK_Clients_Addresses_Customers FOREIGN KEY (CustomerID) REFERENCES Clients.Customers (ID)
  , CONSTRAINT FK_Clients_Addresses_Cities FOREIGN KEY (CityID) REFERENCES GlobalConfig.Cities (ID)
)
COMMIT TRANSACTION T1