USE FoodCourt
GO

DELETE FROM Staff.PhoneNumbers
DELETE FROM Staff.PhoneNumberTypes
DELETE FROM Staff.Addresses
GO

INSERT INTO Staff.PhoneNumberTypes (Type)
VALUES
('Main'),
('Secondary');

/* John Contact Details */
INSERT INTO Staff.Addresses (EmployeeID, AddressOne, AddressTwo, ZipCode, CityID)
SELECT
  e.ID AS EmployeeID
  , 'Mazowiecka' AS AddressOne
  , '8/14' AS AddressTwo
  , '02771' AS ZipCode
  , c.ID AS CityId
FROM 
  Staff.Employees AS e
CROSS JOIN
  GlobalConfig.Cities AS c
WHERE
  e.FirstName = 'John'
  AND c.Name = 'Warsaw'

INSERT INTO Staff.PhoneNumbers (NumberType, EmployeeId, Number, Description)
SELECT
  'Secondary' AS NumberType
  , ID AS EmployeeID
  , '606707232' Number
  , 'Do not use unless nescessary'
FROM 
  Staff.Employees
WHERE 
  FirstName = 'John';

INSERT INTO Staff.PhoneNumbers (NumberType, EmployeeId, Number)
SELECT
  'Main' AS NumberType
  , ID AS EmployeeID
  , '22883223112' Number
FROM 
  Staff.Employees
WHERE 
  FirstName = 'John';

/* Jane Contact Details */
INSERT INTO Staff.Addresses (EmployeeID, AddressOne, AddressTwo, ZipCode, CityID)
SELECT
  e.ID AS EmployeeID
  , 'Mazowiecka' AS AddressOne
  , '8/14' AS AddressTwo
  , '02771' AS ZipCode
  , c.ID AS CityId
FROM 
  Staff.Employees AS e
CROSS JOIN
  GlobalConfig.Cities AS c
WHERE
  e.FirstName = 'Jane'
  AND c.Name = 'Warsaw'

INSERT INTO Staff.PhoneNumbers (NumberType, EmployeeId, Number)
SELECT
  'Main' AS NumberType
  , ID AS EmployeeID
  , '22883223112' Number
FROM 
  Staff.Employees
WHERE 
  FirstName = 'Jane';

 /* Micky Contact Details */
INSERT INTO Staff.Addresses (EmployeeID, AddressOne, ZipCode, CityID)
SELECT
  e.ID AS EmployeeID
  , 'Bolkowska 12' AS AddressOne
  , '02001' AS ZipCode
  , c.ID AS CityId
FROM 
  Staff.Employees AS e
CROSS JOIN
  GlobalConfig.Cities AS c
WHERE
  e.FirstName = 'Micky'
  AND c.Name = 'Warsaw'

INSERT INTO Staff.PhoneNumbers (NumberType, EmployeeId, Number)
SELECT
  'Main' AS NumberType
  , ID AS EmployeeID
  , '224436623' Number
FROM 
  Staff.Employees
WHERE 
  FirstName = 'Micky';