USE FoodCourt
GO

DELETE FROM Staff.EmployeeRoles
DELETE FROM Staff.Roles
DELETE FROM Staff.Employees
GO

/*Roles Seeds */
INSERT INTO Staff.Roles (Name)
VALUES 
('General Manager'), 
('Executive Chef'), 
('Souse Chef'),
('Cook'), 
('Waiter'),
('Delivery Driver')
;

/* Employee Seeds */
INSERT INTO Staff.Employees (FirstName, LastName, BirthDate)
VALUES
('John', 'Doe', '1992-09-09'),
('Jane', 'Doe', '1992-10-09'),
('Micky', 'Doe', '1992-11-09');

/* John Roles */
INSERT INTO Staff.EmployeeRoles (EmployeeId, RoleId, ValidFrom, ValidTo)
SELECT
  e.ID as EmployeeId
  , r.ID AS RoleId
  , CURRENT_TIMESTAMP AS ValidFrom
  , NULL AS ValidTo
FROM
  Staff.Roles AS r
CROSS JOIN
  Staff.Employees AS e
WHERE 
  r.Name IN ('General Manager', 'Souse Chef', 'Cook')
  AND e.FirstName = 'John'
;

/* Jane Roles */
INSERT INTO Staff.EmployeeRoles (EmployeeId, RoleId, ValidFrom, ValidTo)
SELECT
  e.ID as EmployeeId
  , r.ID AS RoleId
  , CURRENT_TIMESTAMP AS ValidFrom
  , NULL AS ValidTo
FROM
  Staff.Roles AS r
CROSS JOIN
  Staff.Employees AS e
WHERE 
  r.Name IN ('Executive Chef', 'Waiter', 'Cook')
  AND e.FirstName = 'Jane'
;

/* Micky Roles */
INSERT INTO Staff.EmployeeRoles (EmployeeId, RoleId, ValidFrom, ValidTo)
SELECT
  e.ID as EmployeeId
  , r.ID AS RoleId
  , CURRENT_TIMESTAMP AS ValidFrom
  , NULL AS ValidTo
FROM
  Staff.Roles AS r
CROSS JOIN
  Staff.Employees AS e
WHERE 
  r.Name IN ('Waiter', 'Delivery Driver')
  AND e.FirstName = 'Micky'
;