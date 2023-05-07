USE FoodCourt
GO

BEGIN TRANSACTION T1
DELETE FROM Restaurants.OpeningHours
DELETE FROM Restaurants.Schedules
DELETE FROM Restaurants.Addresses
DELETE FROM Restaurants.PhoneNumbers
DELETE FROM Restaurants.Locations
GO

/*Locations Seed */
INSERT INTO Restaurants.Locations (Name, Slug)
VALUES 
('Location 1', 'location-1'), 
('Location 2', 'location-2'), 
('Location 3', 'location-3'),
('Location 4', 'location-4'), 
('Location 5', 'location-5');

/*Location Address Seeds */
INSERT INTO Restaurants.Addresses (LocationID, AddressOne, AddressTwo, Longitude, Latitude, ZipCode, CityId)
/*
/* Shedules Seed */
DECLARE @days_of_week TABLE (DayOfWeek TINYINT);
INSERT INTO @days_of_week (DayOfWeek)
VALUES (1), (2), (3), (4), (5), (6), (7);

INSERT INTO Restaurants.Schedules (LocationID, DayOfWeek, StartDate, EndDate)
SELECT
	l.ID
	, dow.DayOfWeek
	, CURRENT_TIMESTAMP
	, NULL
FROM
	Restaurants.Locations AS l
CROSS JOIN
	@days_of_week AS dow


INSERT INTO Restaurants.OpeningHours (ScheduleId, StartTime, EndTime)
SELECT 
	s.ID AS ScheduleId
	, CASE
		WHEN s.DayOfWeek = 7 THEN '10:00'
		ELSE '8:00'
	  END AS StartTime
	, CASE 
	    WHEN s.DayOfWeek IN (5,6,7) THEN '23:00'
		ELSE '20:00'
	END
FROM
	Restaurants.Schedules AS s
	*/