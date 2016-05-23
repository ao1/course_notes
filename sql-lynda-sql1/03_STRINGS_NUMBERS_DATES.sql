/*
Strings 

- most sql types use 'string'
- older versions of mysql use "string"
- escape quotes as follows 'Here''s a single quote'
*/


/*
LENGTH
*/

-- get length of a string
SELECT LENGTH('TEST')

-- make a 'length of city name' column
SELECT Name, LENGTH(Name) AS Len
FROM City
ORDER BY Len DESC, Name

/*
CONCAT
*/

--  mysql concat
SELECT CONCAT('This' , '&' , 'that');

-- ms sql server concat
SELECT 'This' + '&' + 'that';

--sqllite server concat (this is the standard)
SELECT 'This' || '&' || 'that';

/*
SUBSTR
*/

-- slice from 6th element of string, forward
SELECT SUBSTR('THIS STRING',6);

-- create tables from sliced strings
SELECT released,
	SUBSTR(released,1,4) AS Year,
	SUBSTR(released,6,2) AS Month,
	SUBSTR(released,9,2) AS Day
FROM album
ORDER BY released;

/*
TRIM
*/

-- trim all spaces in string
SELECT TRIM('   string   ');

-- trim all spaces on left side of string
SELECT LTRIM('   string   ');

-- trim all spaces on right side of string
SELECT RTRIM('   string   ');

-- trim all dots in string (dots need to be together)
SELECT TRIM('...string...' , '.');

/*
UPPER
*/

-- will return 1 (true)
SELECT UPPER('StrInG') == UPPER('string');
SELECT UPPER('StrInG') == 'STRING';


/*
LOWER
*/

-- will return 1 (true)
SELECT LOWER('StrInG') == 'string';

-- get all city names in lower case
SELECT LOWER(Name) FROM City ORDER BY Name;


/*
Numeric Types

- data types are independent between different SQL dbs
- integers: INTEGER , DECIMAL, MONEY
- reals: REAL, FLOAT
- reals can be very large or small but only to a nunmber of significant digits
- real values are not precise
*/

-- int + real = real
SELECT ( 1 + 1.0 );

-- evaluate expression type (in this case real)
-- the + sign coerces into integers or reals, even if adding 2 strings
SELECT TYPEOF( 1 + 1.0 );

/*
CAST
*/

-- int/int will result in int, imprecise
SELECT 17 / 5, 17 % 5

-- cast int as real
SELECT CAST(1 AS REAL) / 2;

-- result will be a real
SELECT TYPEOF(CAST(1 AS REAL) / 2);

/*
ROUND
*/

-- round to 1 decimal place
SELECT ROUND(2.55555555)

-- round to 3 decimal places
SELECT ROUND(2.55555555 ,3)

/*
Dates and Times

- functions and syntax are not standardized
- time is UTC
*/

-- return current date and time
SELECT DATETIME('now');

-- return current time
SELECT TIME('now');

-- return current date and time, + 3 days
SELECT DATETIME('now' , '+3 days');

-- return current date and time, - 1 month
SELECT DATETIME('now' , '-1 month');

-- return current time, +2h20min 1 year ahead
SELECT DATETIME('now' , '+2 hours' , '+20 minutes', '+1 year');







