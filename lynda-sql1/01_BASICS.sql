/*
- data in a relational db does not have an order by default
- NULL means I don't know, unknown information palceholder
- Standard SQL formalized in 1986
- SQL ususally has some vendor specific aspects
- looking at core SQL features here 
- safer to end with a semi colon
- safer to capitalize keywords, more portable
- multi line comments not always supported
- it is possible to have very complex SQL statements
- find a personal query writing style
- methods for creating ID columns are different on different systems
*/


/*
SELECT 
*/

SELECT 'Hello World';

SELECT 1+2;

SELECT * FROM Country;

SELECT * FROM Country 
ORDER BY Name;

SELECT Name, LifeExpectancy FROM Country 
ORDER BY Name;

-- AS for column name aliases in result table

SELECT Name AS 'Country', LifeExpectancy AS 'Life Expectancy' FROM Country 
ORDER BY Name;

-- Can skip AS (but looks more confusing, don't do it), also can skip quotes 

SELECT Name Country, Code ISO, Population Pop FROM Country 
ORDER BY Code;

-- with WHERE condition

SELECT Name,Continent,Region FROM Country 
WHERE Continent = 'Europe';

-- with a LIMIT clause, to limit results

SELECT Name,Continent,Region FROM Country 
WHERE Continent = 'Europe' 
LIMIT 5;

-- boolean expression evaluation using OR

SELECT Name,Continent,Population FROM Country 
WHERE Population < 100000 OR Population IS NULL
ORDER BY Population DESC;

-- boolean expression evaluation, pop less than 1mil or NULL, and from Oceania 

SELECT Name,Continent,Population FROM Country 
WHERE (Population < 100000 OR Population IS NULL) AND Continent = 'Oceania'
ORDER BY Population DESC;

/*
ORDER BY (multi level order)
*/

SELECT Continent, Region, Name FROM Country
ORDER BY Continent, Region, Name;

/*
NULL values
*/

-- NULL is the lack of a value, special state
-- NULL is not equal to a empty string
-- this will not work

SELECT * FROM test2 WHERE a = NULL;

-- find any row where a is NULL

SELECT * FROM test2 WHERE a IS NULL;

-- find any row where a is not NULL

SELECT * FROM test2 WHERE a IS NOT NULL;

SELECT Name, IndepYear FROM Country
WHERE IndepYear IS NOT NULL
ORDER BY IndepYear;  

/*
LIKE
*/

-- column string search, equivalent to *island*

SELECT Name,Continent,Population FROM Country 
WHERE Name LIKE '%island%' AND Continent = 'Oceania'
ORDER BY Population DESC;

-- column string search, equivalent to North*

SELECT Name,Continent,Population FROM Country 
WHERE Name LIKE 'North%'
ORDER BY Population DESC;

-- column string search, any string where second letter is an 'a', equivalent to ?a* 

SELECT Name,Continent,Population FROM Country 
WHERE Name LIKE '_a%'
ORDER BY Population DESC;

/*
IN
*/

-- filter results from a list using IN

SELECT Name,Continent,Population FROM Country 
WHERE Continent IN ( 'Europe' , 'Asia' , 'Oceania' )
ORDER BY Population DESC;

/*
DISTINCT
*/

-- show only unique results

SELECT DISTINCT Continent FROM Country;

/*
CREATE TABLE
*/


-- part in middle is the 'db schema'

CREATE TABLE test (
	a INTEGER,
	b TEXT
);
INSERT INTO test VALUES (1,'a');
INSERT INTO test VALUES (2,'b');
INSERT INTO test VALUES (3,'c');
SELECT * FROM test;


CREATE TABLE test2 (
	a INTEGER,
	b TEXT,
	c TEXT
);
INSERT INTO test2 VALUES (1,'two','three');
INSERT INTO test2 VALUES (2,'two_','three_');
SELECT * FROM test2;


-- columns a and b will not accept NULL values

CREATE TABLE test3 (
	a INTEGER NOT NULL,
	b TEXT NOT NULL,
	c TEXT
);
INSERT INTO test3 VALUES (1,'THIS','THAT');
SELECT * FROM test3;


-- column a accepts only unique values
-- column c will have a default value 'panda'

CREATE TABLE test3 (
	a TEXT UNIQUE,
	b TEXT,
	c TEXT DEFAULT 'panda'
);
INSERT INTO test3 (a,b,c) VALUES ('ONE','TWO','THREE');
INSERT INTO test3 (a,b) VALUES ('FOUR','FIVE');
SELECT * FROM test3;



-- add column d with ALTER, no default value (will use NULLs)
-- add column e with ALTER, default value 'PANDAS'

CREATE TABLE test4 (
	a TEXT,
	b TEXT,
	c TEXT
);
INSERT INTO test4 (a,b,c) VALUES ('ONE','TWO','THREE');
INSERT INTO test4 (a,b,c) VALUES ('FOO','BAR','BAZ');
INSERT INTO test4 (a,b,c) VALUES ('GO','RUBY','SCALA');
ALTER TABLE test4 ADD d TEXT;
ALTER TABLE test4 ADD e TEXT DEFAULT 'PANDAS';
SELECT * FROM test4;


-- create an auto-ID column (syntax for SQLite)

CREATE TABLE test5 (
	id INTEGER PRIMARY KEY,
	a INTEGER,
	b TEXT
);
INSERT INTO test5 (a,b) VALUES ('TWO','THREE');
INSERT INTO test5 (a,b) VALUES ('AA','BB');
SELECT * FROM test5;


-- select only rows with unqiue values for a,b,a and b together

CREATE TABLE test6 (
	a INT,
	b INT
);
INSERT INTO test6 (a,b) VALUES (1,1);
INSERT INTO test6 (a,b) VALUES (2,1);
INSERT INTO test6 (a,b) VALUES (3,1);
INSERT INTO test6 (a,b) VALUES (1,1);
INSERT INTO test6 (a,b) VALUES (1,2);
INSERT INTO test6 (a,b) VALUES (1,3);
SELECT DISTINCT a FROM test6;
SELECT DISTINCT b FROM test6;
SELECT DISTINCT a,b FROM test6;


/*
DROP TABLE
*/

-- delete a table with DROP

DROP TABLE test;



/*
INSERT
*/

INSERT INTO Customer ( name , address , city , state , zip )
VALUES ( 'Fred' , '123 4 Ave' , 'Bedrock' , 'CA' , '91234');
SELECT * FROM Customer;

-- insert only some values, address/zip will be null

INSERT INTO Customer ( name , city , state )
VALUES ( 'Jimmy' , 'Dallas' , 'TX' );
SELECT * FROM Customer;

/*
INSERT, feed from SELECT
*/

INSERT INTO test2 (a,b,c)
SELECT id, name, description FROM item;

/*
UPDATE
*/

-- update Address and Zip columns for row id 5

UPDATE Customer SET Address = '12 Music', Zip = '98056' 
WHERE id = 5;
SELECT * FROM Customer;

-- bring back NULL values

UPDATE Customer SET Address = NULL, Zip = NULL 
WHERE id = 5;
SELECT * FROM Customer;

/*
ALTER
*/
-- if you use ALTER, examine for other code that depends on the schema you are changing

ALTER TABLE test4 ADD d TEXT;

/*
DELETE rows from table
*/

-- delete row where column a equals 2
-- run the query with SELECT FROM first

DELETE FROM test2
WHERE a = 2;
SELECT * FROM test2;

DELETE FROM Customer WHERE id = 5;
SELECT * FROM Customer;

/*
COUNT
*/

-- COUNT for counting number of rows returned

SELECT COUNT(*) FROM Country;

SELECT COUNT(*) FROM Country
WHERE Population > 1000000;

SELECT COUNT(*) FROM Country
WHERE Population > 1000000 AND Continent = 'Europe';


/*
CASE
*/

-- conditional statements, 2 methods

CREATE TABLE booltest (
	a INT,
	b INT
);
INSERT INTO booltest VALUES (1,0);
SELECT * FROM booltest;


SELECT 
	CASE WHEN a THEN 'true' ELSE 'false' END 
	AS boolA,
	CASE WHEN b THEN 'true' ELSE 'false' END 
	AS boolB
FROM booltest;


SELECT 
	CASE a WHEN 1 THEN 'true' ELSE 'false' END 
	AS boolA,
	CASE b WHEN 1 THEN 'true' ELSE 'false' END 
	AS boolB
FROM booltest;



