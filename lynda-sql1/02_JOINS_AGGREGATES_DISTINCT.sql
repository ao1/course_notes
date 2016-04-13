/*
- we use joins to return records at the intersection of multiple tables, like in a venn diagram
- inner join is the default join, just intersection
- left outer join = intersection + left side
- right outer join = intersection + right side
- note: some databases don't support right joints
- full outer join = is everything
*/

/*
set up 2 tables with sample data
*/

CREATE TABLE left ( id INTEGER, description TEXT );
CREATE TABLE right ( id INTEGER, description TEXT );

INSERT INTO left VALUES ( 1, 'left 01' );
INSERT INTO left VALUES ( 2, 'left 02' );
INSERT INTO left VALUES ( 3, 'left 03' );
INSERT INTO left VALUES ( 4, 'left 04' );
INSERT INTO left VALUES ( 5, 'left 05' );
INSERT INTO left VALUES ( 6, 'left 06' );
INSERT INTO left VALUES ( 7, 'left 07' );
INSERT INTO left VALUES ( 8, 'left 08' );
INSERT INTO left VALUES ( 9, 'left 09' );

INSERT INTO right VALUES ( 6, 'right 06' );
INSERT INTO right VALUES ( 7, 'right 07' );
INSERT INTO right VALUES ( 8, 'right 08' );
INSERT INTO right VALUES ( 9, 'right 09' );
INSERT INTO right VALUES ( 10, 'right 10' );
INSERT INTO right VALUES ( 11, 'right 11' );
INSERT INTO right VALUES ( 11, 'right 12' );
INSERT INTO right VALUES ( 11, 'right 13' );
INSERT INTO right VALUES ( 11, 'right 14' );

SELECT * FROM left; 
SELECT * FROM right;


/*
Joins
*/

-- inner join, intersection only
SELECT l.description AS left, r.description AS right
FROM left AS l
JOIN right AS r
ON l.id = r.id;

SELECT * FROM left;  
SELECT * FROM right;


-- left join, intersection + left side
SELECT l.description AS left, r.description AS right
FROM left AS l
LEFT JOIN right AS r
ON l.id = r.id;

SELECT * FROM left;  
SELECT * FROM right;


/*
Aggregate functions
*/

SELECT Region, COUNT(*) AS Count
	FROM Country
	GROUP BY Region
	ORDER BY Count DESC;

	
-- get a count of tracks in each album
SELECT a.title AS Album, COUNT(t.track_number) as Tracks
  FROM track AS t
  JOIN album AS a
    ON a.id = t.album_id
  GROUP BY a.id
  ORDER BY Tracks DESC, Album;

  
-- get a count of tracks in each album with more than 10 tracks 
-- use WHERE on regular data
-- use HAVING on aggregate data 
SELECT a.title AS Album, COUNT(t.track_number) as Tracks
  FROM track AS t
  JOIN album AS a
    ON a.id = t.album_id
  GROUP BY a.id
  HAVING Tracks >= 10
  ORDER BY Tracks DESC, Album;

-- use WHERE on regular data
-- use HAVING on aggregate data 
SELECT a.title AS Album, COUNT(t.track_number) as Tracks
  FROM track AS t
  JOIN album AS a
    ON a.id = t.album_id
  WHERE a.artist = "The Beatles"
  GROUP BY a.id
  HAVING Tracks >= 10
  ORDER BY Tracks DESC, Album;

-- get average population per region
SELECT Region , AVG(Population) FROM Country
GROUP BY Region;

-- get sum of population per region
SELECT Region , SUM(Population) FROM Country
GROUP BY Region;

/*
DISTINCT
*/

-- select unique heads of state, skip duplicates
SELECT DISTINCT Name, HeadOfState FROM Country;

-- count unique heads of state
SELECT COUNT(DISTINCT HeadOfState) FROM Country;

-- in some versions of oracle db, UNIQUE can be used as well







