/*
Subselects

- nested select statements
- select statements can be used as a data source for another select statement
*/

-- create temporary table / data

CREATE TABLE t ( a TEXT, b TEXT );
INSERT INTO t VALUES ( 'NY0123', 'US4567' );
INSERT INTO t VALUES ( 'AZ9437', 'GB1234' );
INSERT INTO t VALUES ( 'CA1279', 'FR5678' );
SELECT * FROM t;

-- slice original data into new columns

SELECT SUBSTR(a, 1, 2) AS State, SUBSTR(a, 3) AS SCode, SUBSTR(b, 1, 2) AS Country, SUBSTR(b, 3) AS CCode FROM t;

-- use subselect as source table for another select

SELECT co.Name, ss.CCode FROM (
    SELECT SUBSTR(a, 1, 2) AS State, SUBSTR(a, 3) AS SCode, SUBSTR(b, 1, 2) AS Country, SUBSTR(b, 3) AS CCode 
	FROM t
  ) AS ss
  JOIN Country AS co
    ON co.Code2 = ss.Country;

	
-- select album ids for albums containing tracks over 90 minutes, filter as unique
SELECT DISTINCT album_id FROM track 
WHERE duration <= 90;

-- feed in above query as source, select those album records by album id
SELECT * FROM album
  WHERE id IN (
	SELECT DISTINCT album_id FROM track 
	WHERE duration <= 90 );

-- use original query as t, join with custom select a, on album id field
SELECT a.title AS album, a.artist, t.track_number AS seq, t.title, t.duration AS secs
  FROM album AS a
  JOIN (
    SELECT album_id, track_number, duration, title
      FROM track
      WHERE duration <= 90 ) 
  AS t
  ON t.album_id = a.id
  ORDER BY a.title, t.track_number;
	

/*
Views

- saving query results for re-use
*/

-- get track duration for all tracks
SELECT id, album_id, title, track_number, duration / 60 AS m, duration % 60 AS s
FROM track;

-- make it into a view for re-use

CREATE VIEW trackView AS
	SELECT id, album_id, title, track_number, duration / 60 AS m, duration % 60 AS s
	FROM track;

-- run a query on the view
SELECT title FROM trackView
WHERE title LIKE '%Life%';

-- run a join query on the view
SELECT a.title AS album, a.artist, t.track_number AS seq, t.title, t.m, t.s
	FROM album AS a
	JOIN trackView AS t
	ON t.album_id = a.id
ORDER BY a.title, t.track_number

-- delete the view
DROP VIEW trackView;

-- create a view from a join query
CREATE VIEW joinedAlbum AS
  SELECT a.artist AS artist,
      a.title AS album,
      t.title AS track,
      t.track_number AS trackno,
      t.duration / 60 AS m,
      t.duration % 60 AS s
    FROM track AS t
    JOIN album AS a
      ON a.id = t.album_id;

-- query the view	  
SELECT * FROM joinedAlbum;
SELECT * FROM joinedAlbum WHERE artist = 'Jimi Hendrix';

-- query the view again 
-- construct a new duration column from m and s
SELECT artist, album, track, trackno, 
    m || ':' || CASE WHEN s < 10 THEN '0' || s ELSE s END AS duration
    FROM joinedAlbum;

-- drop view
DROP VIEW IF EXISTS joinedAlbum;















