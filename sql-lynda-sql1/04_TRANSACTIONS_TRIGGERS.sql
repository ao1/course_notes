/*
Transactions

-- group together operations
-- transaction will fail if any operations fail
-- db will not be affected if transaction fails
-- can improve performance, especially for inserts
-- 
*/

-- setup table and test data

CREATE TABLE widgetInventory (
  id INTEGER PRIMARY KEY,
  description TEXT,
  onhand INTEGER NOT NULL
);

CREATE TABLE widgetSales (
  id INTEGER PRIMARY KEY,
  inv_id INTEGER,
  quan INTEGER,
  price INTEGER
);

INSERT INTO widgetInventory ( description, onhand ) VALUES ( 'rock', 25 );
INSERT INTO widgetInventory ( description, onhand ) VALUES ( 'paper', 25 );
INSERT INTO widgetInventory ( description, onhand ) VALUES ( 'scissors', 25 );

SELECT * FROM widgetInventory;

-- wrap the following in a transaction:
-- 1. insert a new sales record
-- 2. update an existing inventory record

BEGIN TRANSACTION;
	INSERT INTO widgetSales ( inv_id, quan, price ) VALUES ( 1, 5, 500 );
	UPDATE widgetInventory SET onhand = ( onhand - 5 ) WHERE id = 1;
END TRANSACTION;

SELECT * FROM widgetInventory;
SELECT * FROM widgetSales;

-- this will not execute, because of the rollback
BEGIN TRANSACTION;
	INSERT INTO widgetInventory ( description, onhand ) VALUES ( 'toy', 25 );
ROLLBACK;

SELECT * FROM widgetInventory;

/*
TRIGGERS

- operations automatically performed on an event
*/

-- set up test data


CREATE TABLE widgetCustomer ( id INTEGER PRIMARY KEY, name TEXT, last_order_id INT );
CREATE TABLE widgetSale ( id INTEGER PRIMARY KEY, item_id INT, customer_id INT, quan INT, price INT );

INSERT INTO widgetCustomer (name) VALUES ('Bob');
INSERT INTO widgetCustomer (name) VALUES ('Sally');
INSERT INTO widgetCustomer (name) VALUES ('Fred');

SELECT * FROM widgetCustomer;

-- after every insert to newWidgetSale table
-- update last_order_id column with {new row}.id

CREATE TRIGGER newWidgetSale AFTER INSERT ON widgetSale
    BEGIN
        UPDATE widgetCustomer SET last_order_id = NEW.id WHERE widgetCustomer.id = NEW.customer_id;
    END;
	
-- test trigger

INSERT INTO widgetSale (item_id, customer_id, quan, price) VALUES (1, 3, 5, 1995);
INSERT INTO widgetSale (item_id, customer_id, quan, price) VALUES (2, 2, 3, 1495);
INSERT INTO widgetSale (item_id, customer_id, quan, price) VALUES (3, 1, 1, 2995);

SELECT * FROM widgetSale;
SELECT * FROM widgetCustomer;
	
	
