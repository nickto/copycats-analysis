CREATE TABLE
tr.fund_characteristics (
    fdate DATE,
    fundno INTEGER,
    fundname VARCHAR(32),
    mgrcoab VARCHAR(8),
    rdate DATE,
    assets INTEGER,
    ioc SMALLINT,
    prdate DATE,
    country VARCHAR(32)
);

CREATE TABLE
tr.stock_characteristics (
    fdate DATE,
	cusip VARCHAR(8),
	stkname VARCHAR(32),
	ticker VARCHAR(4),
	ticker2 VARCHAR(6), 
	exchcd VARCHAR(16),
	stkcd VARCHAR(16),
	stkcdesc VARCHAR(16),
	shrout1 SMALLINT,
	prc NUMERIC(10, 4),
	shrout2 INTEGER,
	indcode SMALLINT
);

CREATE TABLE
tr.holdings (
    fdate DATE,
	cusip VARCHAR(8),
    fundno INTEGER,
	shares BIGINT
);

CREATE TABLE
tr.holdings_change (
    fdate DATE,
	cusip VARCHAR(8),
    fundno INTEGER,
	change BIGINT
);