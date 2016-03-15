CREATE TABLE
tr.fund_characteristics (
    fdate date,
    fundno integer,
    fundname varchar(32),
    mgrcoab varchar(8),
    rdate date,
    assets integer,
    ioc smallint,
    prdate date,
    country varchar(32)
);

CREATE TABLE
tr.stock_characteristics (
    fdate date,
	cusip varchar(8),
	stkname varchar(32),
	ticker varchar(4),
	ticker2 varchar(6), 
	exchcd varchar(16),
	stkcd varchar(16),
	stkcdesc varchar(16),
	shrout1 smallint,
	prc numeric(10, 4),
	shrout2 integer,
	indcode smallint
);

CREATE TABLE
tr.holdings (
    fdate date,
	cusip varchar(8),
    fundno integer,
	shares bigint
);

CREATE TABLE
tr.holdings_change (
    fdate date,
	cusip varchar(8),
    fundno integer,
	change bigint
);