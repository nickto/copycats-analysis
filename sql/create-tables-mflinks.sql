CREATE TABLE
mflinks.link1 (
    fundno INTEGER,
    wfcin INTEGER,
    fdate date,
    fundno_id NUMERIC(3,0),
    fundname VARCHAR(40),
    rdate date,
    mgrcoab VARCHAR(6),
    assets DOUBLE PRECISION,
    num_holdings INTEGER,
    ioc INTEGER,
    prdate DATE,
    country VARCHAR(30)
);

CREATE TABLE
mflinks.link2 (
    crsp_fundno NUMERIC(10,0),
    wfcin INTEGER,
    fund_name VARCHAR(140),
    nasdaq VARCHAR(6),
    ncusip VARCHAR(10),
    cusip8 VARCHAR(8),
    merge_fundno NUMERIC(10,0) 
);

