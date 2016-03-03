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
    country varchar(32),
    PRIMARY KEY(fdate, fundno)
)