-- create indeces
CREATE INDEX ON tr.fund_characteristics (fundno);
CREATE INDEX ON tr.fund_characteristics (fdate);

CREATE INDEX ON tr.stock_characteristics (cusip);
CREATE INDEX ON tr.stock_characteristics (fdate);

CREATE INDEX ON tr.holdings (fundno);
CREATE INDEX ON tr.holdings (fdate);

CREATE INDEX ON tr.holdings_change (fundno);
CREATE INDEX ON tr.holdings_change (fdate);


-- create a new table to avoid regular joins (might be dropped if after indexing the performance is okay anyway)
/*
CREATE TABLE tr.detailed_holdings AS 
	SELECT 
		tr.holdings.*, 
		tr.stock_characteristics.stkname,
        tr.stock_characteristics.ticker,
        tr.stock_characteristics.ticker2,
        tr.stock_characteristics.exchcd,
        tr.stock_characteristics.stkcd,
        tr.stock_characteristics.stkcdesc,
        tr.stock_characteristics.shrout1,
        tr.stock_characteristics.prc,
        tr.stock_characteristics.shrout2,
        tr.stock_characteristics.indcode
	FROM tr.holdings
	LEFT OUTER JOIN tr.stock_characteristics ON
		(
			tr.holdings.cusip = tr.stock_characteristics.cusip AND 
			tr.holdings.fdate = tr.stock_characteristics.fdate
		)
    CONSTRAINT detailed_holdings_pkey PRIMARY KEY (fdate, fundno, cusip)
*/