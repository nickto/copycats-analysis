-- create indeces
CREATE INDEX ON tr.fund_characteristics (fundno);
CREATE INDEX ON tr.fund_characteristics (fdate);

CREATE INDEX ON tr.stock_characteristics (cusip);
CREATE INDEX ON tr.stock_characteristics (fdate);

CREATE INDEX ON tr.holdings (fundno);
CREATE INDEX ON tr.holdings (fdate);

CREATE INDEX ON tr.holdings_change (fundno);
CREATE INDEX ON tr.holdings_change (fdate);