-- Create TR indeces
CREATE INDEX ON tr.fund_characteristics (fundno);
CREATE INDEX ON tr.fund_characteristics (fdate);

CREATE INDEX ON tr.stock_characteristics (cusip);
CREATE INDEX ON tr.stock_characteristics (fdate);

CREATE INDEX ON tr.holdings (fundno);
CREATE INDEX ON tr.holdings (fdate);

CREATE INDEX ON tr.holdings_change (fundno);
CREATE INDEX ON tr.holdings_change (fdate);

-- Create CRSP indeces
CREATE INDEX ON crsp.contact_info(crsp_fundno);
CREATE INDEX ON crsp.contact_info(chgdt);

CREATE INDEX ON crsp.daily_returns(crsp_fundno);
CREATE INDEX ON crsp.daily_returns(caldt);

CREATE INDEX ON crsp.front_load(crsp_fundno);
CREATE INDEX ON crsp.front_load(front_group_no);
CREATE INDEX ON crsp.front_load(begdt);
CREATE INDEX ON crsp.front_load(enddt);

CREATE INDEX ON crsp.fund_dividends(crsp_fundno);
CREATE INDEX ON crsp.fund_dividends(caldt);

CREATE INDEX ON crsp.fund_map(crsp_fundno);
CREATE INDEX ON crsp.fund_map(crsp_portno);
CREATE INDEX ON crsp.fund_map(begdt);
CREATE INDEX ON crsp.fund_map(enddt);
CREATE INDEX ON crsp.fund_map(cusip8);

CREATE INDEX ON crsp.fund_summary(crsp_fundno);
CREATE INDEX ON crsp.fund_summary(caldt);
CREATE INDEX ON crsp.fund_summary(cusip8);
CREATE INDEX ON crsp.fund_summary(crsp_portno);

CREATE INDEX ON crsp.monthly_returns(crsp_fundno);
CREATE INDEX ON crsp.monthly_returns(caldt);

CREATE INDEX ON crsp.portfolio_holdings(crsp_portno);
CREATE INDEX ON crsp.portfolio_holdings(report_dt);
CREATE INDEX ON crsp.portfolio_holdings(eff_dt);

CREATE INDEX ON crsp.rear_load(crsp_fundno);
CREATE INDEX ON crsp.rear_load(rear_group_no);
CREATE INDEX ON crsp.rear_load(begdt);
CREATE INDEX ON crsp.rear_load(enddt);

-- Create MFLINKS indeces
CREATE INDEX ON mflinks.link1(fundno);
CREATE INDEX ON mflinks.link1(wfcin);
CREATE INDEX ON mflinks.link1(fdate);
CREATE INDEX ON mflinks.link2(crsp_fundno);
CREATE INDEX ON mflinks.link2(wfcin);
CREATE INDEX ON mflinks.link2(ncusip);
CREATE INDEX ON mflinks.link2(cusip8);

-- Create Stocks indeces
CREATE INDEX ON stocks.daily(date);
CREATE INDEX ON stocks.daily(cusip);
