CREATE TABLE
crsp.contact_info (
    crsp_fundno INTEGER,
    chgdt DATE,
    chgenddt DATE,
    address1 VARCHAR(40),
    address2 VARCHAR(40),
    city VARCHAR(30),
    state VARCHAR(2),
    zip CHAR(5),
    phone_number VARCHAR(12),
    fund_toll_free VARCHAR(12),
    website VARCHAR(256)
);

CREATE TABLE
crsp.daily_returns (
    crsp_fundno INTEGER,
    caldt DATE,
    dnav NUMERIC(16,8),
    dret NUMERIC(16,8)
);

CREATE TABLE
crsp.front_load(
    crsp_fundno INTEGER,
    front_group_no INTEGER,
    begdt DATE,
    enddt DATE,
    dollar_amt INTEGER,
    front_load NUMERIC(6,4)
);

CREATE TABLE
crsp.fund_dividends (
    crsp_fundno INTEGER,
    caldt DATE,
    dis_type VARCHAR(2),
    dis_amt NUMERIC(13,11),
    reinvest_nav NUMERIC(16,8),
    spl_ratio NUMERIC(13,11)
);


CREATE TABLE
crsp.fund_map(
    crsp_fundno INTEGER,
    crsp_portno INTEGER,
    begdt DATE,
    enddt DATE,
    cusip8 VARCHAR(10),
    fund_name VARCHAR(140),
    nasdaq VARCHAR(5),
    ncusip VARCHAR(9),
    first_offer_dt DATE,
    mgmt_name VARCHAR(80),
    mgmt_cd VARCHAR(4),
    mgr_name VARCHAR(30),
    mgr_dt DATE,
    adv_name VARCHAR(40),
    open_to_inv VARCHAR(1),
    retail_fund VARCHAR(1),
    inst_fund VARCHAR(1),
    end_dt DATE,
    dead_flag VARCHAR(1),
    delist_cd VARCHAR(3),
    merge_fundno INTEGER
);

CREATE TABLE
crsp.fund_summary(
    summary_period2 VARCHAR(2),
    crsp_fundno INTEGER,
    caldt DATE,
    nav_latest NUMERIC(16,8),
    nav_latest_dt DATE,
    tna_latest NUMERIC(16,9),
    tna_latest_dt DATE,
    yield NUMERIC(12,10),
--    div_ytd NUMERIC(12,10),
    div_ytd REAL,
--    cap_gains_ytd NUMERIC(12,10),
    cap_gains_ytd REAL,
    nav_52w_h NUMERIC(16,8),
    nav_52w_h_dt DATE,
    nav_52w_l NUMERIC(16,8),
    nav_52w_l_dt DATE,
    unrealized_app_dep REAL,
    unrealized_app_dt DATE,
    asset_dt DATE,
    per_com NUMERIC(10,3),
    per_pref NUMERIC(10,3),
    per_conv NUMERIC(10,3),
    per_corp NUMERIC(10,3),
    per_muni NUMERIC(10,3),
    per_govt NUMERIC(10,3),
    per_oth NUMERIC(10,3),
    per_cash NUMERIC(10,3),
    per_bond NUMERIC(10,3),
    per_abs NUMERIC(10,3),
    per_mbs NUMERIC(10,3),
    per_eq_oth NUMERIC(10,3),
    per_fi_oth NUMERIC(10,3),
    maturity NUMERIC(4,1),
    maturity_dt DATE,
    cusip8 VARCHAR(10),
    crsp_portno INTEGER,
    crsp_cl_grp INTEGER,
    fund_name VARCHAR(140),
    nasdaq VARCHAR(5),
    ncusip VARCHAR(9),
    mgmt_name VARCHAR(80),
    mgmt_cd VARCHAR(4),
    mgr_name VARCHAR(40),
    mgr_dt DATE,
    adv_name VARCHAR(40),
    open_to_inv VARCHAR(1),
    retail_fund VARCHAR(1),
    inst_fund VARCHAR(1),
    m_fund VARCHAR(1),
    index_fund_flag VARCHAR(3),
    vau_fund VARCHAR(1),
    et_flag VARCHAR(1),
    delist_cd VARCHAR(3),
    first_offer_dt DATE,
    end_dt DATE,
    dead_flag VARCHAR(1),
    merge_fundno INTEGER,
    actual_12b1 NUMERIC(10,6),
    max_12b1 NUMERIC(10,6),
    exp_ratio NUMERIC(10,6),
    mgmt_fee NUMERIC(16,6)  ,
    turn_ratio REAL,
--    turn_ratio NUMERIC(10,6),
    fiscal_yearend DATE,
    crsp_obj_cd VARCHAR(4),
    si_obj_cd VARCHAR(3),
    accrual_fund VARCHAR(1),
    sales_restrict VARCHAR(1),
    wbrger_obj_cd VARCHAR(5),
    policy VARCHAR(6),
    lipper_class VARCHAR(4),
    lipper_class_name VARCHAR(42),
    lipper_obj_cd VARCHAR(3),
    lipper_obj_name VARCHAR(42),
    lipper_asset_cd VARCHAR(2),
    lipper_tax_cd VARCHAR(10)
);

CREATE TABLE
crsp.monthly_returns (
    caldt DATE,
    crsp_fundno INTEGER,
    mtna NUMERIC(16,8),
    mret NUMERIC(10,6),
    mnav NUMERIC(16,8)
);

CREATE TABLE
crsp.portfolio_holdings (
    crsp_portno INTEGER,
    report_dt DATE,
    security_rank INTEGER,
    eff_dt DATE,
    percent_tna NUMERIC(10,3),
    nbr_shares NUMERIC(18,2),
    market_val NUMERIC(16,2),
    security_name VARCHAR(200),
    cusip VARCHAR(10),
    permno INTEGER,
    permco INTEGER,
    ticker VARCHAR(6),
    coupon NUMERIC(12,3),
    maturity_dt DATE
);

CREATE TABLE
crsp.rear_load (
    crsp_fundno INTEGER,
    rear_group_no INTEGER,
    begdt DATE,
    enddt DATE,
    load_type VARCHAR(1),
    inv_lvl INTEGER,
    time_period INTEGER,
    rear_load NUMERIC(6,4)
);
