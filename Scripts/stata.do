import excel "G:\Research_Analyst\Reinbold\Ravi\Welfare Cost of Inflation\Data\Lucas Data.xlsx", sheet("Data") firstrow clear

rename NominalGDPmillions gdp
rename GDPDeflator1987100 gdpDef
rename M1billions M1
drop F

keep if year >= 1980

gen m1_gdp = M1 / (gdp / 1000)
gen log_m1_gdp = log(m1_gdp)

reg log_m1_gdp Rate

scatter m1_gdp Rate

scatter log_m1_gdp Rate



import excel "G:\Research_Analyst\Reinbold\Ravi\Welfare Cost of Inflation\Data\Quarterly Data.xlsx", sheet("Data") firstrow cellrange(A3:F162) allstring clear
drop DESC
rename B date
rename MonthTreasuryBillSecondary Rate
rename GrossDomesticProductSAARBi gdp
rename MoneyStockM1SABil M1
rename GrossDomesticProductImplicit gdpDef

destring Rate gdp M1 gdpDef, replace


gen m1_gdp = M1 / gdp
gen log_m1_gdp = log(m1_gdp)
replace Rate = Rate / 100

reg log_m1_gdp Rate

scatter m1_gdp Rate

scatter log_m1_gdp Rate

gen stata_date = qofd(date(date, "DMY"))     
format stata_date %tq

tsset stata_date

tsline m1_gdp
tsline Rate

// Run augmented Dickey-Fuller tests
dfuller Rate, lags(4)
dfuller m1_gdp, lags(4)


tsfilter bk rate_bpCycle = Rate, minperiod(4) maxperiod(6) trend(rate_bpTrend) stationary
tsfilter bk m1_gdp_bpCycle = m1_gdp, minperiod(4) maxperiod(6) trend(m1_gdp_bpTrend)

tsline rate_bpCycle
tsline m1_gdp_bpCycle
tsline rate_bpTrend
tsline m1_gdp_bpTrend

scatter m1_gdp_bpTrend rate_bpTrend
