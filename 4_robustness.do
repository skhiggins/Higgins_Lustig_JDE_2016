/********************************************************************************
ROBUSTNESS CHECKS FOR FISCAL IMPOVERISHMENT PAPER
do file created by Sean Higgins, shiggins@tulane.edu
last revised Dec 22, 2015

**************
** READ ME: **
**************
1. These results were produced for the following paper;
	if using this code or these results please cite:

Higgins, Sean and Nora Lustig. 2016. "Can a Poverty-Reducing and Progressive Tax
  and Transfer System Hurt the Poor?" Journal of Development Economics 122, 63-75.
  doi:10.1016/j.jdeveco.2016.04.001
  
(If using the data set in general, please see 0_master.do for the papers to cite)

********************************************************************************/

*********
** LOG **
*********
time
local project 4_robustness
capture log close
log using "$base/logs/`project'_`time'.log", text replace
di "`c(current_date)' `c(current_time)'"
pwd

************
** LOCALS **
************
// Incomes and poverty line
local y0 ym_BC // pre-fisc income: benchmark case market income
local incomes y0 y1
scalar _z = 2.5 // poverty line of $2.50 per person per day (used in upper-middle income countries)

**************************
** PRELIMINARY PROGRAMS **
**************************
// Program for concentration coefficients and Ginis
// BEGIN covconc (Higgins 2015)
cap program drop covconc
program define covconc, rclass sortpreserve
	syntax varname [if] [in] [pw aw iw fw/], [rank(varname)]
	preserve
	marksample touse
	qui keep if `touse' // drops !`if', !`in', and any missing values of `varname'
	local 1 `varlist'
	if "`rank'"=="" {
		local rank `1'
		local _return gini
		local _returndi Gini
	}
	else {
		local _return conc
		local _returndi Concentration Coefficient
	}
	sort `rank' `1' // sort in increasing order of ranking variable; break ties with other variable
	tempvar F wnorm wsum // F is adjusted fractional rank, wnorm normalized weights,
		// wsum sum of normalized weights for obs 1, ..., i-1
	if "`exp'"!="" { // with weights
		local aw [aw=`exp'] // with = since I used / in syntax
		tempvar weight
		gen `weight' = `exp'
		qui summ `weight'
		qui gen `wnorm' = `weight'/r(sum) // weights normalized to sum to 1
		qui gen double `wsum' = sum(`wnorm')
		qui gen double `F' = `wsum'[_n-1] + `wnorm'/2 // from Lerman and Yitzhaki (1989)
		qui replace `F' = `wnorm'/2 in 1
		qui corr `1' `F' `aw', cov
		local cov = r(cov_12)
		qui summ `1' `aw', meanonly 
		local mean = r(mean)
	}
	else { // no weights
		qui gen `F' = _n/_N // sorted so this works in unweighted case; 
			// cumul `1', gen(`F') would also work
		qui corr `1' `F', cov
		local cov = r(cov_12)
		qui summ `1', meanonly
		local mean = r(mean)
	}
	local `_return' = ((r(N)-1)/r(N))*(2/`mean')*`cov' // the (N-1)/N term adjusts for
		// the fact that Stata does sample cov
	return scalar `_return' = ``_return''
	di as result "`_returndi': ``_return''"
	restore
end // END covconc

**********
** DATA **
**********
use "$base/proc/pof2.dta", clear

// PPP conversion factors (taken from WDI using -wbopendata- re-accessed Dec 2015 
//  but the numbers are pasted here to ensure replicability in case World Bank 
//  data are revised in the future)
local ppp       = 1.5713184 // Consumption-based PPP conversion factor from
	// 2005 local currency to 2005 PPP-adjusted US dollars
local cpibase   = 79.560051 // CPI in PPP base year (2005)
local cpisurvey = 95.203354 // CPI of survey year (2009; survey is 2008-2009 but 
	// prices were already deflated to 2009 prices in survey using deflation factors)

// Note: can be pulled from WDI all at once using the following wrapper I wrote for
//  Azevedo's -wbopendata-:
** ssc install ceq, replace
** ceqppp, country("bra") base(2005) survey(2009) locals

****************
** FOOTNOTE 10 *
****************
local y1 ypf_BC_adj // post-fisc income: benchmark case post-fiscal income

// PPP income variables, poverty dummies
foreach y of local incomes {
	// Income variables in PPP-adjusted dollars per day
	gen double `y'_PPP = ``y''*(1/365)*(`cpibase'/`cpisurvey')*(1/`ppp')
	// Poverty dummy
	gen byte `y'_poor = (`y'_PPP < _z)
	gen double `y'_povgap = max((_z - `y'_PPP)/_z,0)
}

// Fiscal impoverishment and fiscal gains of the poor
gen byte is_fi = (y1_PPP < y0_PPP) & (y1_PPP < _z)
gen byte is_fgp = (y1_PPP > y0_PPP) & (y0_PPP < _z)
gen double fi = min(y0_PPP,_z) - min(y0_PPP,y1_PPP,_z) // summand of axiomatic measure in (1)
gen double fgp = min(y1_PPP,_z) - min(y0_PPP,y1_PPP,_z) // summand of axiomatic measure in (2)

quietly {
	summ is_fi [aw=w] if i==1 & y1_poor==1
	noisily di "Percent of post-fisc poor fiscally impoverished when " _n ///
		"taxes scaled down to equal transfers included in analysis"
	noisily di %3.1f r(mean)*100
}

*****************
** FOOTNOTE 16 **
*****************
local y1 ypf_BC
summ pc_total_taxes if i==1 [aw=w]
local total_taxes = r(sum)
summ pc_total_transfers if i==1 [aw=w]
local total_transfers = r(sum)

keep if i==1 // So I don't have to include the if i==1 condition over and over below

// optimal taxes
tempvar y_minustax tax
gen `y_minustax' = `y0'
gen `tax' = 0
summ `tax'
local total_sim_tax = r(sum)
local min = 0

// start by getting close to total taxes
di "`total_taxes'"
local line = 33*`ppp'*(`cpisurvey'/`cpibase')*365
replace `y_minustax' = `line' if `y_minustax'>`line'
replace `tax' = `y0' - `y_minustax'
summ `tax' [aw=w]
di %12.0f r(sum)

local max = `line'

// now that we are reducing all above $33/day to $33/day, keep going by continuing the 
//  optimal (lexicographic) taxation:
gsort -`y0'

while `total_sim_tax' < `total_taxes' {
	qui summ `y0' if `y0'<`max'-1
	local newmax = r(max)
	
	qui replace `tax' = `tax' + (`max' - `newmax') if `y_minustax'>=`max'-1
	qui replace `y_minustax' = `y0' - `tax'
	
	qui summ `tax' [aw=w]
	local total_sim_tax = r(sum)
	
	local max = `newmax'
}
gen pc_optimal_taxes = `tax'

// optimal transfers of Bolsa Familia
tempvar y_plustransfer transfer
gen `y_plustransfer' = `y0'
gen `transfer' = 0
summ `transfer' if i==1
local total_sim_transfer = r(sum)
local min = 0

di "`total_transfers'"

// start by getting close to here: (tried different lines in $/day until got close to size
//  of Bolsa Familia)
local line = 1.9*`ppp'*(`cpisurvey'/`cpibase')*365 // in local currency
replace `y_plustransfer' = `line' if `y_plustransfer'<`line'
replace `transfer' = `y_plustransfer' - `y0'
summ `transfer' [aw=w]
di %12.0f r(sum)

summ pc_BolsaFamilia [aw=w]
local total_BF = r(sum)

local min = `line'

sort `y0'
// now that we are close from lifting everyone below $1.90 to $1.90 above, keep going with the
//  optimal (lexicographic) redistribution:
while `total_sim_transfer'<`total_BF' {

	qui summ `y0' if `y0'>`min'+1, meanonly
	local newmin = r(min)

	qui replace `transfer' = `transfer' + (`newmin' - `min') if `y_plustransfer'<=`min'+1
	qui replace `y_plustransfer' = `y0' + `transfer' 
	
	qui summ `transfer' [aw=w]
	local total_sim_transfer = r(sum)
	
	local min = `newmin'
}
gen pc_optimal_BF = `transfer'
gen y1_optimal = `y0' + (pc_total_transfers - pc_BolsaFamilia) + pc_optimal_BF - pc_optimal_taxes

quietly {
	summ y1_optimal, meanonly // no weights bc just need min
	noisily di "Minimum income after optimal redistribution:"
	noisily di %3.2f r(min)/(`ppp'*(`cpisurvey'/`cpibase')*365)
}

local y1_optimal y1_optimal
local incomes y0 y1 y1_optimal
foreach y of local incomes {
	cap drop `y'_PPP `y'_poor `y'_povgap
	
	// Income variables in PPP-adjusted dollars per day
	gen double `y'_PPP = ``y''*(1/365)*(`cpibase'/`cpisurvey')*(1/`ppp')
	// Poverty dummy
	gen byte `y'_poor = (`y'_PPP < _z)
	gen double `y'_povgap = max((_z - `y'_PPP)/_z,0)
	
}
quietly {
	summ y1_optimal_povgap [aw=w], meanonly
	noisily di "Poverty gap under optimal redistribution:"
	noisily di %5.4f r(mean)
	summ y1_povgap [aw=w], meanonly
	noisily di "Poverty gap under actual redistribution:"
	noisily di %5.4f r(mean)
	covconc y1_optimal [pw=w]
	noisily di "Gini under optimal redistribution:"
	noisily di %5.4f r(gini)
	covconc `y1' [pw=w]
	noisily di "Gini under actual redistribution:"
	noisily di %5.4f r(gini)
}

*************
** WRAP UP **
*************
log close
exit
