/********************************************************************************
ANALYSIS FOR FISCAL IMPOVERISHMENT PAPER
do file created by Sean Higgins, shiggins@tulane.edu
last revised Apr 26, 2016

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
local project 3_fiscal_impoverishment
capture log close
log using "$base/logs/`project'_`time'.log", text replace
di "`c(current_date)' `c(current_time)'"
pwd

************
** LOCALS **
************
// Incomes and poverty line
local y0 ym_BC // pre-fisc income: benchmark case market income
local y1 ypf_BC // post-fisc income: benchmark case post-fiscal income
local incomes y0 y1
scalar _zlow = 1.25 // low poverty line of $1.25 per person per day (used in low and lower-middle income countries)
scalar _z = 2.5 // poverty line of $2.50 per person per day (used in upper-middle income countries)
scalar _zmax = 4 // highest poverty line for graphs $4 per person per day
local zlow = _zlow
local z = _z
local zmax = _zmax
local precision = 0.01 // increment to poverty line for graphs of FI, FGP, poverty gap

// For graph 
local l_y0 "Pre-fisc"
local l_y1 "Post-fisc"
local thickness thick
local thinner medthick
local thinnest medthin
local end = _zmax
local xcutoff `end'
local inc 1
local lowrestrict // fifgp1>=.2
local low 0
local c1 lcolor(dknavy)
local c2 lcolor(orange)
local c3 lcolor(ltblue) lpattern(dash)
local hx 6
local titlesize large // normally use medium; medlarge if multipanel
local legendsize large
local xsize large // large for multipanel, ow medium
local numsize large // normally use medlarge; large if multipanel
local xopts labsize(`numsize') labcolor(black) notick nogrid
local topts margin(top) size(`xsize') color(black)
local maintopts size(`xsize') color(black)
local subtopts margin(bottom) size(`xsize') color(black)
local xaxis xline(`zlow' `z', lcolor(gs7) lpattern(shortdash) lwidth(`thinner')) ///
	xlabel(none 1 2 3 4, `xopts') ///
	xtitle("Income in dollars per person per day", `topts')
	// for CDFs
local xaxis2 ///
	xlabel(none .2 .4 .6 .8 1, `xopts') ///
	xtitle("Cumulative proportion of the population", `topts')
	// for Lorenz curves
local yopts angle(0) labsize(`numsize') labcolor(black) notick nogrid
local yaxis ylabel(, `yopts') // for CDFs, FI/FGP, poverty gaps
local yaxis2 ylabel(0(.2)1, `yopts') // for Lorenz curves
local lopts ring(0) pos(11) col(1) size(`legendsize') symx(*0.53) keygap(*0.48) rowgap(*.75) ///
	region(margin(zero) lcolor(white))
local legend legend(`lopts') 
local commony fysize(75)
local aspectratio  `commony' fxsize(100) // aspect(0.7) 
local aspectratio2 `commony' fxsize(80) // aspect(0.7)
local if if `lowrestrict' z<=`end'
local o sort lwidth(`thickness')
local plotregion plotregion(margin(zero) fcolor(white) lstyle(none) lcolor(white)) 
local graphregion graphregion(fcolor(white) lstyle(none) lcolor(white)) 

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

// BEGIN _fifg_total (Higgins 2015)
//  Calculates total fiscal impoverishemnt and fiscal gains of the poor
//   measures for a specific poverty line and two income concepts
capture program drop _fifg_total
program define _fifg_total
	#delimit ;
	syntax varlist(min=2 max=2) [if] [in] [aweight], 
		z(real) row(real) matrix(string)
	;
	#delimit cr
	
	confirm matrix `matrix'
	
	local y0 = word("`varlist'",1)
	local y1 = word("`varlist'",2)
	
	tempvar d_fi d_fg p0 p1
	qui gen `d_fi' = min(`y0',`z') - min(`y0',`y1',`z') // individual's FI
	qui gen `d_fg' = min(`y1',`z') - min(`y0',`y1',`z') // individual's FGP
	qui gen `p0' = `z' - min(`y0',`z') // individual's pre-fisc poverty gap
	qui gen `p1' = `z' - min(`y1',`z') // individual's post-fisc poverty gap
	
	matrix `matrix'[`row',1] = `z'
	
	// FI and FGP
	qui summ `d_fi' `if' `in' [`weight' `exp'], meanonly
	matrix `matrix'[`row',2] = r(sum) // FI
	qui summ `d_fg' `if' `in' [`weight' `exp'], meanonly
	matrix `matrix'[`row',3] = r(sum) // FGP
	matrix `matrix'[`row',4] = `matrix'[`row',2] - `matrix'[`row',3] // difference
	
	// Poverty gaps
	qui summ `p0' `if' `in' [`weight' `exp'], meanonly
	matrix `matrix'[`row',5] = r(sum) // pre-fisc poverty gap
	qui summ `p1' `if' `in' [`weight' `exp'], meanonly
	matrix `matrix'[`row',6] = r(sum) // post-fisc poverty gap
	matrix `matrix'[`row',7] = `matrix'[`row',6] - `matrix'[`row',5] // difference
	
end

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

// NOTE: can be pulled from WDI all at once using the following wrapper I wrote for
//  Azevedo's -wbopendata-:
** ssc install ceq, replace
** ceqppp, country("bra") base(2005) survey(2009) locals

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

**********************
** TABLE 2 (BRAZIL) **
**********************
// For Brazil results in Table 2 of Higgins and Lustig, 
//  "Can a Poverty-Reducing and Progressive Tax and Transfer System Hurt the Poor?"
quietly {
	// Create a blank matrix for results
	matrix table2 = J(1,7,.)

	// Pre-fisc poverty headcount (column 1)
	summ y0_poor [aw=w] if i==1, meanonly
	matrix table2[1,1] = r(mean)*100

	// Change in poverty headcount
	summ y1_poor [aw=w] if i==1, meanonly
	matrix table2[1,2] = r(mean)*100 - table2[1,1]

	// Pre-fisc inequality (Gini)
	covconc `y0' [pw=w] if i==1 // default of covconc is to rank by same variable specified in varlist
		// so this gives Gini
	matrix table2[1,3] = r(gini)*100

	// Reynolds-Smolensky (Post-fisc w.r.t. Pre-fisc)
	covconc `y1' [pw=w] if i==1, rank(`y0') // concentration coefficient of post-fisc w.r.t. pre-fisc
	matrix table2[1,4] = table2[1,3] - r(conc)*100

	// Change in inequality (Gini)
	covconc `y1' [pw=w] if i==1 // default of covconc is to rank by same variable specified in varlist
		// so this gives Gini
	matrix table2[1,5] = r(gini)*100 - table2[1,3]

	// Fiscally impoverished as % of population
	summ is_fi [aw=w] if i==1, meanonly
	matrix table2[1,6] = r(mean)*100

	// Fiscally impoverished as % of post-fisc poor
	summ is_fi [aw=w] if i==1 & y1_poor==1
	matrix table2[1,7] = r(mean)*100

	// Display Brazil results for Table 2
	noisily mydi "TABLE 2 (BRAZIL)", s(4)
	noisily matlist table2, format(%3.1f)
}

**********************
** TABLE 3 (BRAZIL) **
**********************
// For Brazil results in Table 3 of Higgins and Lustig, 
//  "Can a Poverty-Reducing and Progressive Tax and Transfer System Hurt the Poor?"
quietly {
	// Create a blank matrix for results
	matrix table3 = J(1,6,.)
	
	// Total FI ($ millions per year) // column 1
	summ fi [aw=w] if i==1
	di %16.0f r(sum)*365 // total in $/year
	matrix table3[1,1] = r(sum)*365/1000000 // $ millions per year
	
	// Per cpaita FI as % of z // column 4
	matrix table3[1,4] = (r(mean)/_z)*100
	
	// Total FGP ($ millions per year) // column 2
	summ fgp [aw=w] if i==1
	di %16.0f r(sum)*365
	matrix table3[1,2] = r(sum)*365/1000000
	
	// Per cpaita FGP as % of z // column 5
	matrix table3[1,5] = (r(mean)/_z)*100
	
	// FI as % of GDP // column 3
	matrix table3[1,3] = (table3[1,1]/table3[1,2])*100
	
	// Change in Poverty Gap Ratio // column 6
	//  (equivalent to difference between columns 4 and 5 by Proposition 4 in paper)
	forval i=0/1 {
		quietly summ y`i'_povgap [aw=w] if i==1
		scalar y`i'gap = r(mean)*100
	}
	matrix table3[1,6] = y1gap - y0gap
	
	noisily mydi "TABLE 3 (BRAZIL)", s(4)
	noisily matlist table3, format(%6.2f)
}

**********************************
** RESULT FOR TEXT IN SECTION 5 **
**********************************
// Percent of impoverished receiving Bolsa Familia
quietly {
	summ BF_imp if is_fi & i==1, meanonly
	noisily mydi "Percent of impoverished who receive Bolsa Familia", s(4)
	noisily display as result r(mean)
}

**************
** FIGURE 2 **
**************
// FIGURE 2a
quietly {
	// CUMULATIVE DISTRIBUTION FUNCTIONS 
	foreach y of local incomes {
		cumul `y'_PPP if i==1 [aw=w], gen(CDF_`y')
		label var CDF_`y' "`l_`y''"
	}

	set scheme s1color
	#delimit ;
	twoway
		(line CDF_y0 y0_PPP if i==1 & y0_PPP<=_zmax [aw=w], `o' `c2')
		(line CDF_y1 y1_PPP if i==1 & y1_PPP<=_zmax [aw=w], `o' `c1')
		,
		`xaxis' `yaxis' `legend' `aspectratio'
		`graphregion' `plotregion'
		title("(a) First order stochastic dominance", `maintops')
		subtitle("(Cumulative distribution functions)", `subtopts')
		name("cdfs", replace)
		nodraw
	;
	#delimit cr
}

// FIGURE 2b
quietly {
	// Lorenz
	foreach y of local incomes {
		glcurve ``y'' if i==1 [aw=w], pvar(`y'Lx) glvar(`y'Ly) sortvar(``y'') nograph lorenz
	}
	// Post-fisc CC wrt pre-fisc
	glcurve `y1' if i==1 [aw=w], pvar(y1Cx) glvar(y1Cy) sortvar(`y0') nograph lorenz

	* for 45 degree line:
	gen diagline = y0Ly
	label var diagline "45 Degree Line"

	* for legends:
	label var y0Ly "Pre-Fisc Lorenz"
	label var y1Ly "Post-Fisc Lorenz"
	label var y1Cy "Post-Fisc Concentration (Pre-Fisc Ranking)"

	#delimit ;
	twoway 
		(line diagline diagline if i==1 [aw=w], sort lwidth(`thinnest') lcolor(black))
		(line y0Ly y0Lx if i==1 [aw=w], `o' `c2')
		(line y1Cy y1Cx if i==1 [aw=w], `o' `c1')
		(line y1Ly y1Lx if i==1 [aw=w], `o' `c3')
		,
		`xaxis2' `yaxis2' `aspectratio2'
		`graphregion' `plotregion'
		legend(`lopts' order(
				2 "Pre-fisc Lorenz" 
				3 "Post-fisc concentration"
				4 "Post-fisc Lorenz" 
			)
		)
		title("(b) Global progressivity", `maintops')
		subtitle("(Lorenz and concentration curves)", `subtopts')
		name("lorenz", replace)
		nodraw
	;
	#delimit cr
}

// COMBINED FIGURE 2
graph combine cdfs lorenz, ///
	imargin(sides) ysize(6) xsize(10) graphregion(margin(0 0 0 0)) ///
	saving("$base/graphs/cdfs_lorenz.gph", replace)
graph export "$base/graphs/cdfs_lorenz.eps", replace
graph export "$base/graphs/cdfs_lorenz.png", replace

// NOTE to remove the white space at the top and bottom of the .eps file,
//  which I couldn't figure out how to do in Stata, I manually opened the .eps
//  file in a text editor and changed the following lines:
**   %%BoundingBox: 0 0 720 432
**   %%HiResBoundingBox: 0.000 0.000 720.000 432.000
//  to the following:
**   %%BoundingBox: 0 54 720 385
**   %%HiResBoundingBox: 0.000 54.000 720.000 385.000
//  (see http://goo.gl/V8ibX6 for more info)

**************
** FIGURE 3 **
**************
// Create blank matrix for results that will be graphed
local rows = (`zmax'/`precision') + 1
if c(matsize) < `rows' set matsize `rows'
matrix figure3_ = J(`rows',7,.)

// Calculate measures that will be graphed at each poverty line
local rcount = 0
forval i=0(`precision')`zmax' {
	local ++rcount
	_fifg_total y0_PPP y1_PPP if i==1 [aw=w], row(`rcount') z(`i') matrix(figure3_)
}

// Graph
preserve
clear
svmat figure3_
rename figure3_1 z
label var z "Poverty line"
rename figure3_2 fi_total
label var fi_total "FI"
rename figure3_3 fg_total
label var fg_total "FGP"
rename figure3_4 fifg_diff
label var fifg_diff "Difference" 
rename figure3_5 p0_total
label var p0_total "Pre-fisc" // Note these labels will be in graph legend,
	// which is why I am not more specific here saying "Pre-Fisc Total Poverty Gap"
rename figure3_6 p1_total
label var p1_total "Post-fisc"
rename figure3_7 p0p1_diff
label var p0p1_diff "Difference"
assert (fifg_diff < p0p1_diff + 1) & (fifg_diff > p0p1_diff - 1) // Proposition 4
	// (the above asserts they are equal but allows for slight rounding error)

// From dollars per day to billions of dollars per year;
//  differences to absolute value
ds z, not // all variables except z are now in `r(varlist)'
foreach var of varlist `r(varlist)' {
	replace `var' = `var'*365/1000000000
	if strpos("`var'","diff") replace `var' = abs(`var')
}

// FIGURE 3a
#delimit ;
graph twoway 
	(line fg_total z, `o' `c2')
	(line fi_total z, `o' `c1')
	(line fifg_diff z, `o' `c3')
	,
	`xaxis' `yaxis' `legend' `aspectratio'
	`graphregion' `plotregion'	
	title("(a) Total FI and FGP", `maintops')
	subtitle("(Billions of dollars per year)", `subtopts')
	name("total_fi_fg", replace)
	nodraw
;
#delimit cr

// FIGURE 3b
#delimit ;
graph twoway
	(line p0_total z, `o' `c2')
	(line p1_total z, `o' `c1')
	(line p0p1_diff z, `o' `c3')
	,
	`xaxis'
	ylabel(0(10)35, `yopts') 
	`legend' `aspectratio'
	`graphregion' `plotregion'	
	title("(b) Total poverty gaps", `maintops')
	subtitle("(Billions of dollars per year)", `subtopts')
	name("total_povgaps", replace)
	nodraw
;
#delimit cr

// COMBINED FIGURE 3
graph combine total_fi_fg total_povgaps, ///
	imargin(sides) ysize(6) xsize(11) graphregion(margin(0 0 0 0)) ///
	saving("$base/graphs/total_fi_fg_povgaps.gph", replace)
graph export "$base/graphs/total_fi_fg_povgaps.eps", replace
graph export "$base/graphs/total_fi_fg_povgaps.png", replace
	
// NOTE to remove the white space at the top and bottom of the .eps file,
//  which I couldn't figure out how to do in Stata, I manually opened the .eps
//  file in a text editor and changed the following lines:
**   %%BoundingBox: 0 0 792 432
**   %%HiResBoundingBox: 0.000 0.000 792.000 432.000
//  to the following:
**   %%BoundingBox: 0 54 792 385
**   %%HiResBoundingBox: 0.000 54.000 792.000 385.000
//  (see http://goo.gl/V8ibX6 for more info)

*****************************
** ADDITIONAL CALCULATIONS **
*****************************
// Maximum poverty reduction for text in Section 5
putmata r=(z fi_total fg_total fifg_diff p0_total p1_total p0p1_diff), replace
mata: select(r, r[,7] :== colmax(r[,7]))
	// column 1 is the poverty line with max poverty reduction
	// column 2 shows total FI at this line (billions of $ per year)
	// column 3 shows total FGP at this line (billions of $ per year)
	// column 4 shows difference between FI and FGP at this line (billions of $ per year)
	// column 5 shows total pre-fisc poverty gap at this line (billions of $ per year)
	// column 6 shows total post-fisc poverty gap at this line (billions of $ per year)
	// column 7 shows reduction in poverty gap (billions of $ per year)
	
// Values of FI, FGP, poverty gap at 1.25 and 2.50 lines
display "1.25 poverty line"
mata: select(r, r[,1] :== 1.25)
display "2.50 poverty line"
mata: select(r, r[,1] :== 2.50)
	// column 1 is the poverty line
	// column 2 shows total FI at this line (billions of $ per year)
	// column 3 shows total FGP at this line (billions of $ per year)
	// column 4 shows difference between FI and FGP at this line (billions of $ per year)
	// column 5 shows total pre-fisc poverty gap at this line (billions of $ per year)
	// column 6 shows total post-fisc poverty gap at this line (billions of $ per year)
	// column 7 shows reduction in poverty gap (billions of $ per year)

restore

*************
** WRAP UP **
*************
log close
exit

