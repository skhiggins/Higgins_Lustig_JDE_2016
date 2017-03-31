/********************************************************************************
STYLISTIC ILLUSTRATION FOR FISCAL IMPOVERISHMENT PAPER
do file created by Sean Higgins, shiggins@tulane.edu
last revised Apr 26, 2016

**************
** READ ME: **
**************
1. This figure was produced for the following paper;
	if using please cite:

Higgins, Sean and Nora Lustig. 2016. "Can a Poverty-Reducing and Progressive Tax
  and Transfer System Hurt the Poor?" Journal of Development Economics 122, 63-75.
  doi:10.1016/j.jdeveco.2016.04.001
  
********************************************************************************/

*********
** LOG **
*********
time
local project 5_stylistic_illustration
capture log close
log using "$base/logs/`project'_`time'.log", text replace
di "`c(current_date)' `c(current_time)'"
pwd

************
** LOCALS **
************
// For graph 
local thickness thick
local c1 lcolor(dknavy)
local c2 lcolor(orange)
local c3 lcolor(gray) lpattern(dash)
local legendsize large
local xsize large // large for multipanel, ow medium
local numsize large // normally use medlarge; large if multipanel
local topts margin(top) size(`xsize') color(black)
local xaxis xlabel(, notick nolabel) ///
	xtitle("Population ordered by pre-fisc income", `topts') 
local yaxis ylabel(, notick labcolor(white) labsize(`numsize')) ///
	title("Income", pos(11) span size(`xsize') color(black))
local yaxis2 ylabel(0(.2)1, `yopts') // for Lorenz curves
local lopts ring(0) pos(11) col(1) size(`legendsize') symx(*0.625) keygap(*0.48) rowgap(*.75) ///
	region(margin(zero) lcolor(white))
local aspectratio  aspect(0.6)
local o sort lwidth(`thickness')
local plotregion plotregion(margin(l=0 b=0 t+0.3 r=0) fcolor(white) lstyle(none) lcolor(white)) 
local graphregion graphregion(margin(zero) fcolor(white) lstyle(none) lcolor(white)) 

**********
** DATA **
**********
clear

// Create variables for curves and areas
set obs 401 // to go from 0 to 4
gen x = _n/100 - 1/100 // so x = 0(0.01)4
gen prefisc  = 6/(1+exp(2.5-x)) - 0.4 // the increasing curve
gen postfisc = cos(3.5*x) + x + 0.3   // the wavy curve
gen z = 2.5
gen fg_dummy = (prefisc < postfisc) & (prefisc < z)
gen fi_dummy = (postfisc < prefisc) & (postfisc < z)
gen fg_hi = min(postfisc,z) if fg_dummy==1
gen fg_lo = prefisc  if fg_dummy==1
gen fi_hi = min(prefisc,z) if fi_dummy==1 
gen fi_lo = postfisc if fi_dummy==1

**************
** FIGURE 1 **
**************
// Variable labels
label var prefisc "Pre-fisc"
label var postfisc "Post-fisc"
label var z "Poverty line"
label var fg_hi "Fiscal gains of the poor"
label var fi_hi "Fiscal impoverishment"

#delimit ;
graph twoway
	(area fg_hi x, color(gs10) lwidth(none) cmissing(n))
	(area fi_hi x, color(gs3)  lwidth(none) cmissing(n))
	(area fg_lo x, color(white) lcolor(white) cmissing(n))
	(area fi_lo x, color(white) lcolor(white) cmissing(n))
	(line z        x, `c3' `o')
	(line prefisc  x, `c2' `o')
	(line postfisc x, `c1' `o')
	, 
	`yaxis' 
	`xaxis'
	legend(order(6 7 5 2 1) `lopts')
	`aspectratio'
	`plotregion' 
	`graphregion'
	saving("$base/graphs/stylistic.gph", replace)
;
#delimit cr

graph export "$base/graphs/stylistic.eps", replace
graph export "$base/graphs/stylistic.png", replace

*************
** WRAP UP **
*************
log close
exit

